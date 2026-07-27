#!/usr/bin/env python3
"""Shared C / Java behavioral-gate machinery (the native-language analog of _gatelib.py).

GnuCOBOL's gate compiles+runs COBOL; this does the same for the C and Java estates.
A scenario's verify/run_gates.py is a thin config: it declares the language, engine source files, the golden-delta set, the fixture
inputs to stage, and the tempdir prefix, then calls selftest_main.

Layout assumed (verify/ = gate.here):
    <release>/codebase/<lang>/**/*.{c,java}        merged start tree
    <release>/codebase/c/include/*.h               C headers (C only)
    here/reference/<file>                          golden delta sources
    here/fixture/<inputs>                          fixed test inputs (staged into run dir)
    here/grader.py                                 Grader (same shape as the COBOL graders)

Gate axes asserted by selftest: GOLDEN passes all criteria (solvable), START fails >=1
(not pre-solved). Both tool-agnostic — no retrieval tool, no agent.
"""
from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass, field
from pathlib import Path


# --- released-artifact layout ----------------------------------------------
# In the public release the per-scenario `out/` trees do not exist: all 34
# scenarios are merged into ONE flat codebase at the release root, and every
# gate sources its START side from there. Override with $TRUSTBENCH_CODEBASE.
CODEBASE = Path(
    os.environ.get("TRUSTBENCH_CODEBASE")
    or Path(__file__).resolve().parents[1] / "codebase"
)


def _dec(b):
    return (b or b"").decode("utf-8", "replace")


def _run(cmd, cwd):
    p = subprocess.run(cmd, cwd=str(cwd), capture_output=True)
    p.out = _dec(p.stdout)
    p.err = _dec(p.stderr)
    return p


@dataclass
class NativeGate:
    here: Path             # the scenario's verify/ dir
    language: str          # "c" | "java"
    engines: list          # source file basenames (build all together)
    delta: set             # basenames that ship a golden reference
    inputs: list           # fixture files staged into the run dir
    prefix: str            # tempdir prefix
    main_class: str = ""   # Java entry class (java only)
    cc_extra: tuple = ()    # extra compiler flags
    grader_module: str = "grader"   # grader.py module to load (per-gate; default "grader")

    @property
    def src(self) -> Path:
        # engines are found by rglob, so the language ROOT is enough (the merged
        # Java tree nests package dirs; C/PL-I/RPG keep a src/ subdir).
        return CODEBASE / self.language

    @property
    def inc(self) -> Path:
        return CODEBASE / "c" / "include"

    @property
    def model(self) -> Path:
        # The pinned record/model classes this engine compiles against. The merged
        # Java tree interleaves every company's model classes across package dirs,
        # so this gate's set is kept with the rig — copied verbatim out of the
        # merged codebase at release time, so it is the same source the agent sees.
        return self.here / "model"

    @property
    def ref(self) -> Path:
        return self.here / "reference"

    @property
    def fixture(self) -> Path:
        return self.here / "fixture"


#: an undefined-symbol name in a clang/ld link error (macOS ld64: `"_sym", referenced
#: from`; GNU ld: ``undefined reference to `sym'``). The leading `_` is the Mach-O
#: convention; strip it to get the C identifier.
_UNDEF_RE = re.compile(
    r'"_?([A-Za-z_]\w*)",\s*referenced from'          # ld64
    r"|undefined reference to [`']_?([A-Za-z_]\w*)'"    # GNU ld
)


def _c_defines_symbol(text: str, sym: str) -> bool:
    """Heuristic: `text` DEFINES `sym` (a `<ret> sym(<params>) {` with a body brace,
    params allowed to span lines) rather than merely calling it (`sym(...);`) or
    declaring it (`sym(...);`)."""
    return bool(re.search(rf"\b{re.escape(sym)}\s*\([^;{{}}]*\)\s*\{{", text))


def _resolve_c_deps(d: Path, srcs: list[str], c_srcdir: Path | None,
                    stderr: str) -> list[str]:
    """Given a link error `stderr`, find sibling `.c` files in the arm's source dir
    that DEFINE the undefined symbols and are NOT standalone programs (no `main()`
    of their own — linking those would collide on `main`), copy them into `d`, and
    return the new src paths to add. This lets a valid fix that refactors logic into
    a helper `.c` the gate doesn't stage still link — the C analog of the Java
    -sourcepath fix. Returns [] when nothing new is found (caller stops retrying)."""
    if not c_srcdir or not c_srcdir.exists():
        return []
    syms = {m.group(1) or m.group(2) for m in _UNDEF_RE.finditer(stderr)}
    if not syms:
        return []
    have = {Path(s).name for s in srcs}
    added: list[str] = []
    for sym in syms:
        for c in sorted(c_srcdir.glob("*.c")):
            if c.name in have:
                continue
            txt = c.read_text(errors="replace")
            if re.search(r"\bint\s+main\s*\(", txt):
                continue  # standalone program → its main() would clash with the engine's
            if _c_defines_symbol(txt, sym):
                shutil.copy(c, d / c.name)
                added.append(str(d / c.name))
                have.add(c.name)
                break
    return added


def _compile_run(gate: NativeGate, d: Path, java_sourcepath: Path | None = None,
                 c_srcdir: Path | None = None):
    """Compile every staged engine source together + run; return (stdout, exit, dir, log).

    `java_sourcepath` (harness path only): the arm's java source ROOT, handed to
    javac via -sourcepath so a fix that refactors logic into a SAME-PACKAGE sibling
    the gate doesn't stage (e.g. the engine now `new FeeScheduleLoader()`) still
    compiles — javac auto-resolves+compiles only the sources it actually references
    from there. Without it, a valid multi-file fix fails `cannot find symbol` (exit
    99) purely because the sibling wasn't staged — the Java analog of the C
    header-clobber bug. The staged (agent/model) files on the command line take
    precedence over sourcepath, so nothing is double-compiled.

    `c_srcdir` (harness path only): the arm's C source dir, used to RESOLVE link-time
    undefined symbols from sibling no-`main` `.c` helpers a fix refactored logic into
    (clang has no -sourcepath, so we resolve iteratively from the link error). The C
    analog of the same fix."""
    log: dict = {}
    if gate.language == "c":
        if gate.inc.exists():
            for h in gate.inc.glob("*.h"):
                # Pristine headers only FILL missing ones — never clobber a
                # header the agent edited (build_and_run_dir stages the agent's
                # headers into `d` first). Overwriting here silently discarded
                # agent header edits, so a correct fix that defines a constant in
                # a header (a clean way to de-dupe) failed to compile (exit 99),
                # unfairly penalizing arms that refactor via headers.
                if not (d / h.name).exists():
                    shutil.copy(h, d / h.name)
        srcs = [str(d / e) for e in gate.engines]
        # Link, resolving undefined symbols from sibling no-main .c helpers the fix
        # may have refactored into (bounded retry; stops when nothing new resolves).
        for _ in range(12):
            cc = _run(["clang", "-std=c11", "-O0", *gate.cc_extra, "-I", str(d),
                       "-o", "engine", *srcs], d)
            log["cc"] = cc
            if cc.returncode == 0:
                break
            extra = _resolve_c_deps(d, srcs, c_srcdir, cc.err)
            if not extra:
                break
            srcs += extra
        if cc.returncode != 0:
            return f"COMPILE FAIL [clang]:\n{cc.err}", 99, d, log
        run = _run(["./engine"], d)
    elif gate.language == "java":
        # Stage the pinned Java model (RiskModel.java + generated record layouts) alongside
        # the engine — the analog of staging C headers — so the engine compiles against the
        # curated contract. Default-package, so a flat copy compiles together.
        model_srcs = []
        if gate.model.exists():
            for j in gate.model.rglob("*.java"):   # rglob: model may live under a package dir
                dst = d / j.name
                # Don't clobber an agent-edited model file staged by
                # build_and_run_dir (same rule as C headers): pristine model
                # only FILLS files the agent didn't touch, so a fix that edits a
                # shared model class is honored instead of silently discarded.
                if not dst.exists():
                    shutil.copy(j, dst)
                model_srcs.append(str(dst))
        sp = (["-sourcepath", str(java_sourcepath)] if java_sourcepath
              and java_sourcepath.exists() else [])
        cc = _run(["javac", "-d", str(d), *sp, *gate.cc_extra,
                   *model_srcs, *[str(d / e) for e in gate.engines]], d)
        log["cc"] = cc
        if cc.returncode != 0:
            return f"COMPILE FAIL [javac]:\n{cc.err}", 99, d, log
        run = _run(["java", "-cp", str(d), gate.main_class], d)
    else:
        return f"unsupported language {gate.language!r}", 97, d, log
    log["run"] = run
    return run.out, run.returncode, d, log


def _stage_sources(gate: NativeGate, d: Path, side: str) -> str | None:
    """Copy engines (golden deltas from reference on the golden side) + fixture inputs."""
    for e in gate.engines:
        root = gate.ref if (side == "golden" and e in gate.delta) else gate.src
        hits = list(root.rglob(e))          # rglob: engine may live under a package dir
        if not hits:
            return f"MISSING {e} (under {root})"
        shutil.copy(hits[0], d / e)
    for inp in gate.inputs:
        f = gate.fixture / inp
        if not f.exists():
            return f"MISSING fixture {inp} ({f})"
        shutil.copy(f, d / inp)
    return None


def build_and_run(gate: NativeGate, side: str):
    d = Path(tempfile.mkdtemp(prefix=f"{gate.prefix}_{side}_"))
    err = _stage_sources(gate, d, side)
    if err:
        return err, 98, d, {}
    return _compile_run(gate, d)


def build_and_run_dir(gate: NativeGate, code_root):
    """Harness path: engines sourced from an arbitrary edited tree (found by name)."""
    code_root = Path(code_root)
    d = Path(tempfile.mkdtemp(prefix=f"{gate.prefix}_agent_"))
    java_sourcepath: Path | None = None
    for e in gate.engines:
        hits = list(code_root.rglob(e))
        if not hits:
            return f"MISSING {e} under {code_root}", 98, d, {}
        shutil.copy(hits[0], d / e)
        # Derive the arm's java source ROOT from the FIRST engine's location by
        # stripping its package path (from main_class), so javac -sourcepath can
        # resolve same-package siblings the agent's fix references but the gate
        # doesn't stage (see _compile_run). e.g. hit …/java/jp/mirai/pay/fee/
        # MdrFeeEngine.java, package jp.mirai.pay.fee → root …/java.
        if gate.language == "java" and java_sourcepath is None and gate.main_class:
            pkg_depth = len(gate.main_class.split(".")) - 1  # class name excluded
            # MUST be absolute: javac runs with cwd=<tempdir>, so a relative
            # -sourcepath (from a relative code_root) would resolve against the
            # tempdir and silently find nothing (→ cannot-find-symbol, exit 99).
            parents = hits[0].resolve().parents
            if pkg_depth < len(parents):
                java_sourcepath = parents[pkg_depth]
    c_srcdir: Path | None = None
    if gate.language == "c":
        for h in code_root.rglob("*.h"):
            shutil.copy(h, d / h.name)
        # the arm's C source dir (where the engine lives) — used by _compile_run to
        # resolve link-time undefined symbols from sibling no-main .c helpers a fix
        # refactored into (clang has no -sourcepath). Absolute for the same reason
        # as java_sourcepath (clang runs with cwd=<tempdir>).
        eng_hits = list(code_root.rglob(gate.engines[0])) if gate.engines else []
        if eng_hits:
            c_srcdir = eng_hits[0].resolve().parent
    elif gate.language == "java" and gate.model.exists():
        # Stage the AGENT's version of each pinned model file (analog of C
        # headers), so a fix that edits a shared model class is honored. The arm
        # is a full codebase copy, so an unedited model file here is byte-
        # identical to pristine; _compile_run fills any the agent lacks.
        for name in {j.name for j in gate.model.rglob("*.java")}:
            hits = list(code_root.rglob(name))
            if hits:
                shutil.copy(hits[0], d / name)
    for inp in gate.inputs:
        f = gate.fixture / inp
        if not f.exists():
            return f"MISSING fixture {inp} ({f})", 98, d, {}
        shutil.copy(f, d / inp)
    return _compile_run(gate, d, java_sourcepath=java_sourcepath, c_srcdir=c_srcdir)


def selftest_main(gate: NativeGate) -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--side", choices=["start", "golden"])
    ap.add_argument("--keep", action="store_true")
    a = ap.parse_args()
    sys.path.insert(0, str(gate.here))
    import importlib
    g = importlib.import_module(gate.grader_module).Grader()

    def run(side):
        out, ec, d, _log = build_and_run(gate, side)
        if a.side or a.keep:
            print(f"--- {side} @ {d} (ec={ec})\n{out}")
        return out, ec, d

    if a.side:
        run(a.side)
        return 0
    son, sec, sd = run("start")
    gon, gec, gd = run("golden")
    rep = g.evaluate(gon, son, gec, sec)  # on=golden, off=start
    print("\n=== criteria (golden / start) ===")
    for c in rep["criteria"]:
        print(f"  {'OK ' if c['on_pass'] else 'XX '}golden  "
              f"{'OK ' if c['off_pass'] else 'XX '}start   {c['id']}: {c.get('expected', '')}")
    s = rep["summary"]
    print(f"\ngolden {s['on_passed']}/{s['total']}  |  start {s['off_passed']}/{s['total']}")
    solv = s["on_passed"] == s["total"]
    disc = s["off_passed"] < s["total"]
    print(f"GATE solvability (golden all): {'PASS' if solv else 'FAIL'}")
    print(f"GATE discrimination (start <full): {'PASS' if disc else 'FAIL'}")
    if not a.keep:
        shutil.rmtree(sd, ignore_errors=True)
        shutil.rmtree(gd, ignore_errors=True)
    return 0 if (solv and disc) else 1
