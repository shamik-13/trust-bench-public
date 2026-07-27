#!/usr/bin/env python3
"""Shared COBOL behavioral-gate machinery for trust-bench scenarios.

Each scenario's verify/run_gates.py is a THIN config over this module: it
declares its engines, golden-delta set, dump programs, tempdir prefix, and
execution mode, then binds the build_and_run / build_and_run_dir / main entry
points the harness + self-test call. Centralizing the compile/run logic here
means a toolchain change (cobc flags, the -fformat=variable multibyte fix, the
build layout) is made ONCE instead of in ten files.

`-fformat=variable` keeps fixed-FORM source but drops the column-72 right
margin, so Japanese (multibyte, 3-byte UTF-8) DISPLAY literals don't truncate
mid-token — the bug that made every behavioral gate fail before this fix.

Layout assumed for every scenario (verify/ = `gate.here`):
    <release>/codebase/cobol/                merged start tree (.cbl + .cpy, flat)
    here/reference/<id>.cbl                  golden delta programs
    here/harness/{LOADER,DUMP*}.cbl          test scaffolding
    here/harness/gen_text.py                 fixture -> flat-file loader
    here/fixture/fixture.json                fixed test inputs

Execution modes:
    "combined" — all engines linked into ONE `engine` binary, run once
                 (the common case; subprograms statically linked into a driver).
    "separate" — each engine compiled + run as its OWN binary (independent
                 drivers that each have a main, e.g. s4's KZ150B / KZ160B).
"""

from __future__ import annotations

import argparse
import os
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

CC = ["cobc", "-x", "-fformat=variable", "-I", "cpy"]


def _dec(b):
    return (b or b"").decode("utf-8", "replace")


def _run(cmd, cwd):
    p = subprocess.run(cmd, cwd=str(cwd), capture_output=True)
    p.out = _dec(p.stdout)
    p.err = _dec(p.stderr)
    return p


@dataclass
class Gate:
    here: Path  # the scenario's verify/ dir
    engines: list  # program ids; first = main (combined mode)
    delta: set  # ids that ship a golden reference
    dumps: list  # [(cbl_filename, binary_name), ...] — dump programs
    prefix: str  # tempdir prefix
    engine_mode: str = "combined"  # "combined" | "separate"
    engine_cc_extra: tuple = ()  # extra cobc flags for the engine compile only
    engines2: list = field(default_factory=list)  # optional 2nd independent combined
    # pipeline (its own main, first = main); compiled to `engine2` and run AFTER `engine`.
    # Lets one scenario gate two unrelated batches that share the loaded input files
    # (e.g. s2: TG214B reject-validation + TG330B correction-output). delta still drives
    # which of these ship a golden reference. Only honored in combined engine_mode.
    cpy_alias: dict = field(default_factory=dict)  # {pre_merge_name: merged_name}
    # Phase B renames a copybook whose name collided across scenarios (s19's CDCARDFC
    # became CDCARD03; another scenario kept the plain name). See _stage_copybooks.

    @property
    def src(self) -> Path:
        return CODEBASE / "cobol"  # merged tree is flat: .cbl and .cpy together

    @property
    def cpy(self) -> Path:
        return CODEBASE / "cobol"

    @property
    def ref(self) -> Path:
        return self.here / "reference"

    @property
    def harn(self) -> Path:
        return self.here / "harness"

    @property
    def fixture(self) -> Path:
        return self.here / "fixture" / "fixture.json"


def _stage_copybooks(gate: Gate, d: Path, cpys):
    """Copy `cpys` into d/cpy, then apply the gate's Phase-B rename aliases.

    The rig's OWN scaffolding (LOADER/DUMP) must speak the same record layout as
    the code under test, and it has to keep working against BOTH trees: the
    per-scenario out/ tree, where a copybook has its authored name, and the
    merged tree, where a name collision may have renamed it (s19's CDCARDFC ->
    CDCARD03) while ANOTHER scenario's copybook of the same name kept the plain
    one. Left alone, `COPY CDCARDFC` in the merged build silently binds the other
    scenario's layout and the run dies at OPEN with a record-length mismatch
    (ST=39) — which looks like a start state that fails every criterion rather
    than like a broken rig.

    So after staging we re-copy each renamed copybook UNDER its pre-merge name.
    In the per-scenario tree the renamed file does not exist and this is a no-op.
    """
    (d / "cpy").mkdir(exist_ok=True)
    for c in cpys:
        shutil.copy(c, d / "cpy" / c.name)
    for pre_merge, merged in gate.cpy_alias.items():
        merged_f = d / "cpy" / f"{merged}.cpy"
        if merged_f.exists():
            shutil.copy(merged_f, d / "cpy" / f"{pre_merge}.cpy")


def _finish(gate: Gate, d: Path):
    """Compile (engines + scaffolding) + run from an already-populated build dir
    `d` (engine .cbl at top level, copybooks in d/cpy/). Returns the dump text,
    exit code, dir, and per-step log. Shared by the self-test and harness paths."""
    shutil.copy(gate.harn / "LOADER.cbl", d / "LOADER.cbl")
    for cbl, _bin in gate.dumps:
        shutil.copy(gate.harn / cbl, d / cbl)
    subprocess.run(
        [sys.executable, str(gate.harn / "gen_text.py"), str(gate.fixture), str(d)],
        capture_output=True,
    )

    log: dict = {}
    eflags = list(gate.engine_cc_extra)
    if gate.engine_mode == "separate":
        engine_bins = [pid.lower() for pid in gate.engines]
        for pid, b in zip(gate.engines, engine_bins):
            log[f"cc_{b}"] = _run(
                [
                    "cobc",
                    "-x",
                    "-fformat=variable",
                    *eflags,
                    "-I",
                    "cpy",
                    "-o",
                    b,
                    f"{pid}.cbl",
                ],
                d,
            )
    else:
        engine_bins = ["engine"]
        log["cc_engine"] = _run(
            ["cobc", "-x", "-fformat=variable", *eflags, "-I", "cpy", "-o", "engine"]
            + [f"{p}.cbl" for p in gate.engines],
            d,
        )
        if gate.engines2:
            log["cc_engine2"] = _run(
                ["cobc", "-x", "-fformat=variable", *eflags, "-I", "cpy", "-o", "engine2"]
                + [f"{p}.cbl" for p in gate.engines2],
                d,
            )
    log["cc_loader"] = _run(CC + ["-o", "loader", "LOADER.cbl"], d)
    for cbl, bin_ in gate.dumps:
        log[f"cc_{bin_}"] = _run(CC + ["-o", bin_, cbl], d)

    for k, r in list(log.items()):
        if k.startswith("cc_") and r.returncode != 0:
            return f"COMPILE FAIL [{k}]:\n{r.err}", 99, d, log

    log["loader"] = _run(["./loader"], d)
    ec = 0
    for b in engine_bins:
        log[b] = _run([f"./{b}"], d)
        ec = ec or log[b].returncode
    if gate.engines2:
        log["engine2"] = _run(["./engine2"], d)
        ec = ec or log["engine2"].returncode
    outs = []
    for _cbl, bin_ in gate.dumps:
        log[bin_] = _run([f"./{bin_}"], d)
        outs.append(log[bin_].out)
    return "\n".join(outs), ec, d, log


def build_and_run(gate: Gate, side: str):
    """Self-test path: engines from the shipped tree (start) or reference deltas
    (golden); copybooks from the canonical CPY."""
    d = Path(tempfile.mkdtemp(prefix=f"{gate.prefix}_{side}_"))
    _stage_copybooks(gate, d, gate.cpy.glob("*.cpy"))
    for pid in (*gate.engines, *gate.engines2):
        srcf = (
            gate.ref if (side == "golden" and pid in gate.delta) else gate.src
        ) / f"{pid}.cbl"
        if not srcf.exists():
            return f"MISSING {pid}.cbl ({srcf})", 98, d, {}
        shutil.copy(srcf, d / f"{pid}.cbl")
    return _finish(gate, d)


def build_and_run_dir(gate: Gate, code_root):
    """Harness path: engines AND copybooks sourced from an arbitrary code tree
    (the agent's edited run repo), found by name regardless of layout. Engine
    order (first = main) is preserved via gate.engines."""
    code_root = Path(code_root)
    d = Path(tempfile.mkdtemp(prefix=f"{gate.prefix}_agent_"))
    _stage_copybooks(gate, d, code_root.rglob("*.cpy"))
    for pid in (*gate.engines, *gate.engines2):
        hits = list(code_root.rglob(f"{pid}.cbl"))
        if not hits:
            return f"MISSING {pid}.cbl under {code_root}", 98, d, {}
        shutil.copy(hits[0], d / f"{pid}.cbl")
    return _finish(gate, d)


def selftest_main(gate: Gate) -> int:
    """CLI self-test: compile+run START (shipped) and GOLDEN (reference deltas),
    score with the scenario's Grader, and assert the two gates that make the
    test meaningful — GOLDEN passes all (solvable), START fails >=1 (not
    pre-solved). `python run_gates.py [--side start|golden] [--keep]`."""
    ap = argparse.ArgumentParser()
    ap.add_argument("--side", choices=["start", "golden"])
    ap.add_argument("--keep", action="store_true")
    a = ap.parse_args()
    sys.path.insert(0, str(gate.here))
    from grader import Grader

    g = Grader()

    def run(side):
        out, ec, d, _log = build_and_run(gate, side)
        if a.side or a.keep:
            print(f"--- {side} @ {d}\n{out}")
        return out, ec, d

    if a.side:
        run(a.side)
        return 0
    son, sec, sd = run("start")
    gon, gec, gd = run("golden")
    rep = g.evaluate(gon, son, gec, sec)  # on=golden, off=start
    print("\n=== criteria (golden / start) ===")
    for c in rep["criteria"]:
        print(
            f"  {'OK ' if c['on_pass'] else 'XX '}golden  "
            f"{'OK ' if c['off_pass'] else 'XX '}start   {c['id']}: {c.get('expected', '')}"
        )
    s = rep["summary"]
    print(
        f"\ngolden {s['on_passed']}/{s['total']}  |  start {s['off_passed']}/{s['total']}"
    )
    solv = s["on_passed"] == s["total"]
    disc = s["off_passed"] < s["total"]
    print(f"GATE solvability (golden all): {'PASS' if solv else 'FAIL'}")
    print(f"GATE discrimination (start <full): {'PASS' if disc else 'FAIL'}")
    shutil.rmtree(sd, ignore_errors=True)
    shutil.rmtree(gd, ignore_errors=True)
    return 0 if (solv and disc) else 1
