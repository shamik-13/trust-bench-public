#!/usr/bin/env python3
"""Validity self-test for every build task: run all behavioral gates against
the merged codebase and assert the two properties that make a build task
meaningful.

    GOLDEN passes every test case   -> the task is solvable
    START  fails at least one       -> the task is not pre-solved

This is the "gold sanity run": on a correct install every gate is PASS. Run it
before you trust any agent score.

    python verify/run_all_gates.py                  # all 30
    python verify/run_all_gates.py s1-fees s25-pay-fee
    python verify/run_all_gates.py --json

Requires: GnuCOBOL (`cobc`) for the COBOL gates, a C compiler for the C gates,
and a JDK (`javac`/`java`) for the Java gates. Scenarios whose toolchain is
missing are reported as SKIP, not FAIL.
"""
from __future__ import annotations

import argparse
import importlib.util
import io
import json
import shutil
import sys
from contextlib import redirect_stdout
from pathlib import Path

HERE = Path(__file__).resolve().parent
SCN = HERE / "scenarios"
sys.path.insert(0, str(HERE))

TOOL_OF = {"cobol": "cobc", "c": "cc", "java": "javac"}


def load_gate(scn: str):
    """Import a scenario's run_gates.py under a unique module name."""
    for m in ("grader", "run_gates"):
        sys.modules.pop(m, None)
    path = SCN / scn / "run_gates.py"
    spec = importlib.util.spec_from_file_location(f"run_gates_{scn}", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def language_of(mod) -> str:
    return getattr(mod.GATE, "language", "cobol")


def run_one(scn: str) -> dict:
    mod = load_gate(scn)
    lang = language_of(mod)
    if shutil.which(TOOL_OF[lang]) is None:
        return {"scenario": scn, "status": "SKIP",
                "detail": f"{TOOL_OF[lang]} not on PATH ({lang} gate)"}

    gate_dir = str(mod.GATE.here)
    sys.path.insert(0, gate_dir)
    buf = io.StringIO()
    try:
        grader_mod = getattr(mod.GATE, "grader_module", "grader")
        grader = importlib.import_module(grader_mod).Grader()
        with redirect_stdout(buf):
            son, sec, sd, _ = mod.build_and_run("start")
            gon, gec, gd, _ = mod.build_and_run("golden")
            rep = grader.evaluate(gon, son, gec, sec)   # on=golden, off=start
        shutil.rmtree(sd, ignore_errors=True)
        shutil.rmtree(gd, ignore_errors=True)
        s = rep["summary"]
        solvable = s["on_passed"] == s["total"]
        discriminating = s["off_passed"] < s["total"]
        return {
            "scenario": scn, "language": lang,
            "status": "PASS" if (solvable and discriminating) else "FAIL",
            "golden": f"{s['on_passed']}/{s['total']}",
            "start": f"{s['off_passed']}/{s['total']}",
            "solvable": solvable, "discriminating": discriminating,
            "failing_start_criteria": [c["id"] for c in rep["criteria"] if not c["off_pass"]],
        }
    except Exception as e:  # a broken toolchain shows up here
        return {"scenario": scn, "language": lang, "status": "FAIL",
                "detail": f"{e!r} :: {buf.getvalue()[-300:]}"}
    finally:
        sys.modules.pop("grader", None)
        if gate_dir in sys.path:
            sys.path.remove(gate_dir)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("scenarios", nargs="*", help="default: every scenario with a gate")
    ap.add_argument("--json", action="store_true")
    a = ap.parse_args()

    todo = a.scenarios or sorted(p.name for p in SCN.iterdir() if p.is_dir())
    results = []
    for scn in todo:
        if not (SCN / scn / "run_gates.py").exists():
            print(f"  [SKIP] {scn}: no gate (answer-judged scenario)")
            continue
        r = run_one(scn)
        results.append(r)
        if not a.json:
            line = f"  [{r['status']}] {scn:24s}"
            if "detail" in r:
                print(f"{line} {r['detail']}")
            else:
                print(f"{line} golden {r['golden']}  |  start {r['start']}")

    if a.json:
        print(json.dumps(results, ensure_ascii=False, indent=2))
    else:
        n_pass = sum(r["status"] == "PASS" for r in results)
        n_fail = sum(r["status"] == "FAIL" for r in results)
        n_skip = sum(r["status"] == "SKIP" for r in results)
        print(f"\n{n_pass} pass, {n_fail} fail, {n_skip} skipped")
    return 1 if any(r["status"] == "FAIL" for r in results) else 0


if __name__ == "__main__":
    sys.exit(main())
