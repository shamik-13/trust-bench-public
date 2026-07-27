#!/usr/bin/env python3
"""Grade a build task: compile and run an agent's edited codebase and score it
against the task's hidden test cases.

    # 1. hand the agent a private copy of the codebase
    cp -R codebase /tmp/agent-run
    # 2. ... let it edit /tmp/agent-run to solve the task prompt ...
    # 3. score it
    python verify/grade_workspace.py --task S1-F6 --workspace /tmp/agent-run

Reports, per Eq. 2 of the paper:

    tests   the fraction of test cases the workspace passes
    pass    1 only if EVERY test case passes, else 0  (the headline build metric)

A workspace that fails to compile scores 0 on every test case, by design.
`--workspace` may be the whole edited codebase or any tree containing the
task's engine programs — they are located by name, so the layout is free.
"""
from __future__ import annotations

import argparse
import importlib
import importlib.util
import io
import json
import shutil
import sys
from contextlib import redirect_stdout
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
SCN = HERE / "scenarios"
sys.path.insert(0, str(HERE))


def find_task(task_id: str) -> dict:
    tasks = json.loads((ROOT / "tasks.json").read_text(encoding="utf-8"))
    for t in tasks:
        if t["task_id"].lower() == task_id.lower():
            if t["grading"] != "behavioral":
                sys.exit(f"{t['task_id']} is answer-judged, not a build task — "
                         "score it with an LLM judge against tasks.json `golden`.")
            return t
    sys.exit(f"unknown task_id {task_id!r}; see tasks.json")


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--task", required=True, help="e.g. S1-F6")
    ap.add_argument("--workspace", required=True, help="the agent's edited code tree")
    ap.add_argument("--json", action="store_true")
    a = ap.parse_args()

    task = find_task(a.task)
    scn = task["scenario"]
    ws = Path(a.workspace).resolve()
    if not ws.exists():
        sys.exit(f"no such workspace: {ws}")

    for m in ("grader", "run_gates"):
        sys.modules.pop(m, None)
    spec = importlib.util.spec_from_file_location(
        f"run_gates_{scn}", SCN / scn / "run_gates.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)

    sys.path.insert(0, str(mod.GATE.here))
    grader = importlib.import_module(getattr(mod.GATE, "grader_module", "grader")).Grader()

    buf = io.StringIO()
    with redirect_stdout(buf):
        out, exit_code, d, _log = mod.build_and_run_dir(ws)
        # the grader's two sides are (golden, start); we score one workspace, so
        # pass it as both and read the `on_*` side.
        rep = grader.evaluate(out, out, exit_code, exit_code)
    shutil.rmtree(d, ignore_errors=True)

    crit = [{"id": c["id"], "passed": bool(c["on_pass"]),
             "expected": c.get("expected", ""), "label": c.get("label", "")}
            for c in rep["criteria"]]
    n_pass = sum(c["passed"] for c in crit)
    total = len(crit)
    compile_failed = isinstance(out, str) and out.startswith(("COMPILE FAIL", "MISSING"))
    result = {
        "task_id": task["task_id"],
        "scenario": scn,
        "engine": task.get("engine"),
        "workspace": str(ws),
        "compiled": not compile_failed,
        "tests": round(n_pass / total, 4) if total else 0.0,
        "tests_passed": n_pass,
        "tests_total": total,
        "pass": int(n_pass == total and total > 0),
        "criteria": crit,
    }

    if a.json:
        print(json.dumps(result, ensure_ascii=False, indent=2))
    else:
        print(f"task {result['task_id']}  ({scn})   workspace: {ws}")
        if compile_failed:
            print(f"\n  BUILD FAILED — every test case scores 0\n  {out[:600]}")
        print("\n  test cases")
        for c in crit:
            print(f"    {'PASS' if c['passed'] else 'FAIL'}  {c['id']}"
                  f"{'  — ' + c['expected'] if c['expected'] else ''}")
        print(f"\n  tests = {n_pass}/{total} ({result['tests']:.2f})   pass = {result['pass']}")
    return 0 if result["pass"] else 1


if __name__ == "__main__":
    sys.exit(main())
