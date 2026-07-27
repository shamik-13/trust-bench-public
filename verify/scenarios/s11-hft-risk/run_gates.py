#!/usr/bin/env python3
"""s11-hft-risk behavioral gate — thin config over _gatelib_native (C compile+run).

START = shipped out/codebase/c/src/; GOLDEN = src/ + reference/ for the delta file. Gates:
GOLDEN passes all criteria, START fails >=1. The same module-level entry points the harness
pipeline (verify/pipeline.py) calls are exported, so headless + UI verify use this rig.
"""
import sys
from functools import partial
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # benchmark/scenarios/
from _gatelib_native import (  # noqa: E402
    NativeGate,
    build_and_run as _bar,
    build_and_run_dir as _bard,
    selftest_main as _main,
)

GATE = NativeGate(
    here=Path(__file__).resolve().parent,
    language="c",
    engines=["mihft_gateway.c", "mihft_margin.c", "mihft_risk.c"],
    delta={"mihft_risk.c"},          # the only file with a golden reference (the SEC1 fix)
    inputs=["orders.csv", "customers.csv"],
    prefix="sec1",
)

# module-level entry points the harness (pipeline.py) + self-test call
build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
