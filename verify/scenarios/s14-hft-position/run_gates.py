#!/usr/bin/env python3
"""s14-hft-position SEC4 behavioral gate — position average-cost (D-SEC-004, C).

START = shipped out/codebase/c/src/mihft_pos.c (live last-price path; weighted-average path
present but dead behind a never-set flag); GOLDEN = reference/mihft_pos.c (weighted average
live). Self-contained single-file engine (own main reads pos_fills.csv). Gates: GOLDEN passes
all criteria, START fails >=1.
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
    engines=["mihft_pos.c"],
    delta={"mihft_pos.c"},
    inputs=["pos_fills.csv"],
    prefix="sec4",
    grader_module="grader",
)

build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
