#!/usr/bin/env python3
"""s11-hft-risk SEC2 behavioral gate — matching priority (D-SEC-002, C).

START = shipped out/codebase/c/src/mihft_match.c (fills the LARGEST resting order first at
equal price — size priority); GOLDEN = reference/mihft_match.c (price-time priority: FIFO at
equal price). Self-contained single-file engine (own main reads match_book.csv). TIF handling
(D-SEC-005) is present for the answer-judged SEC5 task. Gates: GOLDEN passes all, START fails >=1.
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
    engines=["mihft_match.c"],
    delta={"mihft_match.c"},
    inputs=["match_book.csv"],
    prefix="sec2",
    grader_module="grader",
)

build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
