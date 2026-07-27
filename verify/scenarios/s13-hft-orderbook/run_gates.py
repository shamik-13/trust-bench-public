#!/usr/bin/env python3
"""s13-hft-orderbook SEC3 behavioral gate — tick-size validation (D-SEC-003, code-drift, C).

START = shipped out/codebase/c/src/mihft_book.c (uses its own STALE local tick table where
tier 2 = ¥1); GOLDEN = reference/mihft_book.c (tick table matches the authoritative
mihft_tick / SCINSTF: tier 2 = ¥5). The authoritative source mihft_tick.c ships in the
estate for the agent to consult. Gates: GOLDEN passes all criteria, START fails >=1.
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
    engines=["mihft_book.c"],
    delta={"mihft_book.c"},
    inputs=["tick_orders.csv"],
    prefix="sec3",
    grader_module="grader",
)

build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
