#!/usr/bin/env python3
"""s24-pay-settlement behavioral gate — merchant settlement charge rounding (D-2401, C).

Thin config over _gatelib_native (clang compile+run). START = shipped
out/codebase/c/src/mipay_settle.c (truncates the charge); GOLDEN = reference/mipay_settle.c
(rounds half-up). Gates: GOLDEN passes all criteria, START fails >=1.
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
    engines=["mipay_settle.c", "mipay_net.c"],
    delta={"mipay_settle.c"},         # the only file with a golden reference (the D-2401 fix)
    inputs=["merchants.csv", "transactions.csv"],
    prefix="pay2",
)

# module-level entry points the harness (pipeline.py) + self-test call
build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
