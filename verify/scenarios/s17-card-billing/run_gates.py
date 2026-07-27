#!/usr/bin/env python3
"""Behavioral gate for s17-card-billing — thin config over _gatelib (shared compile/run).

START = shipped src/ (CB910S = old 約定率3%, no floor); GOLDEN = src/ + reference/CB910S.cbl
(改定後 約定率5% + 最低支払額¥2,000). The minimum-payment divergence lives ONLY in CB910S, so
delta = {CB910S}. Gates: GOLDEN passes all criteria, START fails >=1. See _gatelib.py."""

import sys
from functools import partial
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # benchmark/scenarios/
from _gatelib import (  # noqa: E402
    Gate,
    build_and_run as _bar,
    build_and_run_dir as _bard,
    selftest_main as _main,
)

GATE = Gate(
    here=Path(__file__).resolve().parent,
    engines=['CB110B', 'CB910S', 'CB920S'],   # first = main driver
    delta={'CB910S'},                          # only the min-payment sub ships a golden reference
    dumps=[('DUMPBILL.cbl', 'dumpbill')],
    prefix='s17',
)

# module-level entry points the harness (pipeline.py) + self-test call
build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
