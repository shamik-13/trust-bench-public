#!/usr/bin/env python3
"""Behavioral gate for s21-card-revolving — thin config over _gatelib (shared compile/run).

START = shipped src/ (CB290S = OLD coarse 残高スライド表); GOLDEN = src/ + reference/CB290S.cbl
(改定後 表 5000/10000/15000/20000). The 元金定額表 divergence lives ONLY in CB290S, so
delta = {CB290S}. Gates: GOLDEN passes all criteria, START fails >=1. See _gatelib.py."""

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
    engines=['CB210B', 'CB290S', 'CB280S'],   # first = main driver
    delta={'CB290S'},                          # only the slide-table sub ships a golden reference
    dumps=[('DUMPSLD.cbl', 'dumpsld')],
    prefix='s21',
)

# module-level entry points the harness (pipeline.py) + self-test call
build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
