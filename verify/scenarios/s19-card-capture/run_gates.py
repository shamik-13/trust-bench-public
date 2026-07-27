#!/usr/bin/env python3
"""Behavioral gate for s19-card-capture — thin config over _gatelib.

START = shipped src/ (CB590S = old 海外事務手数料率 1.60%); GOLDEN = reference/CB590S.cbl (改定後
2.20%). The foreign-usage fee divergence lives ONLY in CB590S, so delta = {CB590S}. Gates: GOLDEN
passes all, START fails >=1 (it under-charges foreign sales). See _gatelib.py."""

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
    engines=['CB510B', 'CB590S'],     # first = main capture driver
    delta={'CB590S'},                  # only the fee sub ships a golden reference
    dumps=[('DUMPCAP.cbl', 'dumpcap')],
    prefix='s19',
    # Phase B renamed this scenario's CDCARDFC to CDCARD03 (another scenario's
    # copybook of that name won the collision). harness/LOADER.cbl still COPYs
    # the authored name, so against the merged tree it must be re-pointed at
    # this scenario's layout — see _gatelib._stage_copybooks.
    cpy_alias={'CDCARDFC': 'CDCARD03'},
)

build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
