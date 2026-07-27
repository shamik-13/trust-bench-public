#!/usr/bin/env python3
"""Behavioral gate for s28-ins-premium — monthly premium by 加入年齢帯×性別 rate table (D-2801, COBOL).

Thin config over _gatelib (shared compile/run). START = shipped src/ (LF110B = OLD 月額保険料率表);
GOLDEN = reference/LF110B.cbl (改定後 表 from the 規程 LF-RULE-PREM). The rate-table divergence lives
ONLY in LF110B, so delta = {LF110B}. Gates: GOLDEN passes all criteria, START fails >=1. See _gatelib.py.
"""

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
    engines=['LF110B'],                 # single combined engine (reads LFPOLF, writes LFPRMF)
    delta={'LF110B'},                   # only the premium engine ships a golden reference
    dumps=[('DUMPPRM.cbl', 'dumpprm')],
    prefix='s28',
)

# module-level entry points the harness (pipeline.py) + self-test call
build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
