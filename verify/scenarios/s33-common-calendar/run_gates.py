#!/usr/bin/env python3
"""Behavioral gate for s33-common-calendar — 資金集中受渡日 via 翌々営業日 basis (D-3301, COBOL).

Thin config over _gatelib. START = shipped src/ (CC110B uses 翌営業日 / 1営業日後); GOLDEN =
reference/CC110B.cbl (correct 翌々営業日 / 2営業日後). Family A: the 翌々営業日 basis is doc-resident.
The divergence lives ONLY in CC110B, so delta = {CC110B}. GOLDEN passes all; START fails >=1.
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
    engines=['CC110B'],
    delta={'CC110B'},
    dumps=[('DUMPVAL.cbl', 'dumpval')],
    prefix='s33',
)

build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
