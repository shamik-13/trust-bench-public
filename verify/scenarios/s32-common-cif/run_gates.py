#!/usr/bin/env python3
"""Behavioral gate for s32-common-cif — 名寄せ統合キー 検査数字 via modulus-11 (D-3201, COBOL).

Thin config over _gatelib. START = shipped src/ (CM110B uses modulus-10 Luhn); GOLDEN =
reference/CM110B.cbl (correct modulus-11 weights 2..7). Family A (doc-code-drift): the modulus-11
algorithm is stated in 業務規程 CM32-RULE-001; code drifted to modulus-10 (Luhn). The divergence lives
ONLY in CM110B, so delta = {CM110B}. GOLDEN passes all; START fails >=1.
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
    engines=['CM110B'],
    delta={'CM110B'},
    dumps=[('DUMPKEY.cbl', 'dumpkey')],
    prefix='s32',
)

build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
