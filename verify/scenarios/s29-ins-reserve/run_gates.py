#!/usr/bin/env python3
"""Behavioral gate for s29-ins-reserve — 解約返戻金 via un-amortised 新契約費 (D-2901, COBOL).

Thin config over _gatelib. START = shipped src/ (LF210B amortises 新契約費 over 60ヶ月); GOLDEN =
reference/LF210B.cbl (correct 120ヶ月 basis). Family A (doc-code-drift): the correct 120ヶ月
amortisation basis is the group standard stated only in the 業務規程 (LF29-RULE-001); shipped code
drifted to 60ヶ月. The divergence lives ONLY in LF210B, so delta = {LF210B}. GOLDEN passes all; START fails >=1.
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
    engines=['LF210B'],
    delta={'LF210B'},
    dumps=[('DUMPCV.cbl', 'dumpcv')],
    prefix='s29',
)

build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
