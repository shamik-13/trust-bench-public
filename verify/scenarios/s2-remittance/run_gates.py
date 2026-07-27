#!/usr/bin/env python3
"""Behavioral gate for s2-remittance — thin config over _gatelib (shared compile/run).

START = shipped src/; GOLDEN = src/ + reference/ for delta programs. Gates:
GOLDEN passes all, START fails >=1. See _gatelib.py for the machinery."""

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
    engines=['TG214B', 'TG912S', 'TG920S'],   # S2-PC2: inbound reject-validation pipeline
    engines2=['TG330B', 'TG931S'],            # S2-C4: 訂正 correction-output pipeline (+ signature sub)
    delta={'TG214B', 'TG330B'},               # both ship a golden reference
    dumps=[('DUMPREJ.cbl', 'dumprej'), ('DUMPOUT.cbl', 'dumpout')],
    prefix='s2',
)

# module-level entry points the harness (pipeline.py) + self-test call
build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
