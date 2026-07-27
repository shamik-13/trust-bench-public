#!/usr/bin/env python3
"""Behavioral gate for s7-warehouse — thin config over _gatelib (shared compile/run).

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
    engines=['JH410B'],
    delta={'JH410B'},
    dumps=[('DUMPDWH.cbl', 'dumpdwh')],
    prefix='s7',
)

# module-level entry points the harness (pipeline.py) + self-test call
build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
