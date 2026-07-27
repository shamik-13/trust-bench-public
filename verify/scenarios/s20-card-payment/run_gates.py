#!/usr/bin/env python3
"""Behavioral gate for s20-card-payment — thin config over _gatelib. START = src/CB610B (applies
principal-first); GOLDEN = reference/CB610B (fee->interest->principal). delta={CB610B}."""
import sys
from functools import partial
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
from _gatelib import Gate, build_and_run as _bar, build_and_run_dir as _bard, selftest_main as _main  # noqa: E402
GATE = Gate(here=Path(__file__).resolve().parent, engines=['CB610B'], delta={'CB610B'},
            dumps=[('DUMPAPP.cbl', 'dumpapp')], prefix='s20')
build_and_run = partial(_bar, GATE); build_and_run_dir = partial(_bard, GATE); main = partial(_main, GATE)
if __name__ == "__main__":
    sys.exit(main())
