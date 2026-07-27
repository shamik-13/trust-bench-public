#!/usr/bin/env python3
"""Behavioral gate for s18-card-authorization (Java) — thin config over _gatelib_native.

START = shipped src/AuthService.java (available credit = limit − balance; IGNORES pending holds);
GOLDEN = reference/AuthService.java (subtracts active authorization holds). Both read the staged
CDCARDF/CDBALF/CDAUTHF fixtures and print one `authId|decision|reason` line per request to stdout.
Gates: GOLDEN passes all criteria, START fails >=1 (it over-approves a hold-reduced request)."""

import sys
from functools import partial
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # benchmark/scenarios/
from _gatelib_native import (  # noqa: E402
    NativeGate,
    build_and_run as _bar,
    build_and_run_dir as _bard,
    selftest_main as _main,
)

GATE = NativeGate(
    here=Path(__file__).resolve().parent,
    language="java",
    engines=["AuthService.java"],
    delta={"AuthService.java"},
    inputs=["CDCARDF", "CDBALF", "CDAUTHF"],
    prefix="s18",
    main_class="AuthService",
    grader_module="grader",
)

build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
