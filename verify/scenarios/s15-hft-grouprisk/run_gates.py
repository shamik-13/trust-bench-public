#!/usr/bin/env python3
"""s15-hft-grouprisk SEC6 behavioral gate — group-risk aggregation (D-SEC-007, Java).

Sibling of run_gates.py (the C SEC1 gate); same thin-config-over-_gatelib_native pattern,
language="java". START = shipped out/codebase/java/src/GroupRiskService.java (signed-net
bug); GOLDEN = reference/GroupRiskService.java (gross). The pinned Java model (RiskModel +
record layouts in out/codebase/java/model) is staged automatically by _gatelib_native.
Gates: GOLDEN passes all criteria, START fails >=1.
"""
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
    engines=["GroupRiskService.java"],
    delta={"GroupRiskService.java"},   # the only file with a golden reference (the SEC6 fix)
    inputs=["positions.csv"],
    prefix="sec6",
    main_class="jp.mirai.sec.grouprisk.GroupRiskService",
    grader_module="grader",
)

# module-level entry points the harness (pipeline.py) + self-test call
build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
