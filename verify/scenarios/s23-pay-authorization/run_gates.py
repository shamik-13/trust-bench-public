#!/usr/bin/env python3
"""s23-pay-authorization behavioral gate — wallet authorization decision (D-2301, Java).

Thin config over _gatelib_native (javac compile+run). START = shipped
out/codebase/java/src/jp/mirai/pay/authorization/AuthEngine.java (subtracts expired holds);
GOLDEN = reference/AuthEngine.java (releases expired holds). The pinned Java model (PayModel +
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
    engines=["AuthEngine.java"],
    delta={"AuthEngine.java"},        # the only file with a golden reference (the D-2301 fix)
    inputs=["wallets.csv", "holds.csv", "pending.csv", "requests.csv"],
    prefix="pay1",
    main_class="jp.mirai.pay.authorization.AuthEngine",
    grader_module="grader",
)

# module-level entry points the harness (pipeline.py) + self-test call
build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
