#!/usr/bin/env python3
"""s26-pay-refund behavioral gate — refund eligibility window (D-2601, Java).

Thin config over _gatelib_native (javac compile+run). START = shipped
out/codebase/java/src/jp/mirai/pay/refund/RefundEngine.java (OLD 90-day window); GOLDEN =
reference/RefundEngine.java (改定後 180-day window from the 返金規程 CD-RULE-REFUND). Family A: the
改定後 window lives ONLY in the doc. Gates: GOLDEN passes all criteria, START fails >=1.
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
    engines=["RefundEngine.java"],
    delta={"RefundEngine.java"},      # the only file with a golden reference (the D-2601 fix)
    inputs=["originals.csv", "requests.csv"],
    prefix="pay4",
    main_class="jp.mirai.pay.refund.RefundEngine",
    grader_module="grader",
)

# module-level entry points the harness (pipeline.py) + self-test call
build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
