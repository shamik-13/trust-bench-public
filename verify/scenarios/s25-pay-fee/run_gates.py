#!/usr/bin/env python3
"""s25-pay-fee behavioral gate — merchant MDR fee by 業種区分 rate table (D-2501, Java).

Thin config over _gatelib_native (javac compile+run). START = shipped
out/codebase/java/src/jp/mirai/pay/fee/MdrFeeEngine.java (OLD rate table); GOLDEN =
reference/MdrFeeEngine.java (改定後 rate table from the 規程 CD-RULE-MDR). Family A: the 改定後
rates live ONLY in the doc — the agent must read it to recover them. Gates: GOLDEN passes all
criteria, START fails >=1.
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
    engines=["MdrFeeEngine.java"],
    delta={"MdrFeeEngine.java"},      # the only file with a golden reference (the D-2501 fix)
    inputs=["merchants.csv", "transactions.csv"],
    prefix="pay3",
    main_class="jp.mirai.pay.fee.MdrFeeEngine",
    grader_module="grader",
)

# module-level entry points the harness (pipeline.py) + self-test call
build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
