#!/usr/bin/env python3
"""s31-ins-claims behavioral gate — 保険金支払 削減割合 (D-3101, Java).

Thin config over _gatelib_native (javac compile+run). START = shipped
out/codebase/java/src/jp/mirai/life/claims/ClaimPayoutEngine.java (OLD 75% 削減率); GOLDEN =
reference/ClaimPayoutEngine.java (改定後 60% from the 約款/規程 LF-RULE-CLAIM). Family A: the 改定後
削減割合 lives ONLY in the doc. Gates: GOLDEN passes all criteria, START fails >=1.
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
    engines=["ClaimPayoutEngine.java"],
    delta={"ClaimPayoutEngine.java"},
    inputs=["claims.csv"],
    prefix="life1",
    main_class="jp.mirai.life.claims.ClaimPayoutEngine",
    grader_module="grader",
)

build_and_run = partial(_bar, GATE)
build_and_run_dir = partial(_bard, GATE)
main = partial(_main, GATE)

if __name__ == "__main__":
    sys.exit(main())
