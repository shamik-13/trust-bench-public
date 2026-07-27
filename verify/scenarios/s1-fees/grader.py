#!/usr/bin/env python3
"""Grader for the over-limit-fee behavioral test. Criteria are derived from the
pinned data_domain + decisions (NOT from golden code). Parses the DUMPFEE output
(FEE|acct|fee_amt|fee_ytd|cap|exempt) and checks each decision's effect.

Expected (golden) per fixture.json:
  ACCT000001 STD0 over 200000 -> 改定後 0.0180 => 3600     (D-0772 rate field)
  ACCT000002 PREM over 200000 -> PREM->STD0 0.0180 => 3600 (D-0612 deprecation)
  ACCT000003 STD0 over 5000000 -> 90000 capped to 50000, CAP=Y (D-0701 cap)
  ACCT000004 EXMP over 200000 -> exempt => 0, EXEMPT=Y      (D-0457 exemption)
  ACCT000005 GLD1 over 100000 -> 改定後 0.0120 => 1200      (D-0457 preferential + D-0772)
Start state fails C1/C2/C3/C5 (old rate, PREM own rate, no cap); golden passes all.
"""
from __future__ import annotations
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())


def _parse(output):
    """DUMPFEE lines -> {acct: {fee, ytd, cap, exempt}}."""
    rows = {}
    for ln in (output or "").splitlines():
        ln = ln.strip()
        if not ln.startswith("FEE|"):
            continue
        _, acct, fee, ytd, cap, exempt = (ln.split("|") + [""] * 6)[:6]
        rows[acct.strip()] = {"fee": float(fee or 0), "ytd": float(ytd or 0),
                              "cap": cap.strip(), "exempt": exempt.strip()}
    return rows


class Grader:
    # the single source the frontend displays
    criteria = [
        {"id": "C1-rate-revision", "label": "STD0 fee uses 改定後 rate 0.0180 (D-0772)", "expected": "ACCT000001 fee = 3600"},
        {"id": "C2-prem-deprecated", "label": "PREM charged standard rate, not a PREM rate (D-0612)", "expected": "ACCT000002 fee = 3600"},
        {"id": "C3-cycle-cap", "label": "per-cycle fees capped at ¥50,000 + CAP-FLAG set (D-0701)", "expected": "ACCT000003 fee = 50000, cap = Y"},
        {"id": "C4-exemption", "label": "rate-0 group is exempt (D-0457)", "expected": "ACCT000004 fee = 0, exempt = Y"},
        {"id": "C5-preferential", "label": "GLD1 preferential 改定後 rate 0.0120", "expected": "ACCT000005 fee = 1200"},
        {"id": "C6-floor-undercap", "label": "STD0 fractional fee floored 円未満切捨て (D-0733) + under-cap CAP-FLAG=N (D-0701)", "expected": "ACCT000006 fee = 1800 (floor of 1800.90, not 1801), cap = N"},
    ]

    def _check_side(self, output, exit_code):
        r = _parse(output)
        def fee(a): return r.get(a, {}).get("fee")
        res = {
            "C1-rate-revision":   fee("ACCT000001") == 3600,
            "C2-prem-deprecated": fee("ACCT000002") == 3600,
            "C3-cycle-cap":       fee("ACCT000003") == 50000 and r.get("ACCT000003", {}).get("cap") == "Y",
            "C4-exemption":       fee("ACCT000004") == 0 and r.get("ACCT000004", {}).get("exempt") == "Y",
            "C5-preferential":    fee("ACCT000005") == 1200,
            "C6-floor-undercap":  fee("ACCT000006") == 1800 and r.get("ACCT000006", {}).get("cap") == "N",
        }
        return res, r

    def evaluate(self, on_out, off_out, on_exit, off_exit):
        on_res, on_rows = self._check_side(on_out, on_exit)
        off_res, off_rows = self._check_side(off_out, off_exit)
        crit = []
        for c in self.criteria:
            crit.append({**c, "on_pass": bool(on_res[c["id"]]), "off_pass": bool(off_res[c["id"]])})
        return {
            "criteria": crit,
            "summary": {
                "on_passed": sum(c["on_pass"] for c in crit), "off_passed": sum(c["off_pass"] for c in crit),
                "total": len(crit), "on_rows": on_rows, "off_rows": off_rows,
            },
        }
