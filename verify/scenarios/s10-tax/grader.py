#!/usr/bin/env python3
"""Grader for the S10 withholding-tax behavioral test. One criterion per account: the posted
TOTAL-TAX-AMT must equal the GOLDEN expectation, recomputed from the fixture with INTEGER
arithmetic (matches COBOL decimal truncation), independent of the golden code:
  '01' 一般 : nat = floor(gross * 0.15315), loc = floor(gross * 0.05), total = nat + loc
  '02' 法人 : nat = floor(gross * 0.15315), loc = 0,                    total = nat   (D-1603)
  '03' NISA : total = 0                                                              (D-1602)
national & local truncated SEPARATELY (D-1604). Parses DUMPTXR lines: TXR|acct|total|nat|local .
"""
from __future__ import annotations
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())
NAT = FIX["national_rate_ppm"]   # 153150  (0.15315 * 1e6)
LOC = FIX["local_rate_ppm"]      # 50000   (0.05    * 1e6)


def _expected(a):
    g = a["gross"]
    if a["type"] == "03":
        return 0
    nat = (g * NAT) // 1_000_000           # floor(gross * 0.15315)
    loc = (g * LOC) // 1_000_000 if a["type"] == "01" else 0
    return nat + loc


def _parse(output):
    posted = {}
    for ln in (output or "").splitlines():
        ln = ln.strip()
        if ln.startswith("TXR|"):
            p = ln.split("|")
            if len(p) >= 3:
                try:
                    posted[p[1].strip()] = int(p[2].strip())
                except ValueError:
                    pass
    return posted


class Grader:
    criteria = [{"id": a["acct"][-2:],
                 "label": f"acct {a['acct'][-4:]} ({a['type']}) — {a['axis']}",
                 "expected": f"total tax = {_expected(a)} 円"}
                for a in FIX["accounts"]]

    def _check(self, output):
        posted = _parse(output)
        res = {}
        for a in FIX["accounts"]:
            res[a["acct"][-2:]] = (posted.get(a["acct"]) == _expected(a))
        return res, posted

    def evaluate(self, on_out, off_out, on_exit, off_exit):
        on_res, on_log = self._check(on_out)
        off_res, off_log = self._check(off_out)
        crit = [{**c, "on_pass": bool(on_res.get(c["id"])), "off_pass": bool(off_res.get(c["id"]))}
                for c in self.criteria]
        return {"criteria": crit,
                "summary": {"on_passed": sum(c["on_pass"] for c in crit),
                            "off_passed": sum(c["off_pass"] for c in crit),
                            "total": len(crit),
                            "on_log": {k: on_log.get(k) for k in sorted(on_log)},
                            "off_log": {k: off_log.get(k) for k in sorted(off_log)}}}
