#!/usr/bin/env python3
"""Grader for the S8 interest-accrual behavioral test. One criterion per account: the posted
INT-AMT must equal the GOLDEN expectation, which the grader recomputes from the fixture
INDEPENDENTLY of the golden code: accrue on the holiday-ROLLED basis date (CR-RESOLVED-DT, D-1402)
using the ACTUAL/365 day-count (D-1403), interest = trunc(bal * rate * actual_days / 365).
Parses DUMPINT output lines: INT|acct|amt|days .
"""
from __future__ import annotations
import json
from datetime import date
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())
CYC = {c["cycle"]: c for c in FIX["cycles"]}


def _d(yyyymmdd):
    s = int(yyyymmdd)
    return date(s // 10000, (s // 100) % 100, s % 100)


def _expected(acct):
    """GOLDEN expected truncated-yen interest for one account (actual/365, rolled basis date)."""
    end = CYC[acct["cycle"]]["resolved"]            # D-1402: accrue to the holiday-rolled basis date
    days = (_d(end) - _d(acct["start"])).days       # D-1403: ACTUAL elapsed calendar days
    return int(acct["bal"] * acct["rate"] * days / 365)   # /365, truncate to yen


def _parse(output):
    posted = {}
    for ln in (output or "").splitlines():
        ln = ln.strip()
        if ln.startswith("INT|"):
            parts = ln.split("|")
            if len(parts) >= 3:
                try:
                    posted[parts[1].strip()] = int(parts[2].strip())
                except ValueError:
                    pass
    return posted


class Grader:
    criteria = [{"id": a["acct"][-2:],
                 "label": f"acct {a['acct'][-4:]} — {a['axis']}",
                 "expected": f"interest = {_expected(a)} 円 (actual/365, basis {CYC[a['cycle']]['resolved']})"}
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
