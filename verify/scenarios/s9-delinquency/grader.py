#!/usr/bin/env python3
"""Grader for the S9 delinquency behavioral test. TWO criteria per account, recomputed from the
fixture INDEPENDENTLY of the golden code:
  - late-charge: trunc(amt * 0.146 * max(0, days_overdue - 10) / 365)  [D-1503 grace, tribal]
  - new-status:  '00' if days<=0, '10' if 1..60, '30' (回収) if >=61    [D-1502 collection trigger]
Parses DUMPDLR lines: DLR|acct|late-charge|status|days|bucket .
"""
from __future__ import annotations
import json
from datetime import date
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())
ASOF = FIX["asof"]; RATE = FIX["rate"]; GRACE = FIX["grace_days"]; COLL = FIX["collection_min_days"]


def _d(yyyymmdd):
    s = int(yyyymmdd)
    return date(s // 10000, (s // 100) % 100, s % 100)


def _days(due):
    n = (_d(ASOF) - _d(due)).days
    return max(0, n)


def _exp_charge(a):
    d = _days(a["due"])
    return int(a["amt"] * RATE * max(0, d - GRACE) / 365)   # truncate to yen


def _exp_status(a):
    d = _days(a["due"])
    if d <= 0:   return "00"
    if d >= COLL: return "30"
    return "10"


def _parse(output):
    posted = {}
    for ln in (output or "").splitlines():
        ln = ln.strip()
        if ln.startswith("DLR|"):
            p = ln.split("|")
            if len(p) >= 4:
                try:
                    posted[p[1].strip()] = (int(p[2].strip()), p[3].strip())
                except ValueError:
                    pass
    return posted


class Grader:
    criteria = []
    for _a in FIX["accounts"]:
        _t = _a["acct"][-2:]
        criteria.append({"id": f"{_t}-chg", "label": f"acct {_a['acct'][-4:]} late-charge — {_a['axis']}",
                         "expected": f"{_exp_charge(_a)} 円 (grace 10d)"})
        criteria.append({"id": f"{_t}-sts", "label": f"acct {_a['acct'][-4:]} status — {_a['axis']}",
                         "expected": f"{_exp_status(_a)} ({_days(_a['due'])}日 overdue)"})

    def _check(self, output):
        posted = _parse(output)
        res = {}
        for a in FIX["accounts"]:
            t = a["acct"][-2:]
            chg, sts = posted.get(a["acct"], (None, None))
            res[f"{t}-chg"] = (chg == _exp_charge(a))
            res[f"{t}-sts"] = (sts == _exp_status(a))
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
