#!/usr/bin/env python3
"""Grader for s20 card-payment. DUMPAPP prints APP|pay-id|fee|int|prin|status. Each payment's
applied (fee,int,prin,status) must equal the GOLDEN allocation (fee->interest->principal)."""
from __future__ import annotations
import json
from pathlib import Path
HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())
def _parse(out):
    rows = {}
    for ln in (out or "").splitlines():
        ln = ln.strip()
        if ln.startswith("APP|"):
            p = (ln.split("|") + [""] * 6)[:6]
            try: rows[p[1].strip()] = (int(p[2]), int(p[3]), int(p[4]), p[5].strip())
            except ValueError: rows[p[1].strip()] = (-1, -1, -1, p[5].strip())
    return rows
class Grader:
    criteria = [{"id": p["pay_id"][-3:],
                 "label": f"{p['pay_id']} ¥{p['amount']:,} -> fee{p['expect_fee']}/int{p['expect_int']}/prin{p['expect_prin']}/{p['expect_status']}",
                 "expected": f"{p['expect_fee']}/{p['expect_int']}/{p['expect_prin']}/{p['expect_status']}"} for p in FIX["payments"]]
    def _check(self, out):
        rows = _parse(out); res = {}
        for p in FIX["payments"]:
            res[p["pay_id"][-3:]] = (rows.get(p["pay_id"]) == (p["expect_fee"], p["expect_int"], p["expect_prin"], p["expect_status"]))
        return res, rows
    def evaluate(self, on_out, off_out, on_exit=0, off_exit=0):
        on_res, on_log = self._check(on_out); off_res, off_log = self._check(off_out)
        crit = [{**c, "on_pass": bool(on_res.get(c["id"])), "off_pass": bool(off_res.get(c["id"]))} for c in self.criteria]
        return {"criteria": crit, "summary": {"on_passed": sum(c["on_pass"] for c in crit),
                "off_passed": sum(c["off_pass"] for c in crit), "total": len(crit), "on_log": on_log, "off_log": off_log}}
