#!/usr/bin/env python3
"""Grader for the s19 card-capture behavioral test. DUMPCAP prints
CAP|sale-id|billed-yen|fee-yen|status. Each sale's (billed, fee, status) must equal the GOLDEN
value (foreign-usage fee = trunc(amount*0.022)). Foreign sales discriminate START (1.60%); JPY
sales (fee 0) and the skipped card are controls.
"""
from __future__ import annotations
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())


def _parse(out):
    rows = {}
    for ln in (out or "").splitlines():
        ln = ln.strip()
        if ln.startswith("CAP|"):
            p = (ln.split("|") + ["", "", "", ""])[:5]
            try:
                rows[p[1].strip()] = (int(p[2].strip() or "-1"), int(p[3].strip() or "-1"), p[4].strip())
            except ValueError:
                rows[p[1].strip()] = (-1, -1, p[4].strip())
    return rows


class Grader:
    criteria = [
        {"id": s["sale_id"][-3:],
         "label": f"{s['sale_id']} {s['currency']} ¥{s['amount']:,} -> billed ¥{s['expect_billed']:,} fee ¥{s['expect_fee']:,}/{s['expect_status']}",
         "expected": f"billed={s['expect_billed']} fee={s['expect_fee']} {s['expect_status']}"}
        for s in FIX["sales"]
    ]

    def _check(self, out):
        rows = _parse(out)
        res = {}
        for s in FIX["sales"]:
            cid = s["sale_id"][-3:]
            got = rows.get(s["sale_id"])
            res[cid] = (got == (s["expect_billed"], s["expect_fee"], s["expect_status"]))
        return res, rows

    def evaluate(self, on_out, off_out, on_exit=0, off_exit=0):
        on_res, on_log = self._check(on_out)
        off_res, off_log = self._check(off_out)
        crit = [{**c, "on_pass": bool(on_res.get(c["id"])), "off_pass": bool(off_res.get(c["id"]))}
                for c in self.criteria]
        return {"criteria": crit,
                "summary": {"on_passed": sum(c["on_pass"] for c in crit),
                            "off_passed": sum(c["off_pass"] for c in crit),
                            "total": len(crit), "on_log": on_log, "off_log": off_log}}
