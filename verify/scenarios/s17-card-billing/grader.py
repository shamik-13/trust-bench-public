#!/usr/bin/env python3
"""Grader for the s17 card-billing behavioral test. Criteria from fixture.json: each card's
statement minimum payment must equal the GOLDEN value (max(truncate(closing*0.05), 2000)) and
its bill status must match. Parses DUMPBILL output: BILL|card-no|min-pay-yen|bill-status.
Language-agnostic given that output. GOLDEN passes all; START (3%/no-floor) fails the billable
cases.
"""
from __future__ import annotations
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())


def _parse(output):
    rows = {}
    for ln in (output or "").splitlines():
        ln = ln.strip()
        if ln.startswith("BILL|"):
            parts = (ln.split("|") + ["", "", ""])[:4]
            _, card, minpay, status = parts
            try:
                mp = int(minpay.strip() or "0")
            except ValueError:
                mp = -1
            rows[card.strip()] = (mp, status.strip())
    return rows


class Grader:
    criteria = [
        {"id": f"{b['card_no'][-4:]}-{b['expect_status']}",
         "label": (f"card …{b['card_no'][-4:]} closing ¥{b['closing']:,} -> "
                   f"min ¥{b['expect_min']:,} / {b['expect_status']}"),
         "expected": f"MIN-PAY={b['expect_min']} STATUS={b['expect_status']}"}
        for b in FIX["balances"]
    ]

    def _check(self, output):
        rows = _parse(output)
        res = {}
        for b in FIX["balances"]:
            cid = f"{b['card_no'][-4:]}-{b['expect_status']}"
            mp, st = rows.get(b["card_no"], (None, None))
            res[cid] = (mp == b["expect_min"] and st == b["expect_status"])
        return res, rows

    def evaluate(self, on_out, off_out, on_exit, off_exit):
        on_res, on_log = self._check(on_out)
        off_res, off_log = self._check(off_out)
        crit = [{**c, "on_pass": bool(on_res.get(c["id"])), "off_pass": bool(off_res.get(c["id"]))}
                for c in self.criteria]
        return {"criteria": crit,
                "summary": {"on_passed": sum(c["on_pass"] for c in crit),
                            "off_passed": sum(c["off_pass"] for c in crit),
                            "total": len(crit),
                            "on_log": {k: v for k, v in on_log.items()},
                            "off_log": {k: v for k, v in off_log.items()}}}
