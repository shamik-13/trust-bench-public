#!/usr/bin/env python3
"""Grader for the s21 card-revolving behavioral test. Criteria from fixture.json: each account's
monthly principal (元金定額), total payment, statement status, and applied slide tier must equal the
GOLDEN value (改定後 残高スライド表). Parses DUMPSLD output: SLDE|card-no|prin-yen|pay-yen|status|tier.
Language-agnostic given that output. GOLDEN passes all; START (OLD coarse table) fails the active
cases (wrong principal -> wrong pay + tier).
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
        if ln.startswith("SLDE|"):
            parts = (ln.split("|") + ["", "", "", "", "", ""])[:6]
            _, card, prin, pay, status, tier = parts
            try:
                pr = int(prin.strip() or "-1")
            except ValueError:
                pr = -1
            try:
                py = int(pay.strip() or "-1")
            except ValueError:
                py = -1
            rows[card.strip()] = (pr, py, status.strip(), tier.strip())
    return rows


class Grader:
    criteria = [
        {"id": f"{b['card_no'][-4:]}-{b['expect_status']}",
         "label": (f"card …{b['card_no'][-4:]} rev ¥{b['rev_bal']:,} -> prin ¥{b['expect_prin']:,} "
                   f"pay ¥{b['expect_pay']:,} / {b['expect_status']}/{b['expect_tier'] or '-'}"),
         "expected": (f"PRIN={b['expect_prin']} PAY={b['expect_pay']} "
                      f"{b['expect_status']}/{b['expect_tier'] or '-'}")}
        for b in FIX["balances"]
    ]

    def _check(self, output):
        rows = _parse(output)
        res = {}
        for b in FIX["balances"]:
            cid = f"{b['card_no'][-4:]}-{b['expect_status']}"
            got = rows.get(b["card_no"])
            res[cid] = (got == (b["expect_prin"], b["expect_pay"],
                                b["expect_status"], b["expect_tier"]))
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
