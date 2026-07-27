#!/usr/bin/env python3
"""Grader for the s28 ins-premium behavioral test (D-2801). Criteria from fixture.json: each
in-force policy's monthly premium (月額保険料), calc-status, and applied 年齢帯 must equal the GOLDEN
value (改定後 月額保険料率表). Parses DUMPPRM output: PRMR|pol-no|prem-yen|calc-status|band.
GOLDEN passes all; START (OLD rate table) fails every in-force case (wrong rate -> wrong premium).
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
        if ln.startswith("PRMR|"):
            parts = (ln.split("|") + ["", "", "", ""])[:5]
            _, pol, prem, status, band = parts
            try:
                pr = int(prem.strip() or "-1")
            except ValueError:
                pr = -1
            rows[pol.strip()] = (pr, status.strip(), band.strip())
    return rows


class Grader:
    criteria = [
        {"id": f"{p['pol_no'][-4:]}-{p['expect_status']}",
         "label": (f"policy …{p['pol_no'][-4:]} age {p['age']}/{p['sex']} 保険金額 ¥{p['sum_assured']:,} "
                   f"-> 月額保険料 ¥{p['expect_prem']:,} / {p['expect_status']}/{p['expect_band'] or '-'}"),
         "expected": (f"PREM={p['expect_prem']} {p['expect_status']}/{p['expect_band'] or '-'}")}
        for p in FIX["policies"]
    ]

    def _check(self, output):
        rows = _parse(output)
        res = {}
        for p in FIX["policies"]:
            cid = f"{p['pol_no'][-4:]}-{p['expect_status']}"
            got = rows.get(p["pol_no"])
            res[cid] = (got == (p["expect_prem"], p["expect_status"], p["expect_band"]))
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
