#!/usr/bin/env python3
"""Grader for the s33 common-calendar behavioral test (D-3301). Criteria from fixture.json: each
processable instruction's 資金受渡日 (value date) and status must equal the GOLDEN value (翌々営業日 =
2営業日後, skipping 休業日). Parses DUMPVAL output: VAL|fct-id|value-dt|val-status. GOLDEN passes all;
START (翌営業日 = 1営業日後) fails every processable instruction (value date one business day too early).
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
        if ln.startswith("VAL|"):
            parts = (ln.split("|") + ["", "", ""])[:4]
            _, fct, dt, status = parts
            dt = dt.strip().lstrip("0") or "0"
            rows[fct.strip()] = (dt, status.strip())
    return rows


def _norm(dt):
    return (str(dt).strip().lstrip("0") or "0")


class Grader:
    criteria = [
        {"id": f"{i['fct_id']}-{i['expect_status']}",
         "label": (f"指図 {i['fct_id']} 指図日 {i['trigger']} 状態 {i['status']} -> "
                   f"受渡日 {i['expect_value_dt']} / {i['expect_status']}"),
         "expected": (f"VAL-DT={i['expect_value_dt']} {i['expect_status']}")}
        for i in FIX["instructions"]
    ]

    def _check(self, output):
        rows = _parse(output)
        res = {}
        for i in FIX["instructions"]:
            cid = f"{i['fct_id']}-{i['expect_status']}"
            got = rows.get(i["fct_id"])
            want = (_norm(i["expect_value_dt"]), i["expect_status"])
            res[cid] = (got == want)
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
