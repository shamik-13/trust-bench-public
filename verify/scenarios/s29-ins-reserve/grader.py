#!/usr/bin/env python3
"""Grader for the s29 ins-reserve behavioral test (D-2901). Criteria from fixture.json: each
calculable policy's 解約返戻金 (CV) and calc-status must equal the GOLDEN value (120ヶ月 amortisation
of 新契約費). Parses DUMPCV output: CVAL|pol-no|cv-yen|calc-status. GOLDEN passes all; START
(60ヶ月 basis) fails policies whose 経過月数 < 120 (the un-amortised charge differs from the
120-basis GOLDEN -> wrong CV), unless the CV floors to 0 in both arms.
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
        if ln.startswith("CVAL|"):
            parts = (ln.split("|") + ["", "", ""])[:4]
            _, pol, cv, status = parts
            try:
                v = int(cv.strip() or "-1")
            except ValueError:
                v = -1
            rows[pol.strip()] = (v, status.strip())
    return rows


class Grader:
    criteria = [
        {"id": f"{p['pol_no'][-4:]}-{p['expect_status']}",
         "label": (f"policy …{p['pol_no'][-4:]} 責準 ¥{p['reserve']:,} 新契約費 ¥{p['cost']:,} "
                   f"経過 {p['elapsed']}ヶ月 -> 解約返戻金 ¥{p['expect_cv']:,} / {p['expect_status']}"),
         "expected": (f"CV={p['expect_cv']} {p['expect_status']}")}
        for p in FIX["policies"]
    ]

    def _check(self, output):
        rows = _parse(output)
        res = {}
        for p in FIX["policies"]:
            cid = f"{p['pol_no'][-4:]}-{p['expect_status']}"
            got = rows.get(p["pol_no"])
            res[cid] = (got == (p["expect_cv"], p["expect_status"]))
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
