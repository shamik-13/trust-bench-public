#!/usr/bin/env python3
"""Grader for the s32 common-cif behavioral test (D-3201). Criteria from fixture.json: each keyable
customer's 名寄せ統合キー 検査数字 (check digit) and key-status must equal the GOLDEN value (modulus-11
weights 2..7). Parses DUMPKEY output: KEY|cif-no|check-digit|key-status. GOLDEN passes all; START
(modulus-10 Luhn) fails every customer whose modulus-11 check digit differs from the Luhn one.
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
        if ln.startswith("KEY|"):
            parts = (ln.split("|") + ["", "", ""])[:4]
            _, cif, cd, status = parts
            try:
                v = int(cd.strip() or "-1")
            except ValueError:
                v = -1
            rows[cif.strip()] = (v, status.strip())
    return rows


class Grader:
    criteria = [
        {"id": f"{c['cif_no']}-{c['expect_status']}",
         "label": (f"CIF {c['cif_no']} 状態 {c['status']} -> 検査数字 {c['expect_cd']} / "
                   f"{c['expect_status']}"),
         "expected": (f"CD={c['expect_cd']} {c['expect_status']}")}
        for c in FIX["customers"]
    ]

    def _check(self, output):
        rows = _parse(output)
        res = {}
        for c in FIX["customers"]:
            cid = f"{c['cif_no']}-{c['expect_status']}"
            got = rows.get(c["cif_no"])
            res[cid] = (got == (c["expect_cd"], c["expect_status"]))
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
