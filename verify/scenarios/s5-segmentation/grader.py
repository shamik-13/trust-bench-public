#!/usr/bin/env python3
"""Grader for the S5 segment-assignment behavioral test. Criterion per customer: the
assigned SEG-CD must match the GOLDEN expectation (full 5-band thresholds D-1102 +
equal-value tie-break D-1103). Parses DUMPSEG output: SEG|cust|seg-cd.
"""
from __future__ import annotations
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())


def _parse(output):
    seg = {}
    for ln in (output or "").splitlines():
        ln = ln.strip()
        if ln.startswith("SEG|"):
            p = (ln.split("|") + ["", ""])[:3]
            seg[p[1].strip()] = p[2].strip()
    return seg


class Grader:
    criteria = [{"id": f"{c['cust']}-{c['expect_seg']}",
                 "label": f"customer {c['cust']} (avg {c['avg_bal']:,})",
                 "expected": f"segment {c['expect_seg']}"} for c in FIX["customers"]]

    def _check(self, output):
        seg = _parse(output)
        return ({f"{c['cust']}-{c['expect_seg']}": seg.get(c["cust"]) == c["expect_seg"]
                 for c in FIX["customers"]}, seg)

    def evaluate(self, on_out, off_out, on_exit, off_exit):
        on_res, on_log = self._check(on_out)
        off_res, off_log = self._check(off_out)
        crit = [{**c, "on_pass": bool(on_res.get(c["id"])), "off_pass": bool(off_res.get(c["id"]))}
                for c in self.criteria]
        return {"criteria": crit,
                "summary": {"on_passed": sum(c["on_pass"] for c in crit),
                            "off_passed": sum(c["off_pass"] for c in crit),
                            "total": len(crit), "on_log": on_log, "off_log": off_log}}
