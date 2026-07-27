#!/usr/bin/env python3
"""Grader for the S7 DWH-extract behavioral test. Criterion per fee row: it must appear in
the JHDWHF extract iff it is confirmed (FH-CONFIRM-FLAG='Y', D-1302). Parses DUMPDWH output:
DWH|acct (presence by account).
"""
from __future__ import annotations
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())


def _parse(output):
    present = set()
    for ln in (output or "").splitlines():
        ln = ln.strip()
        if ln.startswith("DWH|"):
            present.add(ln.split("|", 1)[1].strip())
    return present


class Grader:
    criteria = [{"id": f"{r['acct'][-2:]}-{'in' if r['expect_extracted'] else 'out'}",
                 "label": f"fee {r['acct'][-4:]} (confirm {r['confirm']})",
                 "expected": ("extracted (confirmed)" if r["expect_extracted"] else "NOT extracted (unconfirmed)")}
                for r in FIX["fees"]]

    def _check(self, output):
        present = _parse(output)
        res = {}
        for r in FIX["fees"]:
            cid = f"{r['acct'][-2:]}-{'in' if r['expect_extracted'] else 'out'}"
            res[cid] = (r["acct"] in present) == r["expect_extracted"]
        return res, sorted(present)

    def evaluate(self, on_out, off_out, on_exit, off_exit):
        on_res, on_log = self._check(on_out)
        off_res, off_log = self._check(off_out)
        crit = [{**c, "on_pass": bool(on_res.get(c["id"])), "off_pass": bool(off_res.get(c["id"]))}
                for c in self.criteria]
        return {"criteria": crit,
                "summary": {"on_passed": sum(c["on_pass"] for c in crit),
                            "off_passed": sum(c["off_pass"] for c in crit),
                            "total": len(crit), "on_log": on_log, "off_log": off_log}}
