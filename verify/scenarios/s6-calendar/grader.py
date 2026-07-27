#!/usr/bin/env python3
"""Grader for the S6 cycle-boundary/holiday behavioral test. Criterion per cycle: the
resolved basis date must match the GOLDEN expectation (holiday roll BACKWARD to the
previous business day, D-1202). Parses DUMPCYR output: CYR|cycle|resolved-dt|rolled-flag.
"""
from __future__ import annotations
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())


def _parse(output):
    res = {}
    for ln in (output or "").splitlines():
        ln = ln.strip()
        if ln.startswith("CYR|"):
            p = (ln.split("|") + ["", "", ""])[:4]
            try:
                rd = int(p[2].strip())
            except ValueError:
                rd = None
            res[p[1].strip()] = {"resolved": rd, "rolled": p[3].strip()}
    return res


class Grader:
    criteria = [{"id": f"{c['cycle']}-resolved",
                 "label": f"cycle {c['cycle']} (nominal {c['nominal']})",
                 "expected": f"resolved {c['expect_resolved']} (rolled {c['expect_rolled']})"}
                for c in FIX["cycles"]]

    def _check(self, output):
        got = _parse(output)
        res = {}
        for c in FIX["cycles"]:
            r = got.get(c["cycle"])
            res[f"{c['cycle']}-resolved"] = bool(r) and r["resolved"] == c["expect_resolved"] \
                and r["rolled"] == c["expect_rolled"]
        return res, got

    def evaluate(self, on_out, off_out, on_exit, off_exit):
        on_res, on_log = self._check(on_out)
        off_res, off_log = self._check(off_out)
        crit = [{**c, "on_pass": bool(on_res.get(c["id"])), "off_pass": bool(off_res.get(c["id"]))}
                for c in self.criteria]
        return {"criteria": crit,
                "summary": {"on_passed": sum(c["on_pass"] for c in crit),
                            "off_passed": sum(c["off_pass"] for c in crit),
                            "total": len(crit), "on_log": on_log, "off_log": off_log}}
