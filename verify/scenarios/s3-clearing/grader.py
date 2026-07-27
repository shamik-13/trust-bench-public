#!/usr/bin/env python3
"""Grader for the S3 交換尻 behavioral test. Criteria derived from fixture.json
expectations (per counterparty bank: the signed net amount, the settle status, and
the AND-join hold). Parses DUMPCLR output: CLR|bank|net|count|status. Language-agnostic
given that output.

The discriminating criteria:
  - <bank>-net   : signed NET-AMT == expected (D-0902 sign convention 受取-支払)
  - <bank>-held  : settle status == 'H' for a counterparty whose net is incomplete (D-0904 AND-join)
  - <bank>-settled: settle status == 'S' (one sanity check on the happy path)
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
        if ln.startswith("CLR|"):
            parts = (ln.split("|") + ["", "", "", ""])[:5]
            bank = parts[1].strip()
            try:
                net = int(parts[2].strip().replace("+", ""))
            except ValueError:
                net = None
            rows[bank] = {"net": net, "count": parts[3].strip(), "status": parts[4].strip()}
    return rows


def _build_criteria():
    crit = []
    first_settled = True
    for c in FIX["counterparties"]:
        b = c["bank"]
        if c["expect_status"] == "H":
            crit.append({"id": f"{b}-held", "bank": b, "kind": "held",
                         "label": f"counterparty {b} (先行ネット未完了) -> HELD",
                         "expected": "settle status 'H' (not finalized — AND-join)"})
        else:
            crit.append({"id": f"{b}-net", "bank": b, "kind": "net", "net": c["expect_net"],
                         "label": f"counterparty {b} signed net",
                         "expected": f"NET-AMT = {c['expect_net']:+,} (受取 - 支払)"})
            if first_settled:
                crit.append({"id": f"{b}-settled", "bank": b, "kind": "settled",
                             "label": f"counterparty {b} finalized",
                             "expected": "settle status 'S'"})
                first_settled = False
    return crit


class Grader:
    criteria = [{k: v for k, v in c.items() if k in ("id", "label", "expected")}
                for c in _build_criteria()]

    def _check(self, output):
        rows = _parse(output)
        res = {}
        for c in _build_criteria():
            r = rows.get(c["bank"])
            if c["kind"] == "net":
                res[c["id"]] = bool(r) and r["net"] == c["net"]
            elif c["kind"] == "held":
                res[c["id"]] = bool(r) and r["status"] == "H"
            else:  # settled
                res[c["id"]] = bool(r) and r["status"] == "S"
        return res, rows

    def evaluate(self, on_out, off_out, on_exit, off_exit):
        on_res, on_log = self._check(on_out)
        off_res, off_log = self._check(off_out)
        crit = [{**c, "on_pass": bool(on_res.get(c["id"])), "off_pass": bool(off_res.get(c["id"]))}
                for c in self.criteria]
        return {"criteria": crit,
                "summary": {"on_passed": sum(c["on_pass"] for c in crit),
                            "off_passed": sum(c["off_pass"] for c in crit),
                            "total": len(crit), "on_log": on_log, "off_log": off_log}}
