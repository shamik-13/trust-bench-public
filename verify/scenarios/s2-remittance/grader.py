#!/usr/bin/env python3
"""Grader for the S2 inbound-reject behavioral test. Criteria derived from
fixture.json expectations (reject code per remittance, or ACCEPT/ROUTE = not logged).
Parses DUMPREJ output: REJ|center-seq|reason. Language-agnostic given that output.
"""
from __future__ import annotations
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())


def _parse(output):
    logged = {}
    for ln in (output or "").splitlines():
        ln = ln.strip()
        if ln.startswith("REJ|"):
            _, seq, reason = (ln.split("|") + ["", ""])[:3]
            logged[seq.strip()] = reason.strip()
    return logged


def _parse_out(output):
    """Parse DUMPOUT lines: OUT|orig-center-seq|correction-type|amt-yen|signature.
    Returns {seq: {"type": ..., "amt": int, "sig": str}}."""
    out = {}
    for ln in (output or "").splitlines():
        ln = ln.strip()
        if ln.startswith("OUT|"):
            parts = (ln.split("|") + ["", "", "", ""])[:5]
            _, seq, ctype, amt, sig = parts
            try:
                amt_i = int(amt.strip() or "0")
            except ValueError:
                amt_i = -1
            out[seq.strip()] = {"type": ctype.strip(), "amt": amt_i, "sig": sig.strip()}
    return out


CORR = FIX.get("corrections", [])


class Grader:
    criteria = [
        {"id": f"{m['seq']}-{m['expect']}",
         "label": f"remittance {m['seq']} (type {m['type']}) -> {m['expect']}",
         "expected": (f"logged {m['expect']}" if m["expect"] in ("N1", "A1", "A2", "F1", "L1", "S1")
                      else "NOT in reject log")}
        for m in FIX["remittances"] if m["expect"] != "ACCEPT" or m["seq"] in ("M08",)
    ] + [
        {"id": f"{m['seq']}-CORR",
         "label": f"correction {m['seq']} (訂正区分 {m['correction']['corr_type']})",
         "expected": ("signed TGOUTCF correction written" if m["correction"]["expect_written"]
                      else "rejected -> NO TGOUTCF record")}
        for m in CORR
    ]

    def _check(self, output):
        logged = _parse(output)
        out_recs = _parse_out(output)
        res = {}
        for m in FIX["remittances"]:
            seq, exp = m["seq"], m["expect"]
            cid = f"{seq}-{exp}"
            if exp in ("N1", "A1", "A2", "F1", "L1", "S1"):
                res[cid] = logged.get(seq) == exp
            else:  # ACCEPT / ROUTE -> must NOT be in the reject log
                res[cid] = seq not in logged
        for m in CORR:
            seq = m["seq"]
            cid = f"{seq}-CORR"
            rec = out_recs.get(seq)
            if m["correction"]["expect_written"]:
                # a signed correction record with the right 訂正区分 + amount must exist
                res[cid] = bool(rec) and rec["sig"] != "" \
                    and rec["type"] == m["correction"]["corr_type"] \
                    and rec["amt"] == int(m["amount"])
            else:  # invalid correction -> must NOT be written
                res[cid] = rec is None
        return res, logged

    def evaluate(self, on_out, off_out, on_exit, off_exit):
        on_res, on_log = self._check(on_out)
        off_res, off_log = self._check(off_out)
        crit = [{**c, "on_pass": bool(on_res.get(c["id"])), "off_pass": bool(off_res.get(c["id"]))}
                for c in self.criteria]
        return {"criteria": crit,
                "summary": {"on_passed": sum(c["on_pass"] for c in crit),
                            "off_passed": sum(c["off_pass"] for c in crit),
                            "total": len(crit), "on_log": on_log, "off_log": off_log}}
