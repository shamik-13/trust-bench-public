#!/usr/bin/env python3
"""Grader for the S4 credit-limit behavioral test — two engines, combined criteria.
CREDIT (KZ150B -> KZCRLF, parsed from CRL|acct|raise|kyc|limit): each account's
RAISE-FLAG must match the GOLDEN expectation (KYC gate D-1002).
EXPOSURE (KZ160B -> KZEXPRF, parsed from EXP|cust|product|over|capped): each exposure's
OVER-FLAG must match (per-product cap D-1005). Language-agnostic given that output.
"""
from __future__ import annotations
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIX = json.loads((HERE / "fixture" / "fixture.json").read_text())


def _parse(output):
    crl, exp = {}, {}
    for ln in (output or "").splitlines():
        ln = ln.strip()
        if ln.startswith("CRL|"):
            p = (ln.split("|") + ["", "", "", ""])[:5]
            crl[p[1].strip()] = {"raise": p[2].strip(), "kyc": p[3].strip(), "limit": p[4].strip()}
        elif ln.startswith("EXP|"):
            p = (ln.split("|") + ["", "", "", ""])[:5]
            exp[(p[1].strip(), p[2].strip())] = {"over": p[3].strip(), "capped": p[4].strip()}
    return crl, exp


def _build_criteria():
    crit = []
    for a in FIX["accounts"]:
        crit.append({"id": f"{a['acct']}-raise", "kind": "raise", "key": a["acct"], "want": a["expect_raise"],
                     "label": f"account {a['acct']} (cust {a['cust']})",
                     "expected": f"RAISE-FLAG '{a['expect_raise']}'"
                                 + (" (KYC gate: held)" if a["expect_raise"] == "N" and a["avg_bal"] >= 5000000 else "")})
        crit.append({"id": f"{a['acct']}-limit", "kind": "limit", "key": a["acct"],
                     "want": int(a["expect_new_limit"]),
                     "label": f"account {a['acct']} new limit",
                     "expected": f"NEW-LIMIT {int(a['expect_new_limit']):,}"})
    for e in FIX["exposures"]:
        crit.append({"id": f"{e['cust']}-{e['product']}-over", "kind": "over", "key": (e["cust"], e["product"]),
                     "want": e["expect_over"], "label": f"exposure {e['cust']}/{e['product']}",
                     "expected": f"OVER-FLAG '{e['expect_over']}'"
                                 + (" (per-product cap)" if e["expect_over"] == "Y" else "")})
        crit.append({"id": f"{e['cust']}-{e['product']}-capped", "kind": "capped", "key": (e["cust"], e["product"]),
                     "want": int(e["expect_capped"]),
                     "label": f"exposure {e['cust']}/{e['product']} capped amt",
                     "expected": f"CAPPED-AMT {int(e['expect_capped']):,}"})
    return crit


class Grader:
    criteria = [{k: v for k, v in c.items() if k in ("id", "label", "expected")} for c in _build_criteria()]

    def _check(self, output):
        crl, exp = _parse(output)
        res = {}
        for c in _build_criteria():
            if c["kind"] == "raise":
                r = crl.get(c["key"])
                res[c["id"]] = bool(r) and r["raise"] == c["want"]
            elif c["kind"] == "limit":
                r = crl.get(c["key"])
                res[c["id"]] = bool(r) and r["limit"].isdigit() and int(r["limit"]) == c["want"]
            elif c["kind"] == "capped":
                r = exp.get(c["key"])
                res[c["id"]] = bool(r) and r["capped"].isdigit() and int(r["capped"]) == c["want"]
            else:  # over
                r = exp.get(c["key"])
                res[c["id"]] = bool(r) and r["over"] == c["want"]
        return res, {"crl": crl, "exp": exp}

    def evaluate(self, on_out, off_out, on_exit, off_exit):
        on_res, on_log = self._check(on_out)
        off_res, off_log = self._check(off_out)
        crit = [{**c, "on_pass": bool(on_res.get(c["id"])), "off_pass": bool(off_res.get(c["id"]))}
                for c in self.criteria]
        return {"criteria": crit,
                "summary": {"on_passed": sum(c["on_pass"] for c in crit),
                            "off_passed": sum(c["off_pass"] for c in crit),
                            "total": len(crit), "on_log": on_log, "off_log": off_log}}
