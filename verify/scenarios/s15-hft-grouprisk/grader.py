#!/usr/bin/env python3
"""Grader for s11-hft-risk / group-risk aggregation gate (SEC6, D-SEC-007, behavioral).

GroupRiskService prints one `<cif> <gross_exposure>` line per customer. Criteria are the
golden-expected GROSS exposure per fixture customer (|net_qty|*price summed, longs and
shorts NOT netted). Same shape as grader.py so the gate selftest prints identically.
"""
from __future__ import annotations


class Grader:
    # golden-expected GROSS exposure per customer (see fixture/sec6_fixture.json)
    EXPECT = {"CUST01": 8200000, "CUST02": 1000000}
    LABEL = {
        "CUST01": "long+short -> GROSS 8.2M  [D-SEC-007: no net-off]",
        "CUST02": "long-only control -> 1.0M",
    }

    @staticmethod
    def _parse(out: str) -> dict:
        d = {}
        for line in (out or "").splitlines():
            parts = line.split()
            if len(parts) == 2 and parts[1].lstrip("-").isdigit():
                d[parts[0]] = int(parts[1])
        return d

    def _check(self, parsed: dict) -> dict:
        return {cif: (parsed.get(cif) == exp) for cif, exp in self.EXPECT.items()}

    def evaluate(self, on_out, off_out, on_exit=0, off_exit=0) -> dict:
        on, off = self._check(self._parse(on_out)), self._check(self._parse(off_out))
        criteria = [{
            "id": cif,
            "expected": self.LABEL[cif],
            "on_pass": on[cif],
            "off_pass": off[cif],
        } for cif in self.EXPECT]
        return {"criteria": criteria, "summary": {
            "on_passed": sum(c["on_pass"] for c in criteria),
            "off_passed": sum(c["off_pass"] for c in criteria),
            "total": len(criteria)}}
