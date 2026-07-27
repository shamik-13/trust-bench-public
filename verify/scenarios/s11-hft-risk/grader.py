#!/usr/bin/env python3
"""Grader for s11-hft-risk / pre-trade risk gate (SEC1, behavioral).

The engine prints one `<order_id> <decision_code>` line per order. Criteria are the
golden-expected decision per fixture order; each criterion is checked independently on the
golden and start outputs (same shape as the COBOL graders so the gate selftest prints).
"""
from __future__ import annotations


class Grader:
    # golden-expected decision code per order (see fixture/fixture.json)
    EXPECT = {"ORDA": 4, "ORDB": 8, "ORDC": 0, "ORDD": 4}
    LABEL = {
        "ORDA": "group-margin breach -> REJECT(4)  [D-SEC-001: group view]",
        "ORDB": "notional > per-order cap -> REJECT(8)  [D-SEC-001: cap]",
        "ORDC": "clean small order -> ACCEPT(0)",
        "ORDD": "low-limit customer over margin -> REJECT(4)",
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
        return {oid: (parsed.get(oid) == exp) for oid, exp in self.EXPECT.items()}

    def evaluate(self, on_out, off_out, on_exit=0, off_exit=0) -> dict:
        on, off = self._check(self._parse(on_out)), self._check(self._parse(off_out))
        criteria = [{
            "id": oid,
            "expected": self.LABEL[oid],
            "on_pass": on[oid],
            "off_pass": off[oid],
        } for oid in self.EXPECT]
        return {"criteria": criteria, "summary": {
            "on_passed": sum(c["on_pass"] for c in criteria),
            "off_passed": sum(c["off_pass"] for c in criteria),
            "total": len(criteria)}}
