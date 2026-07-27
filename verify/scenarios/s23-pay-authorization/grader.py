#!/usr/bin/env python3
"""Grader for s23-pay-authorization gate (D-2301, behavioral).

AuthEngine prints one `<reqId> <decision> <avail> <reason>` line per request. Criteria are the
golden-expected DECISION (A/D) per fixture request. The decisive case: W1 requests (R1, R5) are
APPROVED by golden (the expired hold is released) but DECLINED by start (it subtracts the expired
hold). R2/R3/R4 are controls. Same shape as the securities graders.
"""
from __future__ import annotations


class Grader:
    # golden-expected decision per request (see fixture/pay1_fixture.json)
    EXPECT = {"R1": "A", "R5": "A", "R2": "A", "R3": "D", "R4": "D", "R6": "A", "R7": "D"}
    LABEL = {
        "R1": "W1 6000 -> APPROVE (expired hold released; avail 6000)  [D-2301]",
        "R5": "W1 5000 -> APPROVE (expired hold released; avail 6000)  [D-2301]",
        "R2": "W2 3000 -> APPROVE (control, avail 3500)",
        "R3": "W3 5000 -> DECLINE/LIM (control, avail 1500)",
        "R4": "W4 1000 -> DECLINE/STS (control, suspended)",
        "R6": "W5 6000 -> APPROVE (avail 6000: as-of hold counts, USD/expired excluded; START declines)",
        "R7": "W5 8000 -> DECLINE/LIM (avail 6000; a `<=` release of the as-of hold would approve)",
    }

    @staticmethod
    def _parse(out: str) -> dict:
        d = {}
        for line in (out or "").splitlines():
            parts = line.split()
            if len(parts) >= 2 and parts[1] in ("A", "D"):
                d[parts[0]] = parts[1]
        return d

    def _check(self, parsed: dict) -> dict:
        return {rid: (parsed.get(rid) == exp) for rid, exp in self.EXPECT.items()}

    def evaluate(self, on_out, off_out, on_exit=0, off_exit=0) -> dict:
        on, off = self._check(self._parse(on_out)), self._check(self._parse(off_out))
        criteria = [{
            "id": rid,
            "expected": self.LABEL[rid],
            "on_pass": on[rid],
            "off_pass": off[rid],
        } for rid in self.EXPECT]
        return {"criteria": criteria, "summary": {
            "on_passed": sum(c["on_pass"] for c in criteria),
            "off_passed": sum(c["off_pass"] for c in criteria),
            "total": len(criteria)}}
