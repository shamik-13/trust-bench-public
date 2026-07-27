#!/usr/bin/env python3
"""Grader for s11-hft-risk / matching priority gate (SEC2, D-SEC-002, behavioral).

mihft_match prints one `<resting_id> <filled_qty>` line per resting order (arrival order).
Criteria are the golden-expected fill per resting order under price-time priority (FIFO at
equal price). See fixture/match_book.csv.
"""
from __future__ import annotations


class Grader:
    # FIFO: incoming 120 fills R1(50) then R2(70); R3 untouched.
    EXPECT = {"R1": 50, "R2": 70, "R3": 0}
    LABEL = {
        "R1": "earliest arrival fills first -> 50  [D-SEC-002: FIFO not size]",
        "R2": "next arrival partial -> 70  [D-SEC-002: FIFO not size]",
        "R3": "untouched -> 0 (control)",
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
        return {k: (parsed.get(k) == exp) for k, exp in self.EXPECT.items()}

    def evaluate(self, on_out, off_out, on_exit=0, off_exit=0) -> dict:
        on, off = self._check(self._parse(on_out)), self._check(self._parse(off_out))
        criteria = [{
            "id": k, "expected": self.LABEL[k],
            "on_pass": on[k], "off_pass": off[k],
        } for k in self.EXPECT]
        return {"criteria": criteria, "summary": {
            "on_passed": sum(c["on_pass"] for c in criteria),
            "off_passed": sum(c["off_pass"] for c in criteria),
            "total": len(criteria)}}
