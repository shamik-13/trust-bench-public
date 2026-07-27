#!/usr/bin/env python3
"""Grader for s26-pay-refund gate (D-2601, behavioral).

RefundEngine prints one `<reqId> <decision> <reason>` line per request. Criteria are the
golden(改定後 180日)-expected DECISION (A/D) per request. The decisive cases R2/R3 fall in the
(90, 180]-day window: golden ACCEPTS, start (旧 90日) DECLINES. R1/R4/R5/R6 are controls.
"""
from __future__ import annotations


class Grader:
    # golden-expected decision per request (see fixture/pay4_fixture.json)
    # R7/R8 pin the window to EXACTLY 180: 180d must ACCEPT (window>=180) and 181d must
    # DECLINE (window<=180), so an over-extended fix (e.g. 200d) fails R8 and an under fix fails R7.
    EXPECT = {"R1": "A", "R2": "A", "R3": "A", "R4": "D", "R5": "D", "R6": "D",
              "R7": "A", "R8": "D"}
    LABEL = {
        "R1": "59d -> ACCEPT (control, within both windows)",
        "R2": "120d -> ACCEPT (改定後 180日; start declines)  [D-2601]",
        "R3": "165d -> ACCEPT (改定後 180日; start declines)  [D-2601]",
        "R4": "273d -> DECLINE/WIN (control, over both)",
        "R5": "refund>orig -> DECLINE/AMT (control)",
        "R6": "no original -> DECLINE/TXN (control)",
        "R7": "180d (exact boundary) -> ACCEPT (改定後 180日 inclusive; pins window>=180)  [D-2601]",
        "R8": "181d (just over) -> DECLINE/WIN (pins window<=180; over-extended fix fails)  [D-2601]",
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
        return {r: (parsed.get(r) == exp) for r, exp in self.EXPECT.items()}

    def evaluate(self, on_out, off_out, on_exit=0, off_exit=0) -> dict:
        on, off = self._check(self._parse(on_out)), self._check(self._parse(off_out))
        criteria = [{
            "id": r,
            "expected": self.LABEL[r],
            "on_pass": on[r],
            "off_pass": off[r],
        } for r in self.EXPECT]
        return {"criteria": criteria, "summary": {
            "on_passed": sum(c["on_pass"] for c in criteria),
            "off_passed": sum(c["off_pass"] for c in criteria),
            "total": len(criteria)}}
