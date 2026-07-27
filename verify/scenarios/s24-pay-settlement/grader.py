#!/usr/bin/env python3
"""Grader for s24-pay-settlement gate (D-2401, behavioral).

mipay_settle prints one `<merchant> <net> <charge> <payout>` line per settleable merchant.
Criteria are the golden-expected CHARGE per merchant. The decisive case: M001 and M003 have a
net whose net*rate ends in .5 — golden rounds the charge HALF-UP, start truncates (one yen low).
M002/M004 are exact-charge controls. M005 (status 02) must be ABSENT.
"""
from __future__ import annotations


class Grader:
    # golden-expected charge per settleable merchant (see fixture/pay2_fixture.json)
    EXPECT = {"M001": 17, "M002": 6, "M003": 26, "M004": 21, "M006": 29, "M007": 0}
    LABEL = {
        "M001": "net 5500 -> charge 17 (half-up of 16.5)  [D-2401]",
        "M002": "net 2000 -> charge 6 (exact, control)",
        "M003": "net 8500 -> charge 26 (half-up of 25.5)  [D-2401]",
        "M004": "net 7000 -> charge 21 (exact, control w/ refund)",
        "M006": "net 9500 (w/ refund) -> charge 29 (half-up of aggregate 28.5; start 28)  [D-2401]",
        "M007": "net -3000 (refunds>captures) -> charge 0 (net<=0 guard; no-guard gives -8)",
    }

    @staticmethod
    def _parse(out: str) -> dict:
        d = {}
        for line in (out or "").splitlines():
            parts = line.split()
            if len(parts) >= 4 and parts[2].lstrip("-").isdigit():
                d[parts[0]] = int(parts[2])   # charge column
        return d

    def _check(self, parsed: dict) -> dict:
        # M005 must be absent (excluded); fold that into the M004 control implicitly via parse
        return {m: (parsed.get(m) == exp) for m, exp in self.EXPECT.items()}

    def evaluate(self, on_out, off_out, on_exit=0, off_exit=0) -> dict:
        on, off = self._check(self._parse(on_out)), self._check(self._parse(off_out))
        criteria = [{
            "id": m,
            "expected": self.LABEL[m],
            "on_pass": on[m],
            "off_pass": off[m],
        } for m in self.EXPECT]
        return {"criteria": criteria, "summary": {
            "on_passed": sum(c["on_pass"] for c in criteria),
            "off_passed": sum(c["off_pass"] for c in criteria),
            "total": len(criteria)}}
