#!/usr/bin/env python3
"""Grader for s25-pay-fee gate (D-2501, behavioral).

MdrFeeEngine prints one `<txnId> <fee>` line per chargeable transaction. Criteria are the
golden(改定後)-expected MDR fee per transaction. The decisive cases T1..T5 (amount 100000) differ
between the 改定後 table (golden) and the 旧 table (start) for every category; T6 is a small-amount
control where both floor to the same fee; T7 (status 02) is excluded.
"""
from __future__ import annotations


class Grader:
    # golden(改定後)-expected fee per transaction (see fixture/pay3_fixture.json)
    EXPECT = {"T1": 3000, "T2": 3240, "T3": 1300, "T4": 3740, "T5": 4000, "T6": 3}
    LABEL = {
        "T1": "C1 100000 -> 3000 (3.00%)  [D-2501]",
        "T2": "C2 100000 -> 3240 (3.24%)  [D-2501]",
        "T3": "C3 100000 -> 1300 (1.30%)  [D-2501]",
        "T4": "C4 100000 -> 3740 (3.74%)  [D-2501]",
        "T5": "C5 100000 -> 4000 (4.00%)  [D-2501]",
        "T6": "C2 100 -> 3 (control, both tables floor to 3)",
    }

    @staticmethod
    def _parse(out: str) -> dict:
        d = {}
        for line in (out or "").splitlines():
            parts = line.split()
            if len(parts) >= 2 and parts[1].lstrip("-").isdigit():
                d[parts[0]] = int(parts[1])
        return d

    def _check(self, parsed: dict) -> dict:
        return {t: (parsed.get(t) == exp) for t, exp in self.EXPECT.items()}

    def evaluate(self, on_out, off_out, on_exit=0, off_exit=0) -> dict:
        on, off = self._check(self._parse(on_out)), self._check(self._parse(off_out))
        criteria = [{
            "id": t,
            "expected": self.LABEL[t],
            "on_pass": on[t],
            "off_pass": off[t],
        } for t in self.EXPECT]
        return {"criteria": criteria, "summary": {
            "on_passed": sum(c["on_pass"] for c in criteria),
            "off_passed": sum(c["off_pass"] for c in criteria),
            "total": len(criteria)}}
