#!/usr/bin/env python3
"""Grader for s31-ins-claims gate (D-3101, behavioral).

ClaimPayoutEngine prints one `<claimId> <payout>` line per payable claim. Criteria are the
golden(改定後 60%)-expected payout per claim. CLM01/CLM03/CLM06 (within 1 year) differ between the
改定後 60% (golden) and the 旧 75% (start). CLM02 (>=1 year -> 100%) and CLM04 (floored to 0) are
controls (both arms equal); CLM05 (status 09) is excluded from output.
"""
from __future__ import annotations


class Grader:
    # golden(改定後 60%)-expected payout per claim (see fixture/pay_fixture.json)
    EXPECT = {"CLM01": 6000000, "CLM02": 7500000, "CLM03": 2800000, "CLM04": 0,
              "CLM06": 11000000, "CLM07": 10000000, "CLM08": 0}
    LABEL = {
        "CLM01": "1年未満 60% 保険金1000万 -> 6,000,000  [D-3101]",
        "CLM02": "1年以上 100% 控除後 -> 7,500,000 (control)",
        "CLM03": "1年未満 60% 控除後 -> 2,800,000  [D-3101]",
        "CLM04": "1年未満だが貸付超過 -> 0 (control floor)",
        "CLM06": "1年未満 60% 保険金2000万 控除後 -> 11,000,000  [D-3101]",
        "CLM07": "責任開始日からちょうど1年 -> 100% -> 10,000,000 (strict-< 境界; <=なら6,000,000)",
        "CLM08": "1年未満 60%控除後(300万)<貸付(320万) -> 0 (floor; start 75%なら550,000)  [D-3101]",
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
