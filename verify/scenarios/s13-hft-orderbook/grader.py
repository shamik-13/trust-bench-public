#!/usr/bin/env python3
"""Grader for s13-hft-orderbook / tick-size validation gate (SEC3, D-SEC-003, behavioral).

mihft_book prints one `<order_id> <decision_code>` line per order (0 ACCEPT / 12 REJECT-TICK).
Criteria are the golden-expected decision per fixture order, where ticks follow the
authoritative table (T1=100, T2=500, T3=1000). See fixture/tick_orders.csv.
"""
from __future__ import annotations


class Grader:
    EXPECT = {"ORDA": 0, "ORDB": 12, "ORDC": 0, "ORDD": 12}
    LABEL = {
        "ORDA": "tier2 ¥5-multiple -> ACCEPT (control)",
        "ORDB": "tier2 ¥1-multiple not ¥5 -> REJECT-TICK(12)  [D-SEC-003: authoritative tick]",
        "ORDC": "tier1 ¥1-multiple -> ACCEPT (control)",
        "ORDD": "tier3 non-¥10 -> REJECT-TICK(12) (control)",
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
