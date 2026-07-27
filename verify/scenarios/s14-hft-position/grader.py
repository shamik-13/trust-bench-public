#!/usr/bin/env python3
"""Grader for s14-hft-position / position average-cost gate (SEC4, D-SEC-004, behavioral).

mihft_pos prints one `<cif>:<instr> <avg_cost>` line per position. Criteria are the
golden-expected weighted-average cost per fixture position (see fixture/pos_fills.csv).
"""
from __future__ import annotations


class Grader:
    # golden-expected weighted-average cost per position
    EXPECT = {"CIF01:AAA": 1500, "CIF01:BBB": 3000, "CIF02:CCC": 2000}
    LABEL = {
        "CIF01:AAA": "two buys -> weighted avg 1500  [D-SEC-004: not last price]",
        "CIF01:BBB": "single fill -> 3000 (control)",
        "CIF02:CCC": "offsetting buy+sell -> 2000 (Σ|qty| denom; net_qty denom gives -2000, last 4000)",
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
            "id": k,
            "expected": self.LABEL[k],
            "on_pass": on[k],
            "off_pass": off[k],
        } for k in self.EXPECT]
        return {"criteria": criteria, "summary": {
            "on_passed": sum(c["on_pass"] for c in criteria),
            "off_passed": sum(c["off_pass"] for c in criteria),
            "total": len(criteria)}}
