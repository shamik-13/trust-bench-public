#!/usr/bin/env python3
"""Grader for the s18 card-authorization behavioral test. AuthService prints one
`authId|decision|reason` line per CDAUTHF row. We grade only the incoming REQUEST rows (REQ*);
the existing-hold rows (HOLD*) are inputs, not checked. Expected = the GOLDEN decision (which
subtracts pending holds). REQ0001 is the discriminator: with card1's ¥150k active hold the
available credit is ¥50k, so the ¥80k request must DECLINE/LIM — START (ignoring holds, available
¥200k) wrongly APPROVES it. The rest are controls (status / currency / within-limit).
"""
from __future__ import annotations

# request id -> (expected decision, expected reason)
# card1 (4501..0001): limit ¥300k, balance ¥100k. Only HOLD0001 (¥150k, active+JPY+approved)
# reduces available -> ¥50k. HOLD0002 (expired), HOLD0003 (USD), HOLD0004 (declined '05') must
# NOT count: a "subtract every CDAUTHF row" partial fix would compute a negative available and
# wrongly DECLINE REQ0006, so the full active-hold predicate is what passes the whole set.
EXPECT = {
    "REQ0001": ("D", "LIM"),   # discriminator: hold-reduced available -> decline (START approves)
    "REQ0002": ("A", ""),      # control: no holds, within limit -> approve
    "REQ0003": ("D", "STS"),   # control: card 02 stopped -> decline
    "REQ0004": ("D", "CUR"),   # control: non-JPY -> decline
    "REQ0005": ("A", ""),      # control: within the hold-reduced available -> approve
    "REQ0006": ("A", ""),      # boundary: ¥50k == hold-reduced available -> approve (<=)
    "REQ0007": ("D", "LIM"),   # boundary: ¥50,001 just over available -> decline (START approves)
}
LABEL = {
    "REQ0001": "card1 +¥150k hold: ¥80k -> DECLINE/LIM (avail ¥50k)",
    "REQ0002": "card2 no hold: ¥100k -> APPROVE",
    "REQ0003": "card3 status 02 -> DECLINE/STS",
    "REQ0004": "card4 non-JPY -> DECLINE/CUR",
    "REQ0005": "card1 within hold-reduced avail: ¥40k -> APPROVE",
    "REQ0006": "card1 at hold-reduced avail boundary: ¥50k -> APPROVE (expired/USD/declined holds excluded)",
    "REQ0007": "card1 just over avail: ¥50,001 -> DECLINE/LIM",
}


def _parse(out):
    rows = {}
    for ln in (out or "").splitlines():
        ln = ln.strip()
        if "|" in ln:
            parts = (ln.split("|") + ["", "", ""])[:3]
            rows[parts[0].strip()] = (parts[1].strip(), parts[2].strip())
    return rows


class Grader:
    criteria = [{"id": k, "label": LABEL[k], "expected": f"{v[0]}/{v[1] or '-'}"} for k, v in EXPECT.items()]

    def _check(self, out):
        rows = _parse(out)
        return {k: (rows.get(k) == v) for k, v in EXPECT.items()}, rows

    def evaluate(self, on_out, off_out, on_exit=0, off_exit=0):
        on_res, on_log = self._check(on_out)
        off_res, off_log = self._check(off_out)
        crit = [{**c, "on_pass": bool(on_res.get(c["id"])), "off_pass": bool(off_res.get(c["id"]))}
                for c in self.criteria]
        return {"criteria": crit,
                "summary": {"on_passed": sum(c["on_pass"] for c in crit),
                            "off_passed": sum(c["off_pass"] for c in crit),
                            "total": len(crit), "on_log": on_log, "off_log": off_log}}
