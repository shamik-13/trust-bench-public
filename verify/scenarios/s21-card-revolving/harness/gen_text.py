#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the s21 LOADER. Widths MUST match LOADER.cbl input
records, measured in BYTES (GnuCOBOL LINE SEQUENTIAL parses by byte offset, and the 半角ｶﾅ names are
multibyte in UTF-8 — pad by encoded byte length, not character count). Amount PIC 9(11)V99 = 13 digits."""
import json, sys
from pathlib import Path


def fixed(s, n):
    """Left-justify str into EXACTLY n bytes (UTF-8), space-padded / byte-truncated."""
    b = str(s).encode("utf-8")[:n]
    return b + b" " * (n - len(b))


def amt13(yen):
    return f"{int(round(yen * 100)):013d}".encode("ascii")


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    # CDREVF: CARD-NO(16) MEMBER-ID(10) REV-STATUS(2) REV-COURSE-CD(10) MEMBER-NAME-KANA(40)
    #         REV-START-DT(8)  -> 86 bytes/record
    with (out / "rev.txt").open("wb") as f:
        for c in fx["cards"]:
            f.write(fixed(c["card_no"], 16) + fixed(c["member"], 10) + fixed(c["status"], 2)
                    + fixed(c["course"], 10) + fixed(c["name"], 40) + fixed(c["start_dt"], 8) + b"\n")
    # CDRBALF: CARD-NO(16) CYCLE-DT(8) REV-BAL(13) CARRIED-FEE(13) NEW-REV(13)
    with (out / "bal.txt").open("wb") as f:
        for b in fx["balances"]:
            f.write(fixed(b["card_no"], 16) + fixed(b["cycle_dt"], 8) + amt13(b["rev_bal"])
                    + amt13(b["carried_fee"]) + amt13(b["new_rev"]) + b"\n")
    print(f"wrote rev.txt ({len(fx['cards'])}) + bal.txt ({len(fx['balances'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
