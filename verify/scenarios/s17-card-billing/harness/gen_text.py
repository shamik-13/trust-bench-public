#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the s17 LOADER. Widths MUST match
LOADER.cbl input records, measured in BYTES (GnuCOBOL LINE SEQUENTIAL parses by byte
offset, and the 半角ｶﾅ names are multibyte in UTF-8 — so pad by encoded byte length,
not character count, or the trailing fields misalign). Amount PIC 9(11)V99 = 13 digits."""
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
    # CDCARDF: CARD-NO(16) MEMBER-ID(10) CARD-STATUS(2) CREDIT-LIMIT(13) BILL-CYCLE-CD(10)
    #          MEMBER-NAME-KANA(40) OPEN-DT(8)  -> 99 bytes/record
    with (out / "card.txt").open("wb") as f:
        for c in fx["cards"]:
            f.write(fixed(c["card_no"], 16) + fixed(c["member"], 10) + fixed(c["status"], 2)
                    + amt13(c["limit"]) + fixed(c["cycle"], 10) + fixed(c["name"], 40)
                    + fixed(c["open_dt"], 8) + b"\n")
    # CDBALF: CARD-NO(16) CYCLE-DT(8) CLOSING(13) REVOLVING(13) NEW-CHARGE(13) CASH-ADV(13)
    with (out / "bal.txt").open("wb") as f:
        for b in fx["balances"]:
            f.write(fixed(b["card_no"], 16) + fixed(b["cycle_dt"], 8) + amt13(b["closing"])
                    + amt13(b["revolving"]) + amt13(b["new_charge"]) + amt13(b["cash_adv"]) + b"\n")
    print(f"wrote card.txt ({len(fx['cards'])}) + bal.txt ({len(fx['balances'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
