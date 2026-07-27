#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text files the LOADER reads.
Amounts: PIC 9(11)V99 = 13 digits (yen*100). Rates: PIC 9(01)V9(04) = 5 digits
(rate*10000). Field widths MUST match LOADER.cbl's input record layouts."""
import json, sys
from pathlib import Path


def amt13(yen):          # 9(11)V99
    return f"{int(round(yen * 100)):013d}"


def rate5(s):            # 9(01)V9(04), e.g. "0.0180" -> "00180"
    return f"{int(round(float(s) * 10000)):05d}"


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    with (out / "acct.txt").open("w") as f:
        for a in fx["accounts"]:
            f.write(f"{a['acct']:<10}{a['card']:<16}{a['group']:<4}"
                    f"{amt13(a['credit_limit'] + a['over_amt'])}{amt13(a['credit_limit'])}"
                    f"{amt13(a['over_amt'])}{a['kyc']:<2}\n")
    with (out / "card.txt").open("w") as f:
        for a in fx["accounts"]:
            f.write(f"{a['card']:<16}{a['acct']:<10}{'A':<2}\n")
    with (out / "dgrp.txt").open("w") as f:
        for g in fx["dgroups"]:
            f.write(f"{g['code']:<4}{rate5(g['rate_current'])}{rate5(g['rate_old'])}{g['exempt']:<1}\n")
    print(f"wrote acct/card/dgrp.txt to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
