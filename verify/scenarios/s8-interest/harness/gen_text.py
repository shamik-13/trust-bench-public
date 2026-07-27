#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the S8 LOADER. Widths MUST match LOADER.cbl.
KZDBALF: ACCT-NO(16) CYCLE-ID(10) AVG-DAILY-BAL 9(11)V99 = 13 digits (yen*100)
         INT-RATE 9(01)V9(04) = 5 digits (rate*10000) PERIOD-START-DT(8).
KZCYRF:  CYCLE-ID(10) NOMINAL-DT(8) RESOLVED-DT(8) ROLLED-FLAG(1)."""
import json, sys
from pathlib import Path


def bal13(yen):  return f"{int(round(yen * 100)):013d}"      # 9(11)V99
def rate5(r):    return f"{int(round(r * 10000)):05d}"        # 9(01)V9(04)


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    with (out / "dbal.txt").open("w") as f:
        for r in fx["accounts"]:
            f.write(f"{r['acct']:<16}{r['cycle']:<10}{bal13(r['bal'])}{rate5(r['rate'])}{r['start']:08d}\n")
    with (out / "cyr.txt").open("w") as f:
        for c in fx["cycles"]:
            f.write(f"{c['cycle']:<10}{c['nominal']:08d}{c['resolved']:08d}{c['rolled']:<1}\n")
    print(f"wrote dbal.txt ({len(fx['accounts'])}) + cyr.txt ({len(fx['cycles'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
