#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the S5 LOADER. Widths MUST match
LOADER.cbl. AVG-BAL 9(11)V99 = 13 digits (yen*100)."""
import json, sys
from pathlib import Path


def amt13(yen): return f"{int(round(yen * 100)):013d}"


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    # JHCBALF: CUST-ID(10) AVG-BAL(13)
    with (out / "cbal.txt").open("w") as f:
        for c in fx["customers"]:
            f.write(f"{c['cust']:<10}{amt13(c['avg_bal'])}\n")
    print(f"wrote cbal.txt ({len(fx['customers'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
