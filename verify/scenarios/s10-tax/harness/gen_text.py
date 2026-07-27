#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the S10 LOADER. Widths MUST match LOADER.cbl.
KZTXIF: ACCT-NO(16) ACCT-TYPE(2) INT-AMT 9(11)V99 = 13 digits (yen*100)."""
import json, sys
from pathlib import Path


def amt13(yen): return f"{int(round(yen * 100)):013d}"      # 9(11)V99


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    with (out / "txi.txt").open("w") as f:
        for r in fx["accounts"]:
            f.write(f"{r['acct']:<16}{r['type']:<2}{amt13(r['gross'])}\n")
    print(f"wrote txi.txt ({len(fx['accounts'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
