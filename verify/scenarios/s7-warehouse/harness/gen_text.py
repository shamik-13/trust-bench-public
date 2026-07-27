#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the S7 LOADER. Widths MUST match LOADER.cbl.
AMT 9(11)V99 = 13 digits (yen*100)."""
import json, sys
from pathlib import Path


def amt13(yen): return f"{int(round(yen * 100)):013d}"


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    # KZFEEHF: ACCT-NO(16) CYCLE-DT(8) FEE-AMT(13) FEE-YTD(13) CONFIRM-FLAG(1)
    with (out / "fee.txt").open("w") as f:
        for r in fx["fees"]:
            f.write(f"{r['acct']:<16}{r['cycle']:08d}{amt13(r['fee'])}{amt13(r['ytd'])}{r['confirm']:<1}\n")
    print(f"wrote fee.txt ({len(fx['fees'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
