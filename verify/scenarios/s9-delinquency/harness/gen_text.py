#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the S9 LOADER. Widths MUST match LOADER.cbl.
KZDLQF: ACCT-NO(16) OVERDUE-AMT 9(11)V99 = 13 digits (yen*100) DUE-DT(8) ASOF-DT(8) CURR-STATUS(2)."""
import json, sys
from pathlib import Path


def amt13(yen): return f"{int(round(yen * 100)):013d}"      # 9(11)V99


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    asof = fx["asof"]
    with (out / "dlq.txt").open("w") as f:
        for r in fx["accounts"]:
            f.write(f"{r['acct']:<16}{amt13(r['amt'])}{r['due']:08d}{asof:08d}{'10':<2}\n")
    print(f"wrote dlq.txt ({len(fx['accounts'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
