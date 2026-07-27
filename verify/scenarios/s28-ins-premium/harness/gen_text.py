#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the s28 LOADER. Widths MUST match LOADER.cbl input
records, measured in BYTES. Amount PIC 9(11)V99 = 13 digits (yen x100); age PIC 9(08) = 8 digits."""
import json, sys
from pathlib import Path


def fixed(s, n):
    b = str(s).encode("utf-8")[:n]
    return b + b" " * (n - len(b))


def amt13(yen):
    return f"{int(round(yen * 100)):013d}".encode("ascii")


def num(v, n):
    return f"{int(v):0{n}d}".encode("ascii")


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    # LFPOLF: POL-NO(16) ENTRY-AGE-CNT(8) SEX-KBN(2) SUM-ASSURED-AMT(13) POL-STATUS-KBN(2)
    with (out / "pol.txt").open("wb") as f:
        for p in fx["policies"]:
            f.write(fixed(p["pol_no"], 16) + num(p["age"], 8) + fixed(p["sex"], 2)
                    + amt13(p["sum_assured"]) + fixed(p["status"], 2) + b"\n")
    print(f"wrote pol.txt ({len(fx['policies'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
