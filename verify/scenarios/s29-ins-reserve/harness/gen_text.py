#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the s29 LOADER. Widths MUST match LOADER.cbl input
records, in BYTES. Amount PIC 9(11)V99 = 13 digits (yen x100); month PIC 9(08) = 8 digits."""
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
    # LFCVPF: POL-NO(16) RESERVE-AMT(13) NEWBIZ-COST-AMT(13) ELAPSED-MONTH-CNT(8) CV-STATUS-KBN(2)
    with (out / "cv.txt").open("wb") as f:
        for p in fx["policies"]:
            f.write(fixed(p["pol_no"], 16) + amt13(p["reserve"]) + amt13(p["cost"])
                    + num(p["elapsed"], 8) + fixed(p["status"], 2) + b"\n")
    print(f"wrote cv.txt ({len(fx['policies'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
