#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the s33 LOADER (TWO files: calendar + instructions).
Widths MUST match LOADER.cbl input records, in BYTES. CONC-AMT PIC 9(11)V99 = 13 digits (yen x100)."""
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
    # CCCALF: CAL-DT(8) HOLIDAY-FLAG(1)
    with (out / "cal.txt").open("wb") as f:
        for c in fx["calendar"]:
            f.write(num(c["dt"], 8) + fixed(c["flag"], 1) + b"\n")
    # CCFCTF: FCT-ID(10) TRIGGER-DT(8) CONC-AMT(13) FCT-STATUS-KBN(2)
    with (out / "fct.txt").open("wb") as f:
        for i in fx["instructions"]:
            f.write(fixed(i["fct_id"], 10) + num(i["trigger"], 8)
                    + amt13(i["amt"]) + fixed(i["status"], 2) + b"\n")
    print(f"wrote cal.txt ({len(fx['calendar'])}) + fct.txt ({len(fx['instructions'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
