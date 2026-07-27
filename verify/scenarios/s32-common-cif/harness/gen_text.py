#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the s32 LOADER. Widths MUST match LOADER.cbl input
records, in BYTES. CIF-NO PIC X(16) holds the 10-digit key body left-justified; BIRTH-DT PIC 9(08)."""
import json, sys
from pathlib import Path


def fixed(s, n):
    b = str(s).encode("utf-8")[:n]
    return b + b" " * (n - len(b))


def num(v, n):
    return f"{int(v):0{n}d}".encode("ascii")


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    # CMCIFF: CIF-NO(16) BIRTH-DT(8) SEX-KBN(2) CIF-STATUS-KBN(2)
    with (out / "cif.txt").open("wb") as f:
        for c in fx["customers"]:
            f.write(fixed(c["cif_no"], 16) + num(c["birth"], 8)
                    + fixed(c["sex"], 2) + fixed(c["status"], 2) + b"\n")
    print(f"wrote cif.txt ({len(fx['customers'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
