#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the S6 LOADER. Widths MUST match LOADER.cbl."""
import json, sys
from pathlib import Path


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    # KZCALF: CAL-DT(8) HOLIDAY-FLAG(1)
    with (out / "cal.txt").open("w") as f:
        for c in fx["calendar"]:
            f.write(f"{c['date']:08d}{c['holiday']:<1}\n")
    # KZCYCF: CYCLE-ID(10) NOMINAL-DT(8)
    with (out / "cyc.txt").open("w") as f:
        for c in fx["cycles"]:
            f.write(f"{c['cycle']:<10}{c['nominal']:08d}\n")
    print(f"wrote cal.txt({len(fx['calendar'])}) cyc.txt({len(fx['cycles'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
