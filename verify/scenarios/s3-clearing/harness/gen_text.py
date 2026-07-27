#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the S3 LOADER. Widths MUST match
LOADER.cbl input records. Amount PIC 9(11)V99 = 13 digits (yen*100)."""
import json, sys
from pathlib import Path

FIXED_DT = "20260601"


def amt13(yen): return f"{int(round(yen * 100)):013d}"


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    # TGZENF: VALUE-DT(8) CENTER-SEQ(10) ZEN-TYPE(2) COUNTER-BANK(4) COUNTER-BRANCH(4)
    #         RECV-ACCT-NO(16) REMIT-AMT(13=9(11)V99) REMIT-NAME-KANA(40)
    with (out / "zen.txt").open("w") as f:
        for m in fx["messages"]:
            f.write(f"{FIXED_DT}{m['seq']:<10}{m['type']:<2}{m['bank']:<4}{'001':<4}"
                    f"{'0000000000000001':<16}{amt13(m['amount'])}{'SOUSHIN':<40}\n")
    # TGNETCF: COUNTER-BANK(4) OUT-FLAG(1) IN-FLAG(1) CTL-DT(8)
    with (out / "netc.txt").open("w") as f:
        for c in fx["counterparties"]:
            f.write(f"{c['bank']:<4}{c['out_flag']:<1}{c['in_flag']:<1}{FIXED_DT}\n")
    print(f"wrote zen.txt ({len(fx['messages'])}) + netc.txt ({len(fx['counterparties'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
