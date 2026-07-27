#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the S2 LOADER. Widths MUST match
LOADER.cbl input records. Amount PIC 9(11)V99 = 13 digits (yen*100)."""
import json, sys
from pathlib import Path

FIXED_DT = "20260601"


def amt13(yen): return f"{int(round(yen * 100)):013d}"


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    # KZACCTF: ACCT-NO(16) BRANCH(4) ACCT-TYPE(2) CUSTOMER-ID(10) ACCT-NAME-KANA(40) STATUS(2)
    with (out / "acct.txt").open("w") as f:
        for a in fx["accounts"]:
            f.write(f"{a['acct_no']:<16}{a['branch']:<4}{a['acct_type']:<2}{a['cust']:<10}"
                    f"{a['name']:<40}{a['status']:<2}\n")
    # TGINRMF: REMIT-DT(8) CENTER-SEQ(10) REMIT-TYPE(2) SBANK(4) SBR(4) SNAME(40)
    #          PBANK(4) PBR(4) PACCT-TYPE(2) PACCT-NO(16) PNAME(40) AMT(13) MSG(20)
    with (out / "inrm.txt").open("w") as f:
        for m in fx["remittances"] + fx.get("corrections", []):
            f.write(f"{FIXED_DT}{m['seq']:<10}{m['type']:<2}{'0001':<4}{'001':<4}{'SENDER':<40}"
                    f"{'0001':<4}{'001':<4}{'1':<2}{m['payee_acct']:<16}{m['payee_name']:<40}"
                    f"{amt13(m['amount'])}{m.get('msg', ''):<20}\n")
    print(f"wrote acct.txt ({len(fx['accounts'])}) + inrm.txt ({len(fx['remittances'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
