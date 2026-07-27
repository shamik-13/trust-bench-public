#!/usr/bin/env python3
"""fixture.json -> fixed-width DISPLAY text for the S4 LOADER. Widths MUST match
LOADER.cbl input records. AMT 9(11)V99 = 13 digits (yen*100); LIMIT 9(09)V99 = 11."""
import json, sys
from pathlib import Path


def amt13(yen): return f"{int(round(yen * 100)):013d}"
def lim11(yen): return f"{int(round(yen * 100)):011d}"


def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text())
    out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    # KZACCTF: ACCT-NO(16) CUST-ID(10) ACCT-TYPE(2) GROUP-CODE(4) CUR-BAL(13) AVG-BAL(13) CREDIT-LIMIT(11)
    with (out / "acct.txt").open("w") as f:
        for a in fx["accounts"]:
            f.write(f"{a['acct']:<16}{a['cust']:<10}{'01':<2}{'STD0':<4}"
                    f"{amt13(a['avg_bal'])}{amt13(a['avg_bal'])}{lim11(a['limit'])}\n")
    # KZCUSTF: CUST-ID(10) CUST-NAME(40) BRANCH-CODE(4) KYC-STATUS(1)
    with (out / "cust.txt").open("w") as f:
        for c in fx["customers"]:
            f.write(f"{c['cust']:<10}{'KOKYAKU':<40}{'001':<4}{c['kyc']:<1}\n")
    # KZEXPF: CUST-ID(10) PRODUCT-TYPE(2) EXPOSURE-AMT(13)
    with (out / "exp.txt").open("w") as f:
        for e in fx["exposures"]:
            f.write(f"{e['cust']:<10}{e['product']:<2}{amt13(e['exposure'])}\n")
    print(f"wrote acct.txt({len(fx['accounts'])}) cust.txt({len(fx['customers'])}) exp.txt({len(fx['exposures'])}) to {out}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
