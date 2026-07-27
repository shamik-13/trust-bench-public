#!/usr/bin/env python3
"""fixture.json -> byte-width DISPLAY text for the s19 LOADER (pad by UTF-8 bytes; amounts x100)."""
import json, sys
from pathlib import Path
def fixed(s, n):
    b = str(s).encode("utf-8")[:n]; return b + b" " * (n - len(b))
def amt13(yen): return f"{int(round(yen * 100)):013d}".encode("ascii")
def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text()); out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    # CDCARDF: CARD-NO(16) MEMBER-ID(10) STATUS(2) LIMIT(13) NAME(40)
    with (out / "card.txt").open("wb") as f:
        for c in fx["cards"]:
            f.write(fixed(c["card_no"],16)+fixed(c["member"],10)+fixed(c["status"],2)+amt13(c["limit"])+fixed(c["name"],40)+b"\n")
    # CDSALEF: SALE-ID(10) CARD-NO(16) SALE-AMT(13) CURRENCY(10) MERCHANT(4) SALE-DT(8) AUTH-ID(10)
    with (out / "sale.txt").open("wb") as f:
        for s in fx["sales"]:
            f.write(fixed(s["sale_id"],10)+fixed(s["card_no"],16)+amt13(s["amount"])+fixed(s["currency"],10)
                    +fixed(s["merchant"],4)+fixed(s["sale_dt"],8)+fixed(s["auth_id"],10)+b"\n")
    print(f"wrote card.txt ({len(fx['cards'])}) + sale.txt ({len(fx['sales'])}) to {out}")
if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
