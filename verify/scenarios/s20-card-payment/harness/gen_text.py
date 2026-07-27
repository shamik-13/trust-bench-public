#!/usr/bin/env python3
"""fixture.json -> byte-width DISPLAY text for the s20 LOADER (pad by UTF-8 bytes; amounts x100)."""
import json, sys
from pathlib import Path
def fixed(s, n):
    b = str(s).encode("utf-8")[:n]; return b + b" " * (n - len(b))
def amt13(yen): return f"{int(round(yen * 100)):013d}".encode("ascii")
def main(fixture_path, out_dir):
    fx = json.loads(Path(fixture_path).read_text()); out = Path(out_dir); out.mkdir(parents=True, exist_ok=True)
    # CDOSF: CARD-NO(16) FEE(13) INT(13) PRIN(13) CYCLE-DT(8)
    with (out / "os.txt").open("wb") as f:
        for o in fx["outstanding"]:
            f.write(fixed(o["card_no"],16)+amt13(o["fee"])+amt13(o["interest"])+amt13(o["principal"])+fixed(o["cycle_dt"],8)+b"\n")
    # CDPAYF: PAY-ID(10) CARD-NO(16) PAY-AMT(13) PAY-DT(8) METHOD(10)
    with (out / "pay.txt").open("wb") as f:
        for p in fx["payments"]:
            f.write(fixed(p["pay_id"],10)+fixed(p["card_no"],16)+amt13(p["amount"])+fixed(p["pay_dt"],8)+fixed(p["method"],10)+b"\n")
    print(f"wrote os.txt ({len(fx['outstanding'])}) + pay.txt ({len(fx['payments'])}) to {out}")
if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
