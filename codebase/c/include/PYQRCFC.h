/* PYQRCFC.h -- PYQRCF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PYQRCFC_H
#define PYQRCFC_H
#include <stdint.h>

typedef struct pyqrcf_rec {
    char     qr_qr_id[11];
    char     qr_wallet_id[11];
    char     qr_merchant_code[5];
    int64_t  qr_req_amt;      /* minor units (×100) */
    char     qr_qr_status[3];
    char     qr_expire_ts[15];
} pyqrcf_rec_t;

#endif /* PYQRCFC_H */
