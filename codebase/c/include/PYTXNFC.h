/* PYTXNFC.h -- PYTXNF record layout (shared/pinned). org VSAM-ESDS. */
#ifndef PYTXNFC_H
#define PYTXNFC_H
#include <stdint.h>

typedef struct pytxnf_rec {
    char     tx_txn_id[11];
    char     tx_req_id[11];
    char     tx_wallet_id[11];
    char     tx_merchant_code[5];
    int64_t  tx_req_amt;      /* minor units (×100) */
    char     tx_txn_status[3];
    uint32_t tx_auth_dt;      /* YYYYMMDD */
    uint32_t tx_capture_dt;      /* YYYYMMDD */
} pytxnf_rec_t;

#endif /* PYTXNFC_H */
