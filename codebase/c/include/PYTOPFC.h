/* PYTOPFC.h -- PYTOPF record layout (shared/pinned). org VSAM-ESDS. */
#ifndef PYTOPFC_H
#define PYTOPFC_H
#include <stdint.h>

typedef struct pytopf_rec {
    char     tp_topup_id[11];
    char     tp_wallet_id[11];
    int64_t  tp_topup_amt;      /* minor units (×100) */
    char     tp_payment_method[11];
    char     tp_topup_status[3];
    char     tp_request_ts[15];
} pytopf_rec_t;

#endif /* PYTOPFC_H */
