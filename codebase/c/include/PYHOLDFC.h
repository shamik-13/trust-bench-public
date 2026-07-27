/* PYHOLDFC.h -- PYHOLDF record layout (shared/pinned). org 順編成. */
#ifndef PYHOLDFC_H
#define PYHOLDFC_H
#include <stdint.h>

typedef struct pyholdf_rec {
    char     hd_hold_id[11];
    char     hd_wallet_id[11];
    int64_t  hd_hold_amt;      /* minor units (×100) */
    char     hd_hold_result[11];
    char     hd_merchant_code[5];
    char     hd_currency_cd[11];
    uint32_t hd_hold_exp_dt;      /* YYYYMMDD */
} pyholdf_rec_t;

#endif /* PYHOLDFC_H */
