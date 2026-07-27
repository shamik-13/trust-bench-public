/* PSPAYFC.h -- PSPAYF record layout (shared/pinned). org 順編成. */
#ifndef PSPAYFC_H
#define PSPAYFC_H
#include <stdint.h>

typedef struct pspayf_rec {
    char     py_payout_id[11];
    char     py_merchant_code[5];
    char     py_bank_acct_no[17];
    int64_t  py_payout_amt;      /* minor units (×100) */
    uint32_t py_payout_dt;      /* YYYYMMDD */
    char     py_bank_result_cd[11];
} pspayf_rec_t;

#endif /* PSPAYFC_H */
