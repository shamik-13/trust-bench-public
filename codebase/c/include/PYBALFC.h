/* PYBALFC.h -- PYBALF record layout (shared/pinned). org 順編成. */
#ifndef PYBALFC_H
#define PYBALFC_H
#include <stdint.h>

typedef struct pybalf_rec {
    char     bl_wallet_id[11];
    int64_t  bl_ledger_bal_amt;      /* minor units (×100) */
    int64_t  bl_last_topup_amt;      /* minor units (×100) */
    uint32_t bl_bal_as_of_dt;      /* YYYYMMDD */
} pybalf_rec_t;

#endif /* PYBALFC_H */
