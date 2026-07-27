/* PSSETFC.h -- PSSETF record layout (shared/pinned). org CSV. */
#ifndef PSSETFC_H
#define PSSETFC_H
#include <stdint.h>

typedef struct pssetf_rec {
    char     st_settle_id[11];
    char     st_merchant_code[5];
    int64_t  st_net_amt;      /* minor units (×100) */
    int64_t  st_charge_amt;      /* minor units (×100) */
    int64_t  st_payout_amt;      /* minor units (×100) */
    uint32_t st_settle_dt;      /* YYYYMMDD */
} pssetf_rec_t;

#endif /* PSSETFC_H */
