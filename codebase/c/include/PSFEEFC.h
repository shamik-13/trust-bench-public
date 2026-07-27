/* PSFEEFC.h -- PSFEEF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PSFEEFC_H
#define PSFEEFC_H
#include <stdint.h>

typedef struct psfeef_rec {
    char     fe_fee_plan_id[11];
    char     fe_merchant_code[5];
    double   fe_rate_kbn;
    double   fe_rate_value;
    int64_t  fe_min_fee_amt;      /* minor units (×100) */
    int64_t  fe_max_fee_amt;      /* minor units (×100) */
    uint32_t fe_apply_dt;      /* YYYYMMDD */
} psfeef_rec_t;

#endif /* PSFEEFC_H */
