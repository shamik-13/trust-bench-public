/* PYLMTFC.h -- PYLMTF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PYLMTFC_H
#define PYLMTFC_H
#include <stdint.h>

typedef struct pylmtf_rec {
    char     lm_tier_code[5];
    int64_t  lm_per_txn_limit_amt;      /* minor units (×100) */
    int64_t  lm_daily_limit_amt;      /* minor units (×100) */
    int64_t  lm_monthly_limit_amt;      /* minor units (×100) */
    int64_t  lm_alert_threshold_amt;      /* minor units (×100) */
} pylmtf_rec_t;

#endif /* PYLMTFC_H */
