/* PYMERFC.h -- PYMERF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PYMERFC_H
#define PYMERFC_H
#include <stdint.h>

typedef struct pymerf_rec {
    char     mr_merchant_code[5];
    char     mr_merchant_status[3];
    char     mr_mcc[11];
    int64_t  mr_daily_limit_amt;      /* minor units (×100) */
    char     mr_risk_rank[11];
    char     mr_settle_cycle_kbn[3];
} pymerf_rec_t;

#endif /* PYMERFC_H */
