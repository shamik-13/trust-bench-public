/* PYPENDFC.h -- PYPENDF record layout (shared/pinned). org 順編成. */
#ifndef PYPENDFC_H
#define PYPENDFC_H
#include <stdint.h>

typedef struct pypendf_rec {
    char     pn_pend_id[11];
    char     pn_wallet_id[11];
    int64_t  pn_pend_amt;      /* minor units (×100) */
    char     pn_pend_status[3];
    uint32_t pn_capture_dt;      /* YYYYMMDD */
} pypendf_rec_t;

#endif /* PYPENDFC_H */
