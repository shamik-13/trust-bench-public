/* PYARSPFC.h -- PYARSPF record layout (shared/pinned). org 順編成. */
#ifndef PYARSPFC_H
#define PYARSPFC_H
#include <stdint.h>

typedef struct pyarspf_rec {
    char     ar_req_id[11];
    char     ar_wallet_id[11];
    char     ar_decision_kbn[3];
    int64_t  ar_avail_amt;      /* minor units (×100) */
    int64_t  ar_req_amt;      /* minor units (×100) */
    char     ar_decline_reason[5];
} pyarspf_rec_t;

#endif /* PYARSPFC_H */
