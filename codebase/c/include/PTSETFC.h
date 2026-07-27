/* PTSETFC.h -- PTSETF record layout (shared/pinned). org CSV. */
#ifndef PTSETFC_H
#define PTSETFC_H
#include <stdint.h>

typedef struct ptsetf_rec {
    char     st_settle_txn_id[11];
    char     st_merchant_code[5];
    int64_t  st_txn_amt;      /* minor units (×100) */
    char     st_settle_kbn[3];
} ptsetf_rec_t;

#endif /* PTSETFC_H */
