/* PSTXNFC.h -- PSTXNF record layout (shared/pinned). org CSV. */
#ifndef PSTXNFC_H
#define PSTXNFC_H
#include <stdint.h>

typedef struct pstxnf_rec {
    char     tx_txn_id[11];
    char     tx_merchant_code[5];
    char     tx_txn_kbn[3];
    int64_t  tx_txn_amt;      /* minor units (×100) */
    uint32_t tx_txn_dt;      /* YYYYMMDD */
} pstxnf_rec_t;

#endif /* PSTXNFC_H */
