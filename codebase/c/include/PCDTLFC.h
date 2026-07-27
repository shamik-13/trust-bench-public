/* PCDTLFC.h -- PCDTLF record layout (shared/pinned). org VSAM-ESDS. */
#ifndef PCDTLFC_H
#define PCDTLFC_H
#include <stdint.h>

typedef struct pcdtlf_rec {
    char     pd_detail_id[11];
    char     pd_settle_txn_id[11];
    char     pd_merchant_code[5];
    int64_t  pd_txn_amt;      /* minor units (×100) */
    char     pd_settle_kbn[3];
    char     pd_output_status[3];
} pcdtlf_rec_t;

#endif /* PCDTLFC_H */
