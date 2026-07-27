/* PSDTLFC.h -- PSDTLF record layout (shared/pinned). org VSAM-ESDS. */
#ifndef PSDTLFC_H
#define PSDTLFC_H
#include <stdint.h>

typedef struct psdtlf_rec {
    char     dtl_detail_id[11];
    char     dtl_settle_id[11];
    char     dtl_merchant_code[5];
    char     dtl_txn_id[11];
    int64_t  dtl_txn_amt;      /* minor units (×100) */
    int64_t  dtl_charge_amt;      /* minor units (×100) */
    char     dtl_line_kbn[3];
} psdtlf_rec_t;

#endif /* PSDTLFC_H */
