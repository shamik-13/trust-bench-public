/* PCSUMFC.h -- PCSUMF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PCSUMFC_H
#define PCSUMFC_H
#include <stdint.h>

typedef struct pcsumf_rec {
    char     ps_merchant_code[5];
    char     ps_settle_date[11];
    char     ps_settle_kbn[3];
    uint32_t ps_txn_count;
    int64_t  ps_total_amt;      /* minor units (×100) */
    int64_t  ps_carry_amt;      /* minor units (×100) */
} pcsumf_rec_t;

#endif /* PCSUMFC_H */
