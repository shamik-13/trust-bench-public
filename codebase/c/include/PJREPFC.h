/* PJREPFC.h -- PJREPF record layout (shared/pinned). org 順編成. */
#ifndef PJREPFC_H
#define PJREPFC_H
#include <stdint.h>

typedef struct pjrepf_rec {
    char     rp_report_id[11];
    char     rp_merchant_code[5];
    char     rp_settle_date[11];
    int64_t  rp_gross_amt;      /* minor units (×100) */
    int64_t  rp_fee_amt;      /* minor units (×100) */
    int64_t  rp_net_amt;      /* minor units (×100) */
    char     rp_report_status[3];
} pjrepf_rec_t;

#endif /* PJREPFC_H */
