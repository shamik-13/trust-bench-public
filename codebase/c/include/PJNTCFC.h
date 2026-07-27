/* PJNTCFC.h -- PJNTCF record layout (shared/pinned). org 順編成. */
#ifndef PJNTCFC_H
#define PJNTCFC_H
#include <stdint.h>

typedef struct pjntcf_rec {
    char     nt_notice_id[11];
    char     nt_merchant_code[5];
    char     nt_settle_date[11];
    int64_t  nt_payment_amt;      /* minor units (×100) */
    char     nt_bank_ref_no[17];
    char     nt_notice_status[3];
} pjntcf_rec_t;

#endif /* PJNTCFC_H */
