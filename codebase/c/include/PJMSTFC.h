/* PJMSTFC.h -- PJMSTF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PJMSTFC_H
#define PJMSTFC_H
#include <stdint.h>

typedef struct pjmstf_rec {
    char     ms_merchant_code[5];
    char     ms_merchant_name[41];
    char     ms_bank_code[5];
    char     ms_account_no[17];
    char     ms_active_flag;
    char     ms_risk_rank[11];
} pjmstf_rec_t;

#endif /* PJMSTFC_H */
