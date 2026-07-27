/* PCCARFC.h -- PCCARF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PCCARFC_H
#define PCCARFC_H
#include <stdint.h>

typedef struct pccarf_rec {
    char     cr_carry_id[11];
    char     cr_merchant_code[5];
    char     cr_settle_kbn[3];
    int64_t  cr_carry_amt;      /* minor units (×100) */
    char     cr_carry_reason[5];
    char     cr_next_settle_date[11];
} pccarf_rec_t;

#endif /* PCCARFC_H */
