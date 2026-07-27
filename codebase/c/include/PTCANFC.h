/* PTCANFC.h -- PTCANF record layout (shared/pinned). org 順編成. */
#ifndef PTCANFC_H
#define PTCANFC_H
#include <stdint.h>

typedef struct ptcanf_rec {
    char     cn_cancel_id[11];
    char     cn_cap_id[11];
    char     cn_hold_id[11];
    char     cn_merchant_code[5];
    int64_t  cn_cancel_amt;      /* minor units (×100) */
    char     cn_cancel_status[3];
} ptcanf_rec_t;

#endif /* PTCANFC_H */
