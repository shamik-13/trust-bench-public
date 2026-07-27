/* PTINPFC.h -- PTINPF record layout (shared/pinned). org 順編成. */
#ifndef PTINPFC_H
#define PTINPFC_H
#include <stdint.h>

typedef struct ptinpf_rec {
    char     pi_import_batch_id[11];
    char     pi_cap_id[11];
    char     pi_hold_id[11];
    char     pi_merchant_code[5];
    int64_t  pi_cap_amt;      /* minor units (×100) */
    char     pi_import_status[3];
} ptinpf_rec_t;

#endif /* PTINPFC_H */
