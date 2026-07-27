/* PSMERFC.h -- PSMERF record layout (shared/pinned). org CSV. */
#ifndef PSMERFC_H
#define PSMERFC_H
#include <stdint.h>

typedef struct psmerf_rec {
    char     mr_merchant_code[5];
    char     mr_merchant_name[41];
    char     mr_mer_status[3];
    char     mr_bank_acct_no[17];
} psmerf_rec_t;

#endif /* PSMERFC_H */
