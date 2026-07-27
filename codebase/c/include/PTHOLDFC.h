/* PTHOLDFC.h -- PTHOLDF record layout (shared/pinned). org 順編成. */
#ifndef PTHOLDFC_H
#define PTHOLDFC_H
#include <stdint.h>

typedef struct ptholdf_rec {
    char     hd_hold_id[11];
    char     hd_wallet_id[11];
    char     hd_merchant_code[5];
    int64_t  hd_hold_amt;      /* minor units (×100) */
    char     hd_hold_status[3];
} ptholdf_rec_t;

#endif /* PTHOLDFC_H */
