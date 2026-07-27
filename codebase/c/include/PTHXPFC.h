/* PTHXPFC.h -- PTHXPF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PTHXPFC_H
#define PTHXPFC_H
#include <stdint.h>

typedef struct pthxpf_rec {
    char     hx_hold_id[11];
    char     hx_wallet_id[11];
    char     hx_merchant_code[5];
    char     hx_expire_at[11];
    char     hx_reason_code[5];
    char     hx_expire_status[3];
} pthxpf_rec_t;

#endif /* PTHXPFC_H */
