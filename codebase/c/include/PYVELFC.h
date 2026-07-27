/* PYVELFC.h -- PYVELF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PYVELFC_H
#define PYVELFC_H
#include <stdint.h>

typedef struct pyvelf_rec {
    char     vl_wallet_id[11];
    char     vl_window_start_ts[15];
    uint32_t vl_auth_count;
    int64_t  vl_auth_sum_amt;      /* minor units (×100) */
    uint32_t vl_deny_count;
    char     vl_last_req_ts[15];
} pyvelf_rec_t;

#endif /* PYVELFC_H */
