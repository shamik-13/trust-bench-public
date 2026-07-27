/* PSADJFC.h -- PSADJF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PSADJFC_H
#define PSADJFC_H
#include <stdint.h>

typedef struct psadjf_rec {
    char     adj_adjust_id[11];
    char     adj_merchant_code[5];
    char     adj_adjust_kbn[3];
    int64_t  adj_adjust_amt;      /* minor units (×100) */
    char     adj_reason_cd[11];
    uint32_t adj_apply_dt;      /* YYYYMMDD */
    char     adj_approval_status[3];
} psadjf_rec_t;

#endif /* PSADJFC_H */
