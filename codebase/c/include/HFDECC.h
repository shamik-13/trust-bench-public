/* HFDECC.h -- HFDEC record layout (shared/pinned). org VSAM-ESDS. */
#ifndef HFDECC_H
#define HFDECC_H
#include <stdint.h>

typedef struct hfdec_rec {
    char     hd_decision_id[11];
    char     hd_order_id[11];
    char     hd_cif_no[17];
    char     hd_instr_code[5];
    char     hd_decision_cd[11];
    char     hd_reason_cd[11];
    int64_t  hd_notional_amt;      /* minor units (×100) */
    int64_t  hd_limit_used_amt;      /* minor units (×100) */
    char     hd_decision_ts[15];
} hfdec_rec_t;

#endif /* HFDECC_H */
