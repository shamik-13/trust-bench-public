/* SCRISKC.h -- SCRISK record layout (shared/pinned). org VSAM-ESDS. */
#ifndef SCRISKC_H
#define SCRISKC_H
#include <stdint.h>

typedef struct scrisk_rec {
    char     rk_event_id[11];
    char     rk_order_id[11];
    char     rk_cif_no[17];
    char     rk_instr_code[5];
    char     rk_risk_cd[11];
    char     rk_severity_kbn[3];
    int64_t  rk_observed_amt;      /* minor units (×100) */
    int64_t  rk_threshold_amt;      /* minor units (×100) */
    char     rk_event_ts[15];
} scrisk_rec_t;

#endif /* SCRISKC_H */
