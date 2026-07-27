/* SCRISKF2C.h -- SCRISKF2 record layout (shared/pinned). org VSAM-ESDS. */
#ifndef SCRISKF2C_H
#define SCRISKF2C_H
#include <stdint.h>

typedef struct scriskf2_rec {
    char     rk_risk_event_id[11];
    char     rk_cif_no[17];
    char     rk_instr_code[5];
    char     rk_event_ts[15];
    int64_t  rk_limit_amt;      /* minor units (×100) */
    int64_t  rk_used_amt;      /* minor units (×100) */
    char     rk_decision_kbn[3];
} scriskf2_rec_t;

#endif /* SCRISKF2C_H */
