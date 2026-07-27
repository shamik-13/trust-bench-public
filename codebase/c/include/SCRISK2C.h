/* SCRISK2C.h -- SCRISK2 record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCRISK2C_H
#define SCRISK2C_H
#include <stdint.h>

typedef struct scrisk2_rec {
    char     rk_cif_no[17];
    char     rk_instr_tier[11];
    int64_t  rk_max_notional_amt;      /* minor units (×100) */
    uint32_t rk_max_qty;
    char     rk_kill_sw_kbn[3];
} scrisk2_rec_t;

#endif /* SCRISK2C_H */
