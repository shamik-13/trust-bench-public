/* SCMKTDC.h -- SCMKTD record layout (shared/pinned). org CSV. */
#ifndef SCMKTDC_H
#define SCMKTDC_H
#include <stdint.h>

typedef struct scmktd_rec {
    char     md_instr_code[5];
    int64_t  md_bid_amt;      /* minor units (×100) */
    int64_t  md_ask_amt;      /* minor units (×100) */
    int64_t  md_last_amt;      /* minor units (×100) */
    uint32_t md_vol_qty;
    char     md_tick_ts[15];
} scmktd_rec_t;

#endif /* SCMKTDC_H */
