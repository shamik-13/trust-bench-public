/* SCEXPRC.h -- SCEXPR record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCEXPRC_H
#define SCEXPRC_H
#include <stdint.h>

typedef struct scexpr_rec {
    char     xp_cif_no[17];
    char     xp_instr_code[5];
    int64_t  xp_net_notional_amt;      /* minor units (×100) */
    int64_t  xp_buy_open_amt;      /* minor units (×100) */
    int64_t  xp_sell_open_amt;      /* minor units (×100) */
    char     xp_updated_ts[15];
} scexpr_rec_t;

#endif /* SCEXPRC_H */
