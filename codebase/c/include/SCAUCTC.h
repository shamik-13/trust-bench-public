/* SCAUCTC.h -- SCAUCT record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCAUCTC_H
#define SCAUCTC_H
#include <stdint.h>

typedef struct scauct_rec {
    char     ax_instr_code[5];
    char     ax_auction_kbn[3];
    int64_t  ax_cross_amt;      /* minor units (×100) */
    uint32_t ax_imbal_qty;
    uint32_t ax_match_qty;
    char     ax_calc_ts[15];
} scauct_rec_t;

#endif /* SCAUCTC_H */
