/* HFRISKCC.h -- HFRISKC record layout (shared/pinned). org VSAM-KSDS. */
#ifndef HFRISKCC_H
#define HFRISKCC_H
#include <stdint.h>

typedef struct hfriskc_rec {
    char     hrc_cif_no[17];
    char     hrc_instr_code[5];
    int64_t  hrc_open_notional_amt;      /* minor units (×100) */
    uint32_t hrc_reject_cnt;
    char     hrc_last_upd_ts[15];
} hfriskc_rec_t;

#endif /* HFRISKCC_H */
