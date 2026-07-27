/* SCORDSC.h -- SCORDS record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCORDSC_H
#define SCORDSC_H
#include <stdint.h>

typedef struct scords_rec {
    char     os_order_id[11];
    char     os_cif_no[17];
    char     os_instr_code[5];
    char     os_state_kbn[3];
    uint32_t os_leaves_qty;
    uint32_t os_cum_qty;
    int64_t  os_avg_fill_amt;      /* minor units (×100) */
    char     os_last_upd_ts[15];
} scords_rec_t;

#endif /* SCORDSC_H */
