/* SCDROPC.h -- SCDROP record layout (shared/pinned). org 順編成. */
#ifndef SCDROPC_H
#define SCDROPC_H
#include <stdint.h>

typedef struct scdrop_rec {
    char     dc_drop_id[11];
    char     dc_exec_id[11];
    char     dc_order_id[11];
    char     dc_cif_no[17];
    char     dc_instr_code[5];
    char     dc_side_kbn[3];
    uint32_t dc_fill_qty;
    int64_t  dc_fill_amt;      /* minor units (×100) */
    char     dc_send_ts[15];
} scdrop_rec_t;

#endif /* SCDROPC_H */
