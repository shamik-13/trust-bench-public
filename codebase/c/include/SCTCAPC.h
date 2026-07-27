/* SCTCAPC.h -- SCTCAP record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCTCAPC_H
#define SCTCAPC_H
#include <stdint.h>

typedef struct sctcap_rec {
    char     tc_trade_id[11];
    char     tc_exec_id[11];
    char     tc_order_id[11];
    char     tc_instr_code[5];
    char     tc_cif_no[17];
    uint32_t tc_trade_qty;
    int64_t  tc_trade_amt;      /* minor units (×100) */
    char     tc_capture_ts[15];
} sctcap_rec_t;

#endif /* SCTCAPC_H */
