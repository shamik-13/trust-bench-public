/* HFDROPQC.h -- HFDROPQ record layout (shared/pinned). org VSAM-ESDS. */
#ifndef HFDROPQC_H
#define HFDROPQC_H
#include <stdint.h>

typedef struct hfdropq_rec {
    char     hdq_drop_id[11];
    char     hdq_exec_id[11];
    char     hdq_order_id[11];
    char     hdq_instr_code[5];
    uint32_t hdq_fill_qty;
    int64_t  hdq_fill_amt;      /* minor units (×100) */
    char     hdq_capture_ts[15];
} hfdropq_rec_t;

#endif /* HFDROPQC_H */
