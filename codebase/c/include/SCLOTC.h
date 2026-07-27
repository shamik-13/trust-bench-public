/* SCLOTC.h -- SCLOT record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCLOTC_H
#define SCLOTC_H
#include <stdint.h>

typedef struct sclot_rec {
    char     lt_lot_id[11];
    char     lt_cif_no[17];
    char     lt_instr_code[5];
    uint32_t lt_open_qty;
    int64_t  lt_open_amt;      /* minor units (×100) */
    char     lt_acq_ts[15];
    char     lt_src_exec_id[11];
} sclot_rec_t;

#endif /* SCLOTC_H */
