/* SCHLDC.h -- SCHLDF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCHLDC_H
#define SCHLDC_H
#include <stdint.h>

typedef struct schldf_rec {
    char     hd_cif_no[17];
    char     hd_instr_code[5];
    uint32_t hd_asof_dt;      /* YYYYMMDD */
    uint32_t hd_settled_qty;
    uint32_t hd_trade_qty;
    uint32_t hd_restricted_qty;
} schldf_rec_t;

#endif /* SCHLDC_H */
