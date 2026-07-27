/* SCSETC.h -- SCSETF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCSETC_H
#define SCSETC_H
#include <stdint.h>

typedef struct scsetf_rec {
    char     st_settle_id[11];
    char     st_cif_no[17];
    char     st_instr_code[5];
    uint32_t st_settle_dt;      /* YYYYMMDD */
    uint32_t st_net_qty;
    int64_t  st_net_cash_amt;      /* minor units (×100) */
    char     st_status_kbn[3];
} scsetf_rec_t;

#endif /* SCSETC_H */
