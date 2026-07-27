/* SCLMTFC.h -- SCLMTF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCLMTFC_H
#define SCLMTFC_H
#include <stdint.h>

typedef struct sclmtf_rec {
    char     lm_cif_no[17];
    char     lm_instr_tier[11];
    int64_t  lm_max_notional_amt;      /* minor units (×100) */
    uint32_t lm_max_order_qty;
    double   lm_max_rate_cnt;
    char     lm_updated_ts[15];
} sclmtf_rec_t;

#endif /* SCLMTFC_H */
