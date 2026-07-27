/* SCCACC.h -- SCCACT record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCCACC_H
#define SCCACC_H
#include <stdint.h>

typedef struct sccact_rec {
    char     ac_action_id[11];
    char     ac_instr_code[5];
    uint32_t ac_ex_dt;      /* YYYYMMDD */
    char     ac_action_kbn[3];
    char     ac_ratio_num[11];
    char     ac_ratio_den[11];
    int64_t  ac_cash_amt;      /* minor units (×100) */
} sccact_rec_t;

#endif /* SCCACC_H */
