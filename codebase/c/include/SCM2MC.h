/* SCM2MC.h -- SCM2MF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCM2MC_H
#define SCM2MC_H
#include <stdint.h>

typedef struct scm2mf_rec {
    char     m2_cif_no[17];
    char     m2_instr_code[5];
    uint32_t m2_sess_dt;      /* YYYYMMDD */
    uint32_t m2_net_qty;
    int64_t  m2_mark_amt;      /* minor units (×100) */
    int64_t  m2_mark_notional_amt;      /* minor units (×100) */
    int64_t  m2_unrlzd_amt;      /* minor units (×100) */
} scm2mf_rec_t;

#endif /* SCM2MC_H */
