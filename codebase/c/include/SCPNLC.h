/* SCPNLC.h -- SCPNLF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCPNLC_H
#define SCPNLC_H
#include <stdint.h>

typedef struct scpnlf_rec {
    char     pn_cif_no[17];
    char     pn_instr_code[5];
    uint32_t pn_sess_dt;      /* YYYYMMDD */
    int64_t  pn_rlzd_amt;      /* minor units (×100) */
    int64_t  pn_unrlzd_amt;      /* minor units (×100) */
    int64_t  pn_fee_amt;      /* minor units (×100) */
    char     pn_calc_ts[15];
} scpnlf_rec_t;

#endif /* SCPNLC_H */
