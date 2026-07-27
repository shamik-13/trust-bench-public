/* SCEXPC.h -- SCEXPF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCEXPC_H
#define SCEXPC_H
#include <stdint.h>

typedef struct scexpf_rec {
    char     xp_cif_no[17];
    uint32_t xp_sess_dt;      /* YYYYMMDD */
    int64_t  xp_gross_long_amt;      /* minor units (×100) */
    int64_t  xp_gross_short_amt;      /* minor units (×100) */
    int64_t  xp_net_exposure_amt;      /* minor units (×100) */
    char     xp_limit_util_pct[11];
} scexpf_rec_t;

#endif /* SCEXPC_H */
