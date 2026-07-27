/* PSFXRFC.h -- PSFXRF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PSFXRFC_H
#define PSFXRFC_H
#include <stdint.h>

typedef struct psfxrf_rec {
    char     fx_ccy_pair[11];
    double   fx_rate_dt;
    double   fx_ttm_rate;
    char     fx_source_cd[11];
    char     fx_load_status[3];
} psfxrf_rec_t;

#endif /* PSFXRFC_H */
