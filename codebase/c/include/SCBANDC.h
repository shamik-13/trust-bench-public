/* SCBANDC.h -- SCBAND record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCBANDC_H
#define SCBANDC_H
#include <stdint.h>

typedef struct scband_rec {
    char     bd_instr_code[5];
    int64_t  bd_lower_amt;      /* minor units (×100) */
    int64_t  bd_upper_amt;      /* minor units (×100) */
    char     bd_band_ts[15];
    char     bd_source_kbn[3];
} scband_rec_t;

#endif /* SCBANDC_H */
