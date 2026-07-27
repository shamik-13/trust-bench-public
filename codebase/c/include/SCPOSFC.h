/* SCPOSFC.h -- SCPOSF record layout (shared/pinned). org CSV. */
#ifndef SCPOSFC_H
#define SCPOSFC_H
#include <stdint.h>

typedef struct scposf_rec {
    char     ps_cif_no[17];
    char     ps_instr_code[5];
    uint32_t ps_net_qty;
    int64_t  ps_avg_amt;      /* minor units (×100) */
    int64_t  ps_rlzd_amt;      /* minor units (×100) */
} scposf_rec_t;

#endif /* SCPOSFC_H */
