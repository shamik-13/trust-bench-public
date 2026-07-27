/* SCREJC.h -- SCREJ record layout (shared/pinned). org 順編成. */
#ifndef SCREJC_H
#define SCREJC_H
#include <stdint.h>

typedef struct screj_rec {
    char     rj_reject_id[11];
    char     rj_order_id[11];
    char     rj_cif_no[17];
    char     rj_instr_code[5];
    char     rj_reject_cd[11];
    char     rj_reject_ts[15];
} screj_rec_t;

#endif /* SCREJC_H */
