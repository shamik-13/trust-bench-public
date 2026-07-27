/* SCREJTC.h -- SCREJTF record layout (shared/pinned). org VSAM-ESDS. */
#ifndef SCREJTC_H
#define SCREJTC_H
#include <stdint.h>

typedef struct screjtf_rec {
    char     rj_reject_id[11];
    char     rj_order_id[11];
    char     rj_cif_no[17];
    char     rj_instr_code[5];
    char     rj_reject_code[5];
    char     rj_reject_ts[15];
} screjtf_rec_t;

#endif /* SCREJTC_H */
