/* HFRJCTC.h -- HFRJCT record layout (shared/pinned). org VSAM-ESDS. */
#ifndef HFRJCTC_H
#define HFRJCTC_H
#include <stdint.h>

typedef struct hfrjct_rec {
    char     rj_reject_id[11];
    char     rj_order_id[11];
    char     rj_cif_no[17];
    char     rj_instr_code[5];
    char     rj_reject_cd[11];
    char     rj_detail_cd[11];
    char     rj_reject_ts[15];
} hfrjct_rec_t;

#endif /* HFRJCTC_H */
