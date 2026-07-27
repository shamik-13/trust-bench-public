/* SCFEEFC.h -- SCFEEF record layout (shared/pinned). org CSV. */
#ifndef SCFEEFC_H
#define SCFEEFC_H
#include <stdint.h>

typedef struct scfeef_rec {
    char     fe_board_code[5];
    double   fe_fee_rate;
    int64_t  fe_min_fee_amt;      /* minor units (×100) */
} scfeef_rec_t;

#endif /* SCFEEFC_H */
