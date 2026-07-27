/* PYSCOFC.h -- PYSCOF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PYSCOFC_H
#define PYSCOFC_H
#include <stdint.h>

typedef struct pyscof_rec {
    char     sc_score_id[11];
    char     sc_wallet_id[11];
    char     sc_merchant_code[5];
    char     sc_risk_score[11];
    char     sc_score_reason[5];
    char     sc_score_as_of_ts[15];
} pyscof_rec_t;

#endif /* PYSCOFC_H */
