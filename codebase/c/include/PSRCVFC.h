/* PSRCVFC.h -- PSRCVF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PSRCVFC_H
#define PSRCVFC_H
#include <stdint.h>

typedef struct psrcvf_rec {
    char     rcv_receipt_id[11];
    char     rcv_merchant_code[5];
    int64_t  rcv_receipt_amt;      /* minor units (×100) */
    uint32_t rcv_receipt_dt;      /* YYYYMMDD */
    char     rcv_match_status[3];
    char     rcv_settle_id[11];
} psrcvf_rec_t;

#endif /* PSRCVFC_H */
