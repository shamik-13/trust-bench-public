/* PJCANFC.h -- PJCANF record layout (shared/pinned). org VSAM-ESDS. */
#ifndef PJCANFC_H
#define PJCANFC_H
#include <stdint.h>

typedef struct pjcanf_rec {
    char     can_cancel_id[11];
    char     can_txn_id[11];
    char     can_merchant_code[5];
    int64_t  can_refund_amt;      /* minor units (×100) */
    uint32_t can_refund_dt;      /* YYYYMMDD */
    char     can_link_status[3];
} pjcanf_rec_t;

#endif /* PJCANFC_H */
