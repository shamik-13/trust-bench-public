/* PTCAPFC.h -- PTCAPF record layout (shared/pinned). org 順編成. */
#ifndef PTCAPFC_H
#define PTCAPFC_H
#include <stdint.h>

typedef struct ptcapf_rec {
    char     cp_cap_id[11];
    char     cp_hold_id[11];
    char     cp_settle_txn_id[11];
    char     cp_merchant_code[5];
    char     cp_settle_kbn[3];
    int64_t  cp_cap_amt;      /* minor units (×100) */
} ptcapf_rec_t;

#endif /* PTCAPFC_H */
