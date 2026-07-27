/* PTKEYFC.h -- PTKEYF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PTKEYFC_H
#define PTKEYFC_H
#include <stdint.h>

typedef struct ptkeyf_rec {
    char     tk_trace_key[11];
    char     tk_hold_id[11];
    char     tk_cap_id[11];
    char     tk_settle_txn_id[11];
    char     tk_merchant_code[5];
    char     tk_check_result[11];
} ptkeyf_rec_t;

#endif /* PTKEYFC_H */
