/* PCKBNFC.h -- PCKBNF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PCKBNFC_H
#define PCKBNFC_H
#include <stdint.h>

typedef struct pckbnf_rec {
    char     kb_settle_kbn[3];
    char     kb_kbn_name[41];
    char     kb_nettable_flag;
    double   kb_fee_rate;
    char     kb_valid_from[11];
    char     kb_valid_to[11];
} pckbnf_rec_t;

#endif /* PCKBNFC_H */
