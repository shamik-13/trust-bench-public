/* PYNTFFC.h -- PYNTFF record layout (shared/pinned). org 順編成. */
#ifndef PYNTFFC_H
#define PYNTFFC_H
#include <stdint.h>

typedef struct pyntff_rec {
    char     nf_notice_id[11];
    char     nf_wallet_id[11];
    char     nf_notice_kbn[3];
    char     nf_notice_text[11];
    char     nf_send_status[3];
    char     nf_create_ts[15];
} pyntff_rec_t;

#endif /* PYNTFFC_H */
