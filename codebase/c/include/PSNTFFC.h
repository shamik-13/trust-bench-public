/* PSNTFFC.h -- PSNTFF record layout (shared/pinned). org VSAM-ESDS. */
#ifndef PSNTFFC_H
#define PSNTFFC_H
#include <stdint.h>

typedef struct psntff_rec {
    char     ntf_notice_id[11];
    char     ntf_merchant_code[5];
    char     ntf_notice_kbn[3];
    char     ntf_settle_id[11];
    char     ntf_send_status[3];
    char     ntf_send_at[11];
} psntff_rec_t;

#endif /* PSNTFFC_H */
