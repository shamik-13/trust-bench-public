/* SCKILLC.h -- SCKILLF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCKILLC_H
#define SCKILLC_H
#include <stdint.h>

typedef struct sckillf_rec {
    char     kl_kill_key[11];
    char     kl_scope_kbn[3];
    char     kl_instr_code[5];
    char     kl_cif_no[17];
    char     kl_active_flg[11];
    char     kl_reason_code[5];
    char     kl_updated_ts[15];
} sckillf_rec_t;

#endif /* SCKILLC_H */
