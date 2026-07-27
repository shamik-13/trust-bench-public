/* PSCONFC.h -- PSCONF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PSCONFC_H
#define PSCONFC_H
#include <stdint.h>

typedef struct psconf_rec {
    char     cf_conf_key[11];
    char     cf_conf_value[11];
    uint32_t cf_apply_dt;      /* YYYYMMDD */
    uint32_t cf_expire_dt;      /* YYYYMMDD */
    char     cf_updated_at[11];
} psconf_rec_t;

#endif /* PSCONFC_H */
