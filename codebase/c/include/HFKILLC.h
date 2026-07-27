/* HFKILLC.h -- HFKILL record layout (shared/pinned). org VSAM-KSDS. */
#ifndef HFKILLC_H
#define HFKILLC_H
#include <stdint.h>

typedef struct hfkill_rec {
    char     hk_scope_key[11];
    char     hk_kill_flg[11];
    char     hk_reason_cd[11];
    char     hk_updated_ts[15];
    char     hk_updated_by[11];
} hfkill_rec_t;

#endif /* HFKILLC_H */
