/* SCCALFC.h -- SCCALF record layout (shared/pinned). org CSV. */
#ifndef SCCALFC_H
#define SCCALFC_H
#include <stdint.h>

typedef struct sccalf_rec {
    uint32_t ca_sess_dt;      /* YYYYMMDD */
    char     ca_sess_kbn[3];
    char     ca_open_ts[15];
    char     ca_close_ts[15];
} sccalf_rec_t;

#endif /* SCCALFC_H */
