/* PYWALFC.h -- PYWALF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef PYWALFC_H
#define PYWALFC_H
#include <stdint.h>

typedef struct pywalf_rec {
    char     wl_wallet_id[11];
    char     wl_user_id[11];
    char     wl_wallet_status[3];
    char     wl_wallet_tier[11];
    char     wl_user_name_kana[41];
} pywalf_rec_t;

#endif /* PYWALFC_H */
