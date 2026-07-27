/* SCALRTFC.h -- SCALRTF record layout (shared/pinned). org VSAM-ESDS. */
#ifndef SCALRTFC_H
#define SCALRTFC_H
#include <stdint.h>

typedef struct scalrtf_rec {
    char     al_alert_id[11];
    char     al_alert_kbn[3];
    char     al_severity_code[5];
    char     al_subject_id[11];
    char     al_detail_code[5];
    char     al_raised_ts[15];
} scalrtf_rec_t;

#endif /* SCALRTFC_H */
