/* SCHALTC.h -- SCHALT record layout (shared/pinned). org VSAM-ESDS. */
#ifndef SCHALTC_H
#define SCHALTC_H
#include <stdint.h>

typedef struct schalt_rec {
    char     ha_alert_id[11];
    char     ha_instr_code[5];
    char     ha_alert_kbn[3];
    char     ha_severity_cd[11];
    int64_t  ha_observed_amt;      /* minor units (×100) */
    int64_t  ha_limit_amt;      /* minor units (×100) */
    char     ha_event_ts[15];
} schalt_rec_t;

#endif /* SCHALTC_H */
