/* SCSESSC.h -- SCSESSF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCSESSC_H
#define SCSESSC_H
#include <stdint.h>

typedef struct scsessf_rec {
    char     ss_sess_key[11];
    uint32_t ss_sess_dt;      /* YYYYMMDD */
    char     ss_board_code[5];
    char     ss_state_kbn[3];
    char     ss_last_seq_no[17];
    char     ss_updated_ts[15];
} scsessf_rec_t;

#endif /* SCSESSC_H */
