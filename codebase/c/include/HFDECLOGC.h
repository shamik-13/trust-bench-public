/* HFDECLOGC.h -- HFDECLOG record layout (shared/pinned). org VSAM-ESDS. */
#ifndef HFDECLOGC_H
#define HFDECLOGC_H
#include <stdint.h>

typedef struct hfdeclog_rec {
    char     hdl_decision_id[11];
    char     hdl_order_id[11];
    char     hdl_instr_code[5];
    char     hdl_action_code[5];
    char     hdl_reason_code[5];
    char     hdl_decision_ts[15];
} hfdeclog_rec_t;

#endif /* HFDECLOGC_H */
