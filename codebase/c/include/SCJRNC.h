/* SCJRNC.h -- SCJRNF record layout (shared/pinned). org VSAM-ESDS. */
#ifndef SCJRNC_H
#define SCJRNC_H
#include <stdint.h>

typedef struct scjrnf_rec {
    char     jr_seq_no[17];
    char     jr_event_ts[15];
    char     jr_event_kbn[3];
    char     jr_order_id[11];
    char     jr_instr_code[5];
    char     jr_payload_hash[11];
} scjrnf_rec_t;

#endif /* SCJRNC_H */
