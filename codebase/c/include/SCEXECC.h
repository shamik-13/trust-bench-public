/* SCEXECC.h -- SCEXEC record layout (shared/pinned). org CSV. */
#ifndef SCEXECC_H
#define SCEXECC_H
#include <stdint.h>

typedef struct scexec_rec {
    char     ex_exec_id[11];
    char     ex_order_id[11];
    char     ex_instr_code[5];
    char     ex_side_kbn[3];
    uint32_t ex_fill_qty;
    int64_t  ex_fill_amt;      /* minor units (×100) */
    char     ex_exec_ts[15];
} scexec_rec_t;

#endif /* SCEXECC_H */
