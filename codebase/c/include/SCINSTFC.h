/* SCINSTFC.h -- SCINSTF record layout (shared/pinned). org CSV. */
#ifndef SCINSTFC_H
#define SCINSTFC_H
#include <stdint.h>

typedef struct scinstf_rec {
    char     in_instr_code[5];
    char     in_instr_name[41];
    char     in_instr_tier[11];
    int64_t  in_tick_amt;      /* minor units (×100) */
    uint32_t in_lot_qty;
    char     in_board_code[5];
} scinstf_rec_t;

#endif /* SCINSTFC_H */
