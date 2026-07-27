/* SCBOOKC.h -- SCBOOK record layout (shared/pinned). org CSV. */
#ifndef SCBOOKC_H
#define SCBOOKC_H
#include <stdint.h>

typedef struct scbook_rec {
    char     bk_instr_code[5];
    char     bk_side_kbn[3];
    uint32_t bk_level_cnt;
    int64_t  bk_price_amt;      /* minor units (×100) */
    uint32_t bk_book_qty;
    uint32_t bk_order_cnt;
    char     bk_entry_ts[15];
} scbook_rec_t;

#endif /* SCBOOKC_H */
