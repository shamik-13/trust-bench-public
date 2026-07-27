/* SCORDFC.h -- SCORDF record layout (shared/pinned). org CSV. */
#ifndef SCORDFC_H
#define SCORDFC_H
#include <stdint.h>

typedef struct scordf_rec {
    char     or_order_id[11];
    char     or_cif_no[17];
    char     or_instr_code[5];
    char     or_side_kbn[3];
    char     or_ord_type[3];
    char     or_tif_code[5];
    uint32_t or_ord_qty;
    int64_t  or_price_amt;      /* minor units (×100) */
    char     or_instr_tier[11];
} scordf_rec_t;

#endif /* SCORDFC_H */
