/* SCAUDFC.h -- SCAUDF record layout (shared/pinned). org VSAM-ESDS. */
#ifndef SCAUDFC_H
#define SCAUDFC_H
#include <stdint.h>

typedef struct scaudf_rec {
    char     au_audit_id[11];
    char     au_order_id[11];
    char     au_event_kbn[3];
    char     au_cif_no[17];
    char     au_instr_code[5];
    char     au_event_ts[15];
    char     au_detail_cd[11];
} scaudf_rec_t;

#endif /* SCAUDFC_H */
