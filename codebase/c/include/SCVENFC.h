/* SCVENFC.h -- SCVENF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCVENFC_H
#define SCVENFC_H
#include <stdint.h>

typedef struct scvenf_rec {
    char     vn_venue_code[5];
    char     vn_board_code[5];
    char     vn_latency_us[11];
    char     vn_fee_bps[11];
    char     vn_enabled_kbn[3];
    uint32_t vn_capacity_qty;
} scvenf_rec_t;

#endif /* SCVENFC_H */
