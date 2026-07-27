/* SCROUTC.h -- SCROUTEF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef SCROUTC_H
#define SCROUTC_H
#include <stdint.h>

typedef struct scroutef_rec {
    char     rt_route_key[11];
    char     rt_instr_code[5];
    char     rt_board_code[5];
    char     rt_venue_kbn[3];
    char     rt_priority_no[17];
    uint32_t rt_max_slice_qty;
    char     rt_enabled_flg[11];
} scroutef_rec_t;

#endif /* SCROUTC_H */
