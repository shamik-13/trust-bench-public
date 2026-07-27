/* HFRATEC.h -- HFRATE record layout (shared/pinned). org VSAM-KSDS. */
#ifndef HFRATEC_H
#define HFRATEC_H
#include <stdint.h>

typedef struct hfrate_rec {
    char     hr_bucket_key[11];
    char     hr_window_ts[15];
    uint32_t hr_order_cnt;
    int64_t  hr_notional_amt;      /* minor units (×100) */
    uint32_t hr_drop_cnt;
} hfrate_rec_t;

#endif /* HFRATEC_H */
