/* HFQUOTFC.h -- HFQUOTF record layout (shared/pinned). org VSAM-KSDS. */
#ifndef HFQUOTFC_H
#define HFQUOTFC_H
#include <stdint.h>

typedef struct hfquotf_rec {
    char     hqt_instr_code[5];
    int64_t  hqt_bid_amt;      /* minor units (×100) */
    int64_t  hqt_ask_amt;      /* minor units (×100) */
    int64_t  hqt_mid_amt;      /* minor units (×100) */
    int64_t  hqt_spread_amt;      /* minor units (×100) */
    char     hqt_quote_ts[15];
} hfquotf_rec_t;

#endif /* HFQUOTFC_H */
