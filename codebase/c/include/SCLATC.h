/* SCLATC.h -- SCLATF record layout (shared/pinned). org 順編成. */
#ifndef SCLATC_H
#define SCLATC_H
#include <stdint.h>

typedef struct sclatf_rec {
    char     lt_sample_id[11];
    char     lt_order_id[11];
    char     lt_stage_kbn[3];
    char     lt_start_ts[15];
    char     lt_end_ts[15];
    char     lt_latency_ns[11];
} sclatf_rec_t;

#endif /* SCLATC_H */
