/* SCCUSTC.h -- SCCUST record layout (shared/pinned). org CSV. */
#ifndef SCCUSTC_H
#define SCCUSTC_H
#include <stdint.h>

typedef struct sccust_rec {
    char     cu_cif_no[17];
    int64_t  cu_group_limit;      /* minor units (×100) */
    int64_t  cu_group_used_amt;      /* minor units (×100) */
    int64_t  cu_acct_used_amt;      /* minor units (×100) */
} sccust_rec_t;

#endif /* SCCUSTC_H */
