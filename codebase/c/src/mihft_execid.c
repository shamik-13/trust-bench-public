/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20240213  市場基盤部  約定ID採番ベンチ初版
 */

#include "mihft_types.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#if defined(MIHFT_DECISION_ACCEPT)
#define MIHFT_EXECID_RC_OK MIHFT_DECISION_ACCEPT
#elif defined(MIHFT_DECISION_OK)
#define MIHFT_EXECID_RC_OK MIHFT_DECISION_OK
#elif defined(MIHFT_DECISION_PASS)
#define MIHFT_EXECID_RC_OK MIHFT_DECISION_PASS
#elif defined(MIHFT_OK)
#define MIHFT_EXECID_RC_OK MIHFT_OK
#else
#define MIHFT_EXECID_RC_OK 0
#endif

#if defined(MIHFT_DECISION_REJECT)
#define MIHFT_EXECID_RC_NG MIHFT_DECISION_REJECT
#elif defined(MIHFT_DECISION_ERROR)
#define MIHFT_EXECID_RC_NG MIHFT_DECISION_ERROR
#elif defined(MIHFT_ERROR)
#define MIHFT_EXECID_RC_NG MIHFT_ERROR
#else
#define MIHFT_EXECID_RC_NG 2
#endif

#define MIHFT_EXECID_SHARD_BITS 10u
#define MIHFT_EXECID_ORD_BITS 12u
#define MIHFT_EXECID_SHARD_MAX ((uint32_t)((1u << MIHFT_EXECID_SHARD_BITS) - 1u))
#define MIHFT_EXECID_ORD_MAX ((uint32_t)((1u << MIHFT_EXECID_ORD_BITS) - 1u))
#define MIHFT_EXECID_SEQ_MAX (UINT64_MAX >> (MIHFT_EXECID_SHARD_BITS + MIHFT_EXECID_ORD_BITS))

struct bench_case {
    uint64_t seq_no;
    uint32_t shard_id;
    uint32_t fill_ord;
    uint64_t prev_exec_id;
};

static int make_exec_id(uint64_t seq_no, uint32_t shard_id, uint32_t fill_ord, uint64_t *exec_id)
{
    uint64_t v;

    if (exec_id == NULL) {
        return -1;
    }
    if (seq_no == 0u || seq_no > MIHFT_EXECID_SEQ_MAX) {
        return -2;
    }
    if (shard_id > MIHFT_EXECID_SHARD_MAX) {
        return -3;
    }
    if (fill_ord == 0u || fill_ord > MIHFT_EXECID_ORD_MAX) {
        return -4;
    }

    v = seq_no;
    v <<= MIHFT_EXECID_SHARD_BITS;
    v |= (uint64_t)shard_id;
    v <<= MIHFT_EXECID_ORD_BITS;
    v |= (uint64_t)fill_ord;

    *exec_id = v;
    return 0;
}

static int run_bench(void)
{
    static const struct bench_case cases[] = {
        { 129840001u,  3u, 1u, 0u },
        { 129840001u,  3u, 2u, 0u },
        { 129840002u, 17u, 1u, 0u },
        { 129840105u, 17u, 1u, 0u },
        { 129840105u, 17u, 2u, 0u },
        { 129840106u, 18u, 1u, 0u },
        { 129840211u, 31u, 1u, 0u },
        { 129840212u, 31u, 1u, 0u }
    };
    uint64_t last_id = 0u;
    uint64_t xor_sum = 0u;
    uint64_t id = 0u;
    size_t i;

    for (i = 0u; i < sizeof(cases) / sizeof(cases[0]); i++) {
        if (make_exec_id(cases[i].seq_no, cases[i].shard_id, cases[i].fill_ord, &id) != 0) {
            fprintf(stderr, "E1001:約定ID採番入力不正\n");
            return MIHFT_EXECID_RC_NG;
        }
        if (cases[i].prev_exec_id != 0u && cases[i].prev_exec_id == id) {
            fprintf(stderr, "E1002:約定ID重複検知\n");
            return MIHFT_EXECID_RC_NG;
        }
        if (last_id != 0u && id <= last_id) {
            fprintf(stderr, "E1003:約定ID順序不正\n");
            return MIHFT_EXECID_RC_NG;
        }
        xor_sum ^= id;
        last_id = id;
    }

    if (xor_sum == 0u) {
        fprintf(stderr, "E1004:約定ID検証値不正\n");
        return MIHFT_EXECID_RC_NG;
    }

    printf("I0001:約定ID採番正常 件数=%lu 検証値=%016llX\n",
           (unsigned long)(sizeof(cases) / sizeof(cases[0])),
           (unsigned long long)xor_sum);

    return MIHFT_EXECID_RC_OK;
}

int main(void)
{
    int rc;

    rc = run_bench();
    if (rc != MIHFT_EXECID_RC_OK) {
        return rc;
    }

    return MIHFT_EXECID_RC_OK;
}
