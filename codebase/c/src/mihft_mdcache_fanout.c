/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240709  三宅 拓也 (E-241)    市場データキャッシュ配信の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MDCACHE_LINE_MAX          512u
#define MDCACHE_RING_BITS         12u
#define MDCACHE_RING_SIZE         (1u << MDCACHE_RING_BITS)
#define MDCACHE_RING_MASK         (MDCACHE_RING_SIZE - 1u)
#define MDCACHE_INST_BUCKETS      2048u
#define MDCACHE_INST_CODE_MAX     32u
#define MDCACHE_PRICE_SCALE_MAX   999999999999LL
#define MDCACHE_QTY_MAX           999999999999999LL
#define MDCACHE_TS_MAX            999999999999999999LL

#if defined(MIHFT_DECISION_OK)
#define MDCACHE_NORMAL_CODE MIHFT_DECISION_OK
#elif defined(MIHFT_DECISION_ACCEPT)
#define MDCACHE_NORMAL_CODE MIHFT_DECISION_ACCEPT
#elif defined(MIHFT_DECISION_PASS)
#define MDCACHE_NORMAL_CODE MIHFT_DECISION_PASS
#elif defined(MIHFT_DEC_OK)
#define MDCACHE_NORMAL_CODE MIHFT_DEC_OK
#else
#define MDCACHE_NORMAL_CODE 0
#endif

#define MDCACHE_ERR_IO     71
#define MDCACHE_ERR_PARSE  72
#define MDCACHE_ERR_RING   73

typedef struct {
    char instr_code[MDCACHE_INST_CODE_MAX];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    int64_t tick_ts;
} mdcache_scmktd_row;

typedef struct {
    char instr_code[MDCACHE_INST_CODE_MAX];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    int64_t tick_ts;
    uint32_t seq;
    uint32_t flags;
} mdcache_fanout_msg;

typedef struct {
    atomic_uint seq;
    mdcache_fanout_msg msg;
} mdcache_ring_slot;

typedef struct {
    char instr_code[MDCACHE_INST_CODE_MAX];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    int64_t tick_ts;
    bool used;
} mdcache_depth_state;

static mdcache_ring_slot g_ring[MDCACHE_RING_SIZE];
static atomic_uint g_ring_head = ATOMIC_VAR_INIT(0u);
static mdcache_depth_state g_depth[MDCACHE_INST_BUCKETS];

static void log_line(size_t line_no, const char *code)
{
    fprintf(stderr, "行=%zu コード=%s\n", line_no, code);
}

static uint32_t fnv1a32(const char *s)
{
    uint32_t h = 2166136261u;

    while (*s != '\0') {
        h ^= (unsigned char)*s;
        h *= 16777619u;
        ++s;
    }
    return h;
}

static char *trim_field(char *s)
{
    char *end;

    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n') {
        ++s;
    }

    end = s + strlen(s);
    while (end > s) {
        char c = end[-1];
        if (c != ' ' && c != '\t' && c != '\r' && c != '\n') {
            break;
        }
        --end;
    }
    *end = '\0';
    return s;
}

static bool parse_i64_bounded(const char *s, int64_t max_value, int64_t *out)
{
    char *end = NULL;
    intmax_t v;

    if (s == NULL || *s == '\0' || *s == '-' || *s == '+') {
        return false;
    }

    errno = 0;
    v = strtoimax(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return false;
    }
    if (v < 0 || v > max_value) {
        return false;
    }

    *out = (int64_t)v;
    return true;
}

static bool split_csv(char *line, char *fields[], size_t need)
{
    size_t n = 0;
    char *p = line;

    while (n < need) {
        fields[n++] = trim_field(p);
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p = '\0';
        ++p;
    }

    if (n != need) {
        return false;
    }
    if (strchr(fields[need - 1], ',') != NULL) {
        return false;
    }
    return true;
}

static bool looks_header(const char *line)
{
    return strstr(line, "INSTR-CODE") != NULL &&
           strstr(line, "BID-AMT") != NULL &&
           strstr(line, "ASK-AMT") != NULL;
}

static bool parse_scmktd(char *line, mdcache_scmktd_row *row)
{
    char *f[6];

    if (!split_csv(line, f, 6u)) {
        return false;
    }
    if (f[0][0] == '\0' || strlen(f[0]) >= sizeof(row->instr_code)) {
        return false;
    }

    memcpy(row->instr_code, f[0], strlen(f[0]) + 1u);
    if (!parse_i64_bounded(f[1], MDCACHE_PRICE_SCALE_MAX, &row->bid_amt)) {
        return false;
    }
    if (!parse_i64_bounded(f[2], MDCACHE_PRICE_SCALE_MAX, &row->ask_amt)) {
        return false;
    }
    if (!parse_i64_bounded(f[3], MDCACHE_PRICE_SCALE_MAX, &row->last_amt)) {
        return false;
    }
    if (!parse_i64_bounded(f[4], MDCACHE_QTY_MAX, &row->vol_qty)) {
        return false;
    }
    if (!parse_i64_bounded(f[5], MDCACHE_TS_MAX, &row->tick_ts)) {
        return false;
    }
    if (row->bid_amt > 0 && row->ask_amt > 0 && row->bid_amt > row->ask_amt) {
        return false;
    }

    return true;
}

static mdcache_depth_state *depth_slot_for(const char *instr_code)
{
    uint32_t h = fnv1a32(instr_code);
    uint32_t start = h & (MDCACHE_INST_BUCKETS - 1u);
    uint32_t i;

    for (i = 0u; i < MDCACHE_INST_BUCKETS; ++i) {
        mdcache_depth_state *slot = &g_depth[(start + i) & (MDCACHE_INST_BUCKETS - 1u)];

        if (!slot->used) {
            slot->used = true;
            memcpy(slot->instr_code, instr_code, strlen(instr_code) + 1u);
            return slot;
        }
        if (strcmp(slot->instr_code, instr_code) == 0) {
            return slot;
        }
    }

    return NULL;
}

static uint32_t depth_merge(const mdcache_scmktd_row *row)
{
    mdcache_depth_state *slot = depth_slot_for(row->instr_code);
    uint32_t flags = 0u;

    if (slot == NULL) {
        return 0x80000000u;
    }

    if (slot->bid_amt != row->bid_amt) {
        flags |= 0x00000001u;
    }
    if (slot->ask_amt != row->ask_amt) {
        flags |= 0x00000002u;
    }
    if (slot->last_amt != row->last_amt) {
        flags |= 0x00000004u;
    }
    if (slot->vol_qty != row->vol_qty) {
        flags |= 0x00000008u;
    }
    if (slot->tick_ts > row->tick_ts) {
        flags |= 0x40000000u;
    }

    slot->bid_amt = row->bid_amt;
    slot->ask_amt = row->ask_amt;
    slot->last_amt = row->last_amt;
    slot->vol_qty = row->vol_qty;
    slot->tick_ts = row->tick_ts;

    return flags;
}

static bool cache_distribute(const mdcache_scmktd_row *row, uint32_t flags)
{
    unsigned int seq = atomic_fetch_add_explicit(&g_ring_head, 1u, memory_order_relaxed);
    mdcache_ring_slot *slot = &g_ring[seq & MDCACHE_RING_MASK];
    unsigned int seen = atomic_load_explicit(&slot->seq, memory_order_acquire);

    if (seq >= MDCACHE_RING_SIZE && seen + MDCACHE_RING_SIZE > seq) {
        return false;
    }

    memcpy(slot->msg.instr_code, row->instr_code, strlen(row->instr_code) + 1u);
    slot->msg.bid_amt = row->bid_amt;
    slot->msg.ask_amt = row->ask_amt;
    slot->msg.last_amt = row->last_amt;
    slot->msg.vol_qty = row->vol_qty;
    slot->msg.tick_ts = row->tick_ts;
    slot->msg.seq = seq;
    slot->msg.flags = flags;

    atomic_store_explicit(&slot->seq, seq + 1u, memory_order_release);
    return true;
}

int main(void)
{
    char line[MDCACHE_LINE_MAX];
    size_t line_no = 0u;
    size_t accepted = 0u;

    while (fgets(line, sizeof(line), stdin) != NULL) {
        mdcache_scmktd_row row;
        uint32_t flags;

        ++line_no;
        if (line_no == 1u && looks_header(line)) {
            continue;
        }
        if (strchr(line, '\n') == NULL && !feof(stdin)) {
            log_line(line_no, "E-LINE");
            return MDCACHE_ERR_PARSE;
        }

        if (!parse_scmktd(line, &row)) {
            log_line(line_no, "E-PARSE");
            return MDCACHE_ERR_PARSE;
        }

        flags = depth_merge(&row);
        if ((flags & 0x80000000u) != 0u) {
            log_line(line_no, "E-DEPTH");
            return MDCACHE_ERR_RING;
        }
        if (!cache_distribute(&row, flags)) {
            log_line(line_no, "E-RING");
            return MDCACHE_ERR_RING;
        }

        ++accepted;
    }

    if (ferror(stdin)) {
        log_line(line_no, "E-IO");
        return MDCACHE_ERR_IO;
    }
    if (accepted == 0u) {
        log_line(line_no, "E-EMPTY");
        return MDCACHE_ERR_PARSE;
    }

    return MDCACHE_NORMAL_CODE;
}
