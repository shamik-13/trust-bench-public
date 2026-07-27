/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20191022  西村 亮 (E-204)    初版作成。SCMKTD取込と単一銘柄スナップショット更新を実装。
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MIHFT_DECISION_TAKE
#define MIHFT_DECISION_TAKE 0
#endif

#ifndef MIHFT_DECISION_DROP_STALE
#define MIHFT_DECISION_DROP_STALE 10
#endif

#ifndef MIHFT_DECISION_LAST_ONLY
#define MIHFT_DECISION_LAST_ONLY 20
#endif

#ifndef MIHFT_DECISION_REJECT
#define MIHFT_DECISION_REJECT 30
#endif

#ifndef MIHFT_MAX_INSTR_CODE
#define MIHFT_MAX_INSTR_CODE 32
#endif

#ifndef MIHFT_MAX_CSV_LINE
#define MIHFT_MAX_CSV_LINE 512
#endif

#ifndef MIHFT_MAX_SPREAD_AMT_X100
#define MIHFT_MAX_SPREAD_AMT_X100 5000LL
#endif

#ifndef MIHFT_MAX_VOL_QTY
#define MIHFT_MAX_VOL_QTY 1000000000000LL
#endif

typedef struct {
    char instr_code[MIHFT_MAX_INSTR_CODE];
    int64_t bid_amt_x100;
    int64_t ask_amt_x100;
    int64_t last_amt_x100;
    int64_t vol_qty;
    int64_t tick_ts;
} scmmktd_record;

typedef struct {
    atomic_uint seq;
    char instr_code[MIHFT_MAX_INSTR_CODE];
    atomic_llong bid_amt_x100;
    atomic_llong ask_amt_x100;
    atomic_llong last_amt_x100;
    atomic_llong vol_qty;
    atomic_llong tick_ts;
    atomic_int last_only;
} market_snapshot;

static int parse_error(const char *msg, unsigned long line_no)
{
    fprintf(stderr, "E%04lu:%s\n", line_no, msg);
    return 2;
}

static void trim_eol(char *s)
{
    size_t n = strlen(s);

    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static bool is_header_line(const char *line)
{
    return strncmp(line, "INSTR-CODE,", 11U) == 0;
}

static bool parse_i64_field(const char *s, int64_t min_value, int64_t max_value, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
        return false;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno == ERANGE || end == s || *end != '\0') {
        return false;
    }
    if (v < min_value || v > max_value) {
        return false;
    }

    *out = (int64_t)v;
    return true;
}

static bool copy_instr_code(char *dst, size_t dst_len, const char *src)
{
    size_t n;

    if (src == NULL || *src == '\0' || dst_len == 0U) {
        return false;
    }

    n = strlen(src);
    if (n >= dst_len) {
        return false;
    }

    for (size_t i = 0U; i < n; i++) {
        unsigned char c = (unsigned char)src[i];

        if (!((c >= '0' && c <= '9') ||
              (c >= 'A' && c <= 'Z') ||
              c == '-' || c == '_' || c == '.')) {
            return false;
        }
    }

    memcpy(dst, src, n + 1U);
    return true;
}

static bool split_csv6(char *line, char *field[6])
{
    char *p = line;

    for (size_t i = 0U; i < 6U; i++) {
        field[i] = p;

        if (i == 5U) {
            return strchr(p, ',') == NULL;
        }

        p = strchr(p, ',');
        if (p == NULL) {
            return false;
        }
        *p++ = '\0';
    }

    return false;
}

static bool parse_scmmktd(char *line, scmmktd_record *rec)
{
    char *field[6];

    if (!split_csv6(line, field)) {
        return false;
    }

    if (!copy_instr_code(rec->instr_code, sizeof(rec->instr_code), field[0])) {
        return false;
    }
    if (!parse_i64_field(field[1], 1, LLONG_MAX, &rec->bid_amt_x100)) {
        return false;
    }
    if (!parse_i64_field(field[2], 1, LLONG_MAX, &rec->ask_amt_x100)) {
        return false;
    }
    if (!parse_i64_field(field[3], 1, LLONG_MAX, &rec->last_amt_x100)) {
        return false;
    }
    if (!parse_i64_field(field[4], 0, MIHFT_MAX_VOL_QTY, &rec->vol_qty)) {
        return false;
    }
    if (!parse_i64_field(field[5], 0, LLONG_MAX, &rec->tick_ts)) {
        return false;
    }

    return true;
}

static void snapshot_init(market_snapshot *snap)
{
    atomic_init(&snap->seq, 0U);
    snap->instr_code[0] = '\0';
    atomic_init(&snap->bid_amt_x100, 0);
    atomic_init(&snap->ask_amt_x100, 0);
    atomic_init(&snap->last_amt_x100, 0);
    atomic_init(&snap->vol_qty, 0);
    atomic_init(&snap->tick_ts, -1);
    atomic_init(&snap->last_only, 0);
}

static bool is_same_or_empty_symbol(const market_snapshot *snap, const scmmktd_record *rec)
{
    return snap->instr_code[0] == '\0' || strcmp(snap->instr_code, rec->instr_code) == 0;
}

static bool spread_is_abnormal(const scmmktd_record *rec)
{
    int64_t spread;

    if (rec->bid_amt_x100 > rec->ask_amt_x100) {
        return true;
    }

    spread = rec->ask_amt_x100 - rec->bid_amt_x100;
    return spread > MIHFT_MAX_SPREAD_AMT_X100;
}

static int update_snapshot(market_snapshot *snap, const scmmktd_record *rec)
{
    int64_t old_ts = atomic_load_explicit(&snap->tick_ts, memory_order_acquire);
    bool last_only;

    if (!is_same_or_empty_symbol(snap, rec)) {
        return MIHFT_DECISION_REJECT;
    }

    if (rec->tick_ts < old_ts) {
        return MIHFT_DECISION_DROP_STALE;
    }

    last_only = spread_is_abnormal(rec);

    atomic_fetch_add_explicit(&snap->seq, 1U, memory_order_acq_rel);
    if (snap->instr_code[0] == '\0') {
        memcpy(snap->instr_code, rec->instr_code, strlen(rec->instr_code) + 1U);
    }
    atomic_store_explicit(&snap->bid_amt_x100, rec->bid_amt_x100, memory_order_relaxed);
    atomic_store_explicit(&snap->ask_amt_x100, rec->ask_amt_x100, memory_order_relaxed);
    atomic_store_explicit(&snap->last_amt_x100, rec->last_amt_x100, memory_order_relaxed);
    atomic_store_explicit(&snap->vol_qty, rec->vol_qty, memory_order_relaxed);
    atomic_store_explicit(&snap->last_only, last_only ? 1 : 0, memory_order_relaxed);
    atomic_store_explicit(&snap->tick_ts, rec->tick_ts, memory_order_release);
    atomic_fetch_add_explicit(&snap->seq, 1U, memory_order_release);

    return last_only ? MIHFT_DECISION_LAST_ONLY : MIHFT_DECISION_TAKE;
}

int main(void)
{
    market_snapshot snap;
    char line[MIHFT_MAX_CSV_LINE];
    unsigned long line_no = 0UL;
    int last_decision = MIHFT_DECISION_REJECT;
    bool processed = false;

    snapshot_init(&snap);

    while (fgets(line, sizeof(line), stdin) != NULL) {
        scmmktd_record rec;

        line_no++;
        trim_eol(line);

        if (line[0] == '\0') {
            continue;
        }
        if (line_no == 1UL && is_header_line(line)) {
            continue;
        }
        if (strlen(line) >= sizeof(line) - 1U) {
            return parse_error("行長超過", line_no);
        }

        if (!parse_scmmktd(line, &rec)) {
            return parse_error("SCMKTD解析失敗", line_no);
        }

        last_decision = update_snapshot(&snap, &rec);
        processed = true;
    }

    if (ferror(stdin)) {
        return parse_error("標準入力読取失敗", line_no);
    }

    if (!processed) {
        fprintf(stderr, "E0000:SCMKTD未入力\n");
        return 2;
    }

    return last_decision;
}
