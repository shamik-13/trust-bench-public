/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20190416  三宅 拓也 (E-241)  約定重複排除ベンチマーク初版
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_RING_SIZE 1024u
#define MIHFT_LINE_MAX 512u
#define MIHFT_DECISION_HOLD_DUP 16
#define MIHFT_ERR_IO 2
#define MIHFT_ERR_PARSE 3
#define MIHFT_ERR_RANGE 4

struct scexec_record {
    char exec_id[32];
    char order_id[32];
    char instr_code[24];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    int64_t exec_ts;
};

struct dedupe_slot {
    unsigned char used;
    uint64_t hash;
    char exec_id[32];
    char order_id[32];
    int64_t fill_qty;
    int64_t fill_amt;
    int64_t exec_ts;
};

static uint64_t
fnv1a_bytes(uint64_t h, const unsigned char *p)
{
    while (*p != '\0') {
        h ^= (uint64_t)*p++;
        h *= UINT64_C(1099511628211);
    }
    return h;
}

static uint64_t
make_key_hash(const char *exec_id, const char *order_id)
{
    uint64_t h = UINT64_C(1469598103934665603);
    h = fnv1a_bytes(h, (const unsigned char *)exec_id);
    h ^= UINT64_C(0xff);
    h *= UINT64_C(1099511628211);
    h = fnv1a_bytes(h, (const unsigned char *)order_id);
    return h;
}

static int
copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);
    if (n == 0 || n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1u);
    return 0;
}

static int
parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (*s == '\0') {
        return -1;
    }
    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }
    *out = (int64_t)v;
    return 0;
}

static char *
trim_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0u && (s[n - 1u] == '\n' || s[n - 1u] == '\r')) {
        s[--n] = '\0';
    }
    return s;
}

static int
split_csv7(char *line, char *field[7])
{
    size_t i = 0u;
    char *p = line;

    while (i < 7u) {
        field[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return (i == 7u && strchr(field[6], ',') == NULL) ? 0 : -1;
}

static int
is_header(const char *s)
{
    return strcmp(s, "EXEC-ID") == 0;
}

static int
parse_scexec(char *line, struct scexec_record *rec)
{
    char *f[7];

    trim_eol(line);
    if (split_csv7(line, f) != 0) {
        return -1;
    }
    if (copy_field(rec->exec_id, sizeof(rec->exec_id), f[0]) != 0 ||
        copy_field(rec->order_id, sizeof(rec->order_id), f[1]) != 0 ||
        copy_field(rec->instr_code, sizeof(rec->instr_code), f[2]) != 0) {
        return -1;
    }
    if ((strcmp(f[3], "B") != 0) && (strcmp(f[3], "S") != 0)) {
        return -1;
    }
    rec->side_kbn = f[3][0];
    if (parse_i64(f[4], &rec->fill_qty) != 0 ||
        parse_i64(f[5], &rec->fill_amt) != 0 ||
        parse_i64(f[6], &rec->exec_ts) != 0) {
        return -1;
    }
    if (rec->fill_qty <= 0 || rec->fill_amt <= 0 || rec->exec_ts <= 0) {
        return -1;
    }
    return 0;
}

static int
notional_over_limit(int64_t amt)
{
    return amt > (int64_t)MIHFT_MAX_NOTIONAL;
}

static int
dedupe_check(struct dedupe_slot ring[MIHFT_RING_SIZE], const struct scexec_record *rec)
{
    uint64_t h = make_key_hash(rec->exec_id, rec->order_id);
    size_t pos = (size_t)(h & (uint64_t)(MIHFT_RING_SIZE - 1u));
    struct dedupe_slot *slot = &ring[pos];

    if (slot->used != 0u) {
        int same_key = (slot->hash == h &&
                        strcmp(slot->exec_id, rec->exec_id) == 0 &&
                        strcmp(slot->order_id, rec->order_id) == 0);
        if (same_key) {
            if (slot->exec_ts == rec->exec_ts &&
                slot->fill_qty == rec->fill_qty &&
                slot->fill_amt == rec->fill_amt) {
                return 1;
            }
            return MIHFT_DECISION_HOLD_DUP;
        }
        if (slot->exec_ts == rec->exec_ts &&
            slot->fill_qty == rec->fill_qty &&
            slot->fill_amt == rec->fill_amt) {
            return MIHFT_DECISION_HOLD_DUP;
        }
    }

    slot->used = 1u;
    slot->hash = h;
    memcpy(slot->exec_id, rec->exec_id, sizeof(slot->exec_id));
    memcpy(slot->order_id, rec->order_id, sizeof(slot->order_id));
    slot->fill_qty = rec->fill_qty;
    slot->fill_amt = rec->fill_amt;
    slot->exec_ts = rec->exec_ts;
    return 0;
}

int
main(void)
{
    struct dedupe_slot ring[MIHFT_RING_SIZE];
    char line[MIHFT_LINE_MAX];
    int decision = 0;
    unsigned long row = 0ul;

    memset(ring, 0, sizeof(ring));

    while (fgets(line, sizeof(line), stdin) != NULL) {
        struct scexec_record rec;
        int d;

        ++row;
        if (strchr(line, '\n') == NULL && !feof(stdin)) {
            fprintf(stderr, "入力行が長すぎます: %lu\n", row);
            return MIHFT_ERR_PARSE;
        }
        if (row == 1ul && is_header(line)) {
            continue;
        }
        if (parse_scexec(line, &rec) != 0) {
            fprintf(stderr, "約定ＣＳＶ解析に失敗しました: %lu\n", row);
            return MIHFT_ERR_PARSE;
        }
        if (notional_over_limit(rec.fill_amt)) {
            decision = 8;
            continue;
        }

        d = dedupe_check(ring, &rec);
        if (d == MIHFT_DECISION_HOLD_DUP) {
            decision = MIHFT_DECISION_HOLD_DUP;
            continue;
        }
        if (d == 1) {
            continue;
        }
    }

    if (ferror(stdin)) {
        fprintf(stderr, "標準入力の読込に失敗しました\n");
        return MIHFT_ERR_IO;
    }
    if (row == 0ul) {
        fprintf(stderr, "約定入力が空です\n");
        return MIHFT_ERR_PARSE;
    }

    return decision;
}
