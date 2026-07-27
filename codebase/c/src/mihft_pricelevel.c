/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220222  渡辺 隆 (E-260)  価格レベル正規化の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_FIELD_MAX 128
#define MIHFT_INSTR_MAX 32
#define MIHFT_TS_MAX 40
#define MIHFT_LEVEL_MAX 200000

typedef struct {
    char instr_code[MIHFT_INSTR_MAX];
    char side_kbn;
    int32_t level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;
    char entry_ts[MIHFT_TS_MAX];
} scbook_level;

static void trim_line(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int split_csv(char *line, char **fields, size_t max_fields)
{
    size_t count = 0;
    char *p = line;

    while (count < max_fields) {
        fields[count++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return count == max_fields && strchr(fields[max_fields - 1], ',') == NULL;
}

static int copy_field(char *dst, size_t dst_size, const char *src)
{
    size_t n = strlen(src);
    if (n == 0 || n >= dst_size) {
        return 0;
    }
    memcpy(dst, src, n + 1);
    return 1;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (*s == '\0') {
        return 0;
    }
    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || *end != '\0') {
        return 0;
    }
    *out = (int64_t)v;
    return 1;
}

static int parse_i32(const char *s, int32_t *out)
{
    int64_t v;
    if (!parse_i64(s, &v) || v < INT32_MIN || v > INT32_MAX) {
        return 0;
    }
    *out = (int32_t)v;
    return 1;
}

static int parse_side(const char *s, char *side)
{
    if ((s[0] == 'B' || s[0] == 'S') && s[1] == '\0') {
        *side = s[0];
        return 1;
    }
    return 0;
}

static int read_record(char *line, scbook_level *rec)
{
    char *f[7];

    trim_line(line);
    if (line[0] == '\0') {
        return 0;
    }
    if (!split_csv(line, f, 7)) {
        return -1;
    }
    if (!copy_field(rec->instr_code, sizeof(rec->instr_code), f[0])) {
        return -1;
    }
    if (!parse_side(f[1], &rec->side_kbn)) {
        return -1;
    }
    if (!parse_i32(f[2], &rec->level_cnt)) {
        return -1;
    }
    if (!parse_i64(f[3], &rec->price_amt) || rec->price_amt < 0) {
        return -1;
    }
    if (!parse_i64(f[4], &rec->book_qty) || rec->book_qty < 0) {
        return -1;
    }
    if (!parse_i64(f[5], &rec->order_cnt) || rec->order_cnt < 0) {
        return -1;
    }
    if (!copy_field(rec->entry_ts, sizeof(rec->entry_ts), f[6])) {
        return -1;
    }
    return 1;
}

static int same_level(const scbook_level *a, const scbook_level *b)
{
    return a->side_kbn == b->side_kbn &&
           a->price_amt == b->price_amt &&
           strcmp(a->instr_code, b->instr_code) == 0;
}

static int add_i64_checked(int64_t a, int64_t b, int64_t *out)
{
    if (b > 0 && a > INT64_MAX - b) {
        return 0;
    }
    *out = a + b;
    return 1;
}

static int consolidate(scbook_level *levels, size_t *count)
{
    size_t out = 0;

    for (size_t i = 0; i < *count; i++) {
        size_t pos = out;
        for (size_t j = 0; j < out; j++) {
            if (same_level(&levels[j], &levels[i])) {
                pos = j;
                break;
            }
        }
        if (pos == out) {
            levels[out++] = levels[i];
        } else {
            int64_t qty;
            int64_t orders;
            if (!add_i64_checked(levels[pos].book_qty, levels[i].book_qty, &qty)) {
                return 0;
            }
            if (!add_i64_checked(levels[pos].order_cnt, levels[i].order_cnt, &orders)) {
                return 0;
            }
            levels[pos].book_qty = qty;
            levels[pos].order_cnt = orders;
            if (strcmp(levels[pos].entry_ts, levels[i].entry_ts) < 0) {
                memcpy(levels[pos].entry_ts, levels[i].entry_ts, sizeof(levels[pos].entry_ts));
            }
        }
    }
    *count = out;
    return 1;
}

static int cmp_level(const void *lhs, const void *rhs)
{
    const scbook_level *a = (const scbook_level *)lhs;
    const scbook_level *b = (const scbook_level *)rhs;
    int c = strcmp(a->instr_code, b->instr_code);

    if (c != 0) {
        return c;
    }
    if (a->side_kbn != b->side_kbn) {
        return a->side_kbn == 'B' ? -1 : 1;
    }
    if (a->price_amt != b->price_amt) {
        if (a->side_kbn == 'B') {
            return a->price_amt > b->price_amt ? -1 : 1;
        }
        return a->price_amt < b->price_amt ? -1 : 1;
    }
    /* 集約後は (銘柄,サイド,価格) は一意。表示の安定化のため板数量で並べるのみで、
     * 同一価格帯の充当順序を表すものではない(約定選択は mihft_match 本体に従う)。 */
    if (a->book_qty != b->book_qty) {
        return a->book_qty > b->book_qty ? -1 : 1;
    }
    return 0;
}

static int check_notional(const scbook_level *rec)
{
    if (rec->book_qty != 0 && rec->price_amt > MIHFT_MAX_NOTIONAL / rec->book_qty) {
        return 8;
    }
    if (rec->price_amt * rec->book_qty > MIHFT_MAX_NOTIONAL) {
        return 8;
    }
    return 0;
}

static int write_records(scbook_level *levels, size_t count)
{
    char last_instr[MIHFT_INSTR_MAX] = "";
    char last_side = '\0';
    int32_t rank = 0;

    for (size_t i = 0; i < count; i++) {
        int decision = check_notional(&levels[i]);
        if (decision != 0) {
            return decision;
        }
        if (strcmp(last_instr, levels[i].instr_code) != 0 || last_side != levels[i].side_kbn) {
            if (!copy_field(last_instr, sizeof(last_instr), levels[i].instr_code)) {
                return 2;
            }
            last_side = levels[i].side_kbn;
            rank = 1;
        } else {
            if (rank == INT32_MAX) {
                return 2;
            }
            rank++;
        }
        levels[i].level_cnt = rank;
        if (printf("%s,%c,%" PRId32 ",%" PRId64 ",%" PRId64 ",%" PRId64 ",%s\n",
                   levels[i].instr_code,
                   levels[i].side_kbn,
                   levels[i].level_cnt,
                   levels[i].price_amt,
                   levels[i].book_qty,
                   levels[i].order_cnt,
                   levels[i].entry_ts) < 0) {
            return 2;
        }
    }
    if (fflush(stdout) == EOF) {
        return 2;
    }
    return 0;
}

int main(void)
{
    scbook_level *levels = NULL;
    size_t count = 0;
    size_t cap = 0;
    char line[1024];
    int result = 0;

    while (fgets(line, sizeof(line), stdin) != NULL) {
        scbook_level rec;
        int parsed = read_record(line, &rec);

        if (parsed < 0) {
            free(levels);
            fprintf(stderr, "CSV解析エラー\n");
            return 2;
        }
        if (parsed == 0) {
            continue;
        }
        if (count == MIHFT_LEVEL_MAX) {
            free(levels);
            fprintf(stderr, "入力件数上限超過\n");
            return 2;
        }
        if (count == cap) {
            size_t next = cap == 0 ? 1024 : cap * 2;
            scbook_level *tmp;
            if (next > MIHFT_LEVEL_MAX) {
                next = MIHFT_LEVEL_MAX;
            }
            tmp = (scbook_level *)realloc(levels, next * sizeof(*levels));
            if (tmp == NULL) {
                free(levels);
                fprintf(stderr, "メモリ確保エラー\n");
                return 2;
            }
            levels = tmp;
            cap = next;
        }
        levels[count++] = rec;
    }

    if (ferror(stdin)) {
        free(levels);
        fprintf(stderr, "入力読込エラー\n");
        return 2;
    }
    if (!consolidate(levels, &count)) {
        free(levels);
        fprintf(stderr, "集約数量オーバーフロー\n");
        return 2;
    }

    qsort(levels, count, sizeof(*levels), cmp_level);
    result = write_records(levels, count);
    free(levels);

    if (result == 2) {
        fprintf(stderr, "出力処理エラー\n");
    }
    return result;
}
