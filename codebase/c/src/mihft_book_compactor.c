/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20190416  藤田 和也 (E-271)  SCBOOK板レベル圧縮の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_LINE_MAX 512
#define MIHFT_CODE_MAX 32
#define MIHFT_SIDE_MAX 2
#define MIHFT_TS_MAX 32

typedef struct {
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;
    char entry_ts[MIHFT_TS_MAX];
    size_t seq_no;
} scbook_row_t;

typedef struct {
    scbook_row_t *v;
    size_t n;
    size_t cap;
} scbook_vec_t;

static void chomp_line(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int is_header_line(const char *s)
{
    return strncmp(s, "INSTR-CODE,", 11) == 0;
}

static int copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *endp = NULL;
    long long v;

    if (*s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || endp == s || *endp != '\0') {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int parse_int(const char *s, int *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int split_csv7(char *line, char *f[7])
{
    int i = 0;
    char *p = line;

    for (;;) {
        if (i >= 7) {
            return -1;
        }
        f[i++] = p;

        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    return i == 7 ? 0 : -1;
}

static int parse_scbook_line(char *line, scbook_row_t *row, size_t seq_no)
{
    char *f[7];
    int64_t price;
    int64_t qty;
    int64_t orders;

    if (split_csv7(line, f) != 0) {
        return -1;
    }

    if (copy_field(row->instr_code, sizeof(row->instr_code), f[0]) != 0) {
        return -1;
    }

    if (strlen(f[1]) != 1 || (f[1][0] != 'B' && f[1][0] != 'S')) {
        return -1;
    }
    row->side_kbn = f[1][0];

    if (parse_int(f[2], &row->level_cnt) != 0 || row->level_cnt <= 0) {
        return -1;
    }

    if (parse_i64(f[3], &price) != 0 || price <= 0) {
        return -1;
    }

    if (parse_i64(f[4], &qty) != 0 || qty < 0) {
        return -1;
    }

    if (parse_i64(f[5], &orders) != 0 || orders < 0) {
        return -1;
    }

    if (qty == 0 && orders != 0) {
        return -1;
    }

    if (copy_field(row->entry_ts, sizeof(row->entry_ts), f[6]) != 0) {
        return -1;
    }

    row->price_amt = price;
    row->book_qty = qty;
    row->order_cnt = orders;
    row->seq_no = seq_no;
    return 0;
}

static int vec_push(scbook_vec_t *vec, const scbook_row_t *row)
{
    scbook_row_t *nv;
    size_t ncap;

    if (vec->n == vec->cap) {
        ncap = vec->cap == 0 ? 128u : vec->cap * 2u;
        if (ncap < vec->cap || ncap > SIZE_MAX / sizeof(*vec->v)) {
            return -1;
        }

        nv = (scbook_row_t *)realloc(vec->v, ncap * sizeof(*vec->v));
        if (nv == NULL) {
            return -1;
        }

        vec->v = nv;
        vec->cap = ncap;
    }

    vec->v[vec->n++] = *row;
    return 0;
}

static int cmp_key(const void *a, const void *b)
{
    const scbook_row_t *x = (const scbook_row_t *)a;
    const scbook_row_t *y = (const scbook_row_t *)b;
    int c = strcmp(x->instr_code, y->instr_code);

    if (c != 0) {
        return c;
    }

    if (x->side_kbn != y->side_kbn) {
        return x->side_kbn == 'B' ? -1 : 1;
    }

    if (x->price_amt != y->price_amt) {
        if (x->side_kbn == 'B') {
            return x->price_amt > y->price_amt ? -1 : 1;
        }
        return x->price_amt < y->price_amt ? -1 : 1;
    }

    if (x->seq_no != y->seq_no) {
        return x->seq_no < y->seq_no ? -1 : 1;
    }

    return 0;
}

static int same_level_key(const scbook_row_t *a, const scbook_row_t *b)
{
    return strcmp(a->instr_code, b->instr_code) == 0 &&
           a->side_kbn == b->side_kbn &&
           a->price_amt == b->price_amt;
}

static int add_i64_checked(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }

    *out = a + b;
    return 0;
}

static int compact_rows(scbook_vec_t *vec)
{
    size_t r;
    size_t w = 0;

    if (vec->n == 0) {
        return 0;
    }

    qsort(vec->v, vec->n, sizeof(*vec->v), cmp_key);

    for (r = 0; r < vec->n; r++) {
        if (vec->v[r].book_qty == 0) {
            continue;
        }

        if (w > 0 && same_level_key(&vec->v[w - 1], &vec->v[r])) {
            int64_t qty_sum;
            int64_t ord_sum;

            if (add_i64_checked(vec->v[w - 1].book_qty, vec->v[r].book_qty, &qty_sum) != 0 ||
                add_i64_checked(vec->v[w - 1].order_cnt, vec->v[r].order_cnt, &ord_sum) != 0) {
                return -1;
            }

            vec->v[w - 1].book_qty = qty_sum;
            vec->v[w - 1].order_cnt = ord_sum;
            if (strcmp(vec->v[r].entry_ts, vec->v[w - 1].entry_ts) < 0) {
                memcpy(vec->v[w - 1].entry_ts, vec->v[r].entry_ts, sizeof(vec->v[w - 1].entry_ts));
            }
        } else {
            if (w != r) {
                vec->v[w] = vec->v[r];
            }
            w++;
        }
    }

    vec->n = w;
    return 0;
}

static int emit_rows(const scbook_vec_t *vec)
{
    size_t i;
    int level = 0;
    const char *prev_instr = NULL;
    char prev_side = '\0';

    for (i = 0; i < vec->n; i++) {
        const scbook_row_t *row = &vec->v[i];

        if (prev_instr == NULL ||
            strcmp(prev_instr, row->instr_code) != 0 ||
            prev_side != row->side_kbn) {
            level = 1;
            prev_instr = row->instr_code;
            prev_side = row->side_kbn;
        } else {
            if (level == INT_MAX) {
                fprintf(stderr, "レベル番号が上限を超過しました\n");
                return -1;
            }
            level++;
        }

        if (printf("%s,%c,%d,%" PRId64 ",%" PRId64 ",%" PRId64 ",%s\n",
                   row->instr_code,
                   row->side_kbn,
                   level,
                   row->price_amt,
                   row->book_qty,
                   row->order_cnt,
                   row->entry_ts) < 0) {
            fprintf(stderr, "出力に失敗しました\n");
            return -1;
        }
    }

    if (fflush(stdout) != 0) {
        fprintf(stderr, "出力確定に失敗しました\n");
        return -1;
    }

    return 0;
}

int main(void)
{
    scbook_vec_t rows = {0};
    char line[MIHFT_LINE_MAX];
    size_t seq = 0;
    int rc = 0;

    while (fgets(line, sizeof(line), stdin) != NULL) {
        scbook_row_t row;

        if (strchr(line, '\n') == NULL && !feof(stdin)) {
            fprintf(stderr, "入力行が長すぎます\n");
            rc = 2;
            goto cleanup;
        }

        chomp_line(line);
        if (line[0] == '\0' || is_header_line(line)) {
            continue;
        }

        if (parse_scbook_line(line, &row, seq) != 0) {
            fprintf(stderr, "SCBOOK入力の解析に失敗しました\n");
            rc = 2;
            goto cleanup;
        }

        if (vec_push(&rows, &row) != 0) {
            fprintf(stderr, "作業領域の確保に失敗しました\n");
            rc = 3;
            goto cleanup;
        }

        seq++;
    }

    if (ferror(stdin)) {
        fprintf(stderr, "入力読込に失敗しました\n");
        rc = 2;
        goto cleanup;
    }

    if (compact_rows(&rows) != 0) {
        fprintf(stderr, "板数量の集約で桁あふれを検知しました\n");
        rc = 4;
        goto cleanup;
    }

    if (emit_rows(&rows) != 0) {
        rc = 5;
        goto cleanup;
    }

    rc = 0;

cleanup:
    free(rows.v);
    return rc;
}
