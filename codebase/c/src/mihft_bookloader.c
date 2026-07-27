/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20231114  大野 修 (E-225)  初版作成
 * 1.01  20240414  三宅 拓也 (E-241)  重複気配集約と段数欠落検査を追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_READ_BUFSZ 512
#define MIHFT_INIT_CAP 256
#define MIHFT_PATH_ENV "SCBOOK_PATH"

enum {
    MIHFT_RC_ACCEPT = 0,
    MIHFT_RC_PARSE = 1,
    MIHFT_RC_IO = 2,
    MIHFT_RC_MEMORY = 3,
    MIHFT_RC_REJECT_NOTIONAL = 8
};

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;
    int64_t entry_ts;
} MihftRawLevel;

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;
    int64_t first_ts;
    int64_t last_ts;
} MihftBookLevel;

typedef struct {
    MihftBookLevel *v;
    size_t n;
    size_t cap;
} MihftBookVec;

static int mihft_is_header(const char *s)
{
    return strncmp(s, "INSTR-CODE,", 11) == 0;
}

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_parse_i64(const char *s, int64_t minv, int64_t maxv, int64_t *out)
{
    char *endp;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || *endp != '\0' || v < minv || v > maxv) {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int mihft_copy_code(char *dst, size_t dstsz, const char *src)
{
    size_t n;

    if (src == NULL || *src == '\0') {
        return -1;
    }

    n = strlen(src);
    if (n >= dstsz) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static int mihft_parse_side(const char *s, char *out)
{
    if (s == NULL || s[1] != '\0' || (s[0] != 'B' && s[0] != 'S')) {
        return -1;
    }

    *out = s[0];
    return 0;
}

static int mihft_split7(char *line, char **f)
{
    size_t i = 0;
    char *p = line;

    while (i < 7) {
        f[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    return (i == 7 && strchr(f[6], ',') == NULL) ? 0 : -1;
}

static int mihft_parse_line(char *line, MihftRawLevel *out)
{
    char *f[7];
    int64_t v;

    mihft_chomp(line);
    if (mihft_split7(line, f) != 0) {
        return -1;
    }

    if (mihft_copy_code(out->instr_code, sizeof(out->instr_code), f[0]) != 0) {
        return -1;
    }
    if (mihft_parse_side(f[1], &out->side_kbn) != 0) {
        return -1;
    }
    if (mihft_parse_i64(f[2], 1, INT_MAX, &v) != 0) {
        return -1;
    }
    out->level_cnt = (int)v;
    if (mihft_parse_i64(f[3], 1, INT64_MAX, &out->price_amt) != 0) {
        return -1;
    }
    if (mihft_parse_i64(f[4], 0, INT64_MAX, &out->book_qty) != 0) {
        return -1;
    }
    if (mihft_parse_i64(f[5], 0, INT64_MAX, &out->order_cnt) != 0) {
        return -1;
    }
    if (mihft_parse_i64(f[6], 0, INT64_MAX, &out->entry_ts) != 0) {
        return -1;
    }

    return 0;
}

static int mihft_mul_over_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a != 0 && b > INT64_MAX / a) {
        return -1;
    }

    *out = a * b;
    return 0;
}

static int mihft_append(MihftBookVec *vec, const MihftRawLevel *src)
{
    MihftBookLevel *nv;
    size_t ncap;

    if (vec->n == vec->cap) {
        ncap = vec->cap == 0 ? MIHFT_INIT_CAP : vec->cap * 2;
        if (ncap < vec->cap) {
            return -1;
        }
        nv = (MihftBookLevel *)realloc(vec->v, ncap * sizeof(*vec->v));
        if (nv == NULL) {
            return -1;
        }
        vec->v = nv;
        vec->cap = ncap;
    }

    memcpy(vec->v[vec->n].instr_code, src->instr_code, sizeof(vec->v[vec->n].instr_code));
    vec->v[vec->n].side_kbn = src->side_kbn;
    vec->v[vec->n].level_cnt = src->level_cnt;
    vec->v[vec->n].price_amt = src->price_amt;
    vec->v[vec->n].book_qty = src->book_qty;
    vec->v[vec->n].order_cnt = src->order_cnt;
    vec->v[vec->n].first_ts = src->entry_ts;
    vec->v[vec->n].last_ts = src->entry_ts;
    vec->n++;

    return 0;
}

static int mihft_cmp_priority(const void *ap, const void *bp)
{
    const MihftBookLevel *a = (const MihftBookLevel *)ap;
    const MihftBookLevel *b = (const MihftBookLevel *)bp;
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
    if (a->level_cnt != b->level_cnt) {
        return a->level_cnt < b->level_cnt ? -1 : 1;
    }
    return 0;
}

static int mihft_same_key(const MihftBookLevel *a, const MihftBookLevel *b)
{
    return a->side_kbn == b->side_kbn &&
           a->price_amt == b->price_amt &&
           strcmp(a->instr_code, b->instr_code) == 0;
}

static int mihft_same_book(const MihftBookLevel *a, const MihftBookLevel *b)
{
    return a->side_kbn == b->side_kbn &&
           strcmp(a->instr_code, b->instr_code) == 0;
}

static int mihft_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if (b > 0 && a > INT64_MAX - b) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int mihft_normalize(MihftBookVec *vec)
{
    size_t r;
    size_t w = 0;

    if (vec->n == 0) {
        return 0;
    }

    qsort(vec->v, vec->n, sizeof(vec->v[0]), mihft_cmp_priority);

    for (r = 0; r < vec->n; r++) {
        if (w > 0 && mihft_same_key(&vec->v[w - 1], &vec->v[r])) {
            int64_t qty;
            int64_t cnt;

            if (mihft_add_i64(vec->v[w - 1].book_qty, vec->v[r].book_qty, &qty) != 0) {
                return -1;
            }
            if (mihft_add_i64(vec->v[w - 1].order_cnt, vec->v[r].order_cnt, &cnt) != 0) {
                return -1;
            }
            vec->v[w - 1].book_qty = qty;
            vec->v[w - 1].order_cnt = cnt;
            if (vec->v[r].level_cnt < vec->v[w - 1].level_cnt) {
                vec->v[w - 1].level_cnt = vec->v[r].level_cnt;
            }
            if (vec->v[r].first_ts < vec->v[w - 1].first_ts) {
                vec->v[w - 1].first_ts = vec->v[r].first_ts;
            }
            if (vec->v[r].last_ts > vec->v[w - 1].last_ts) {
                vec->v[w - 1].last_ts = vec->v[r].last_ts;
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

static int mihft_validate_levels(const MihftBookVec *vec)
{
    size_t i;
    int expect = 1;

    for (i = 0; i < vec->n; i++) {
        int64_t notional;

        if (i == 0 || !mihft_same_book(&vec->v[i - 1], &vec->v[i])) {
            expect = 1;
        }

        if (vec->v[i].level_cnt != expect) {
            fprintf(stderr, "段数欠落: 銘柄=%s 側=%c 期待=%d 実値=%d\n",
                    vec->v[i].instr_code, vec->v[i].side_kbn, expect, vec->v[i].level_cnt);
            return MIHFT_RC_PARSE;
        }

        if (mihft_mul_over_i64(vec->v[i].price_amt, vec->v[i].book_qty, &notional) != 0 ||
            notional > MIHFT_MAX_NOTIONAL) {
            fprintf(stderr, "想定元本超過: 銘柄=%s 側=%c 段=%d\n",
                    vec->v[i].instr_code, vec->v[i].side_kbn, vec->v[i].level_cnt);
            return MIHFT_RC_REJECT_NOTIONAL;
        }

        expect++;
    }

    return MIHFT_RC_ACCEPT;
}

static FILE *mihft_open_input(const char **used_path)
{
    const char *path = getenv(MIHFT_PATH_ENV);
    FILE *fp;

    if (path != NULL && *path != '\0') {
        *used_path = path;
        return fopen(path, "r");
    }

    fp = fopen("SCBOOK.csv", "r");
    if (fp != NULL) {
        *used_path = "SCBOOK.csv";
        return fp;
    }

    *used_path = "SCBOOK";
    return fopen("SCBOOK", "r");
}

int main(void)
{
    MihftBookVec book = {0};
    char line[MIHFT_READ_BUFSZ];
    const char *path = NULL;
    FILE *fp;
    unsigned long lineno = 0;
    int rc = MIHFT_RC_ACCEPT;

    fp = mihft_open_input(&path);
    if (fp == NULL) {
        fprintf(stderr, "入力オープン失敗: %s\n", path == NULL ? "SCBOOK" : path);
        return MIHFT_RC_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        MihftRawLevel raw;
        int64_t notional;

        lineno++;
        if (lineno == 1 && mihft_is_header(line)) {
            continue;
        }
        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fprintf(stderr, "入力行長超過: 行=%lu\n", lineno);
            rc = MIHFT_RC_PARSE;
            break;
        }
        if (mihft_parse_line(line, &raw) != 0) {
            fprintf(stderr, "入力形式不正: 行=%lu\n", lineno);
            rc = MIHFT_RC_PARSE;
            break;
        }
        if (mihft_mul_over_i64(raw.price_amt, raw.book_qty, &notional) != 0 ||
            notional > MIHFT_MAX_NOTIONAL) {
            fprintf(stderr, "想定元本超過: 行=%lu\n", lineno);
            rc = MIHFT_RC_REJECT_NOTIONAL;
            break;
        }
        if (mihft_append(&book, &raw) != 0) {
            fprintf(stderr, "領域確保失敗: 行=%lu\n", lineno);
            rc = MIHFT_RC_MEMORY;
            break;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "入力読込失敗: %s\n", path == NULL ? "SCBOOK" : path);
        rc = MIHFT_RC_IO;
    }

    if (fclose(fp) != 0 && rc == MIHFT_RC_ACCEPT) {
        fprintf(stderr, "入力クローズ失敗: %s\n", path == NULL ? "SCBOOK" : path);
        rc = MIHFT_RC_IO;
    }

    if (rc == MIHFT_RC_ACCEPT && mihft_normalize(&book) != 0) {
        fprintf(stderr, "気配集約失敗\n");
        rc = MIHFT_RC_MEMORY;
    }

    if (rc == MIHFT_RC_ACCEPT) {
        rc = mihft_validate_levels(&book);
    }

    free(book.v);
    return rc;
}
