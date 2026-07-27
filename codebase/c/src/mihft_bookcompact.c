/* 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220222  藤田 和也 (E-271)   初版作成、SCBOOK板圧縮ホットパスを実装
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_LINE_MAX 512
#define MIHFT_FIELD_MAX 7
#define MIHFT_DEPTH_MAX 4096

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;
    char entry_ts[40];
} MihftLevel;

typedef struct {
    MihftLevel rows[MIHFT_DEPTH_MAX];
    size_t used;
    char instr_code[32];
    char side_kbn;
    int cap;
    int active;
} MihftBookWork;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_split_csv(char *line, char **fields, size_t max_fields)
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

    return (p == NULL) ? (int)count : -1;
}

static int mihft_parse_i64(const char *s, int64_t min_v, int64_t max_v, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }
    if ((int64_t)v < min_v || (int64_t)v > max_v) {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int mihft_parse_level(char *line, MihftLevel *out)
{
    char *fields[MIHFT_FIELD_MAX];
    int64_t v;

    mihft_chomp(line);
    if (mihft_split_csv(line, fields, MIHFT_FIELD_MAX) != MIHFT_FIELD_MAX) {
        return -1;
    }

    if (fields[0][0] == '\0' || strlen(fields[0]) >= sizeof(out->instr_code)) {
        return -1;
    }
    if (!((fields[1][0] == 'B' || fields[1][0] == 'S') && fields[1][1] == '\0')) {
        return -1;
    }

    if (mihft_parse_i64(fields[2], 0, MIHFT_DEPTH_MAX, &v) != 0) {
        return -1;
    }
    out->level_cnt = (int)v;

    if (mihft_parse_i64(fields[3], 0, INT64_MAX, &out->price_amt) != 0) {
        return -1;
    }
    if (mihft_parse_i64(fields[4], 0, INT64_MAX, &out->book_qty) != 0) {
        return -1;
    }
    if (mihft_parse_i64(fields[5], 0, INT64_MAX, &out->order_cnt) != 0) {
        return -1;
    }
    if (fields[6][0] == '\0' || strlen(fields[6]) >= sizeof(out->entry_ts)) {
        return -1;
    }

    strcpy(out->instr_code, fields[0]);
    out->side_kbn = fields[1][0];
    strcpy(out->entry_ts, fields[6]);

    return 0;
}

static int mihft_same_book(const MihftBookWork *book, const MihftLevel *row)
{
    return book->active &&
           book->side_kbn == row->side_kbn &&
           strcmp(book->instr_code, row->instr_code) == 0;
}

static int mihft_add_checked_i64(int64_t a, int64_t b, int64_t *out)
{
    if (b > 0 && a > INT64_MAX - b) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int mihft_abs_notional_ok(int64_t price_amt, int64_t book_qty)
{
    if (price_amt == 0 || book_qty == 0) {
        return 1;
    }
    if (price_amt > MIHFT_MAX_NOTIONAL / book_qty) {
        return 0;
    }
    return 1;
}

static int mihft_store_level(MihftBookWork *book, const MihftLevel *row)
{
    size_t i;

    if (row->book_qty == 0) {
        return 0;
    }
    if (!mihft_abs_notional_ok(row->price_amt, row->book_qty)) {
        return 8;
    }

    for (i = 0; i < book->used; i++) {
        if (book->rows[i].price_amt == row->price_amt) {
            int64_t qty_sum;
            int64_t cnt_sum;

            if (mihft_add_checked_i64(book->rows[i].book_qty, row->book_qty, &qty_sum) != 0 ||
                mihft_add_checked_i64(book->rows[i].order_cnt, row->order_cnt, &cnt_sum) != 0) {
                return -1;
            }

            book->rows[i].book_qty = qty_sum;
            book->rows[i].order_cnt = cnt_sum;
            if (strcmp(row->entry_ts, book->rows[i].entry_ts) < 0) {
                strcpy(book->rows[i].entry_ts, row->entry_ts);
            }
            return 0;
        }
    }

    if (book->used >= MIHFT_DEPTH_MAX) {
        return -1;
    }

    book->rows[book->used++] = *row;
    return 0;
}

static int mihft_cmp_bid(const void *a, const void *b)
{
    const MihftLevel *x = (const MihftLevel *)a;
    const MihftLevel *y = (const MihftLevel *)b;

    if (x->price_amt < y->price_amt) {
        return 1;
    }
    if (x->price_amt > y->price_amt) {
        return -1;
    }
    return strcmp(x->entry_ts, y->entry_ts);
}

static int mihft_cmp_ask(const void *a, const void *b)
{
    const MihftLevel *x = (const MihftLevel *)a;
    const MihftLevel *y = (const MihftLevel *)b;

    if (x->price_amt < y->price_amt) {
        return -1;
    }
    if (x->price_amt > y->price_amt) {
        return 1;
    }
    return strcmp(x->entry_ts, y->entry_ts);
}

static int mihft_flush_book(MihftBookWork *book)
{
    size_t i;
    size_t keep;

    if (!book->active) {
        return 0;
    }

    qsort(book->rows, book->used, sizeof(book->rows[0]),
          book->side_kbn == 'B' ? mihft_cmp_bid : mihft_cmp_ask);

    keep = book->used;
    if (book->cap >= 0 && keep > (size_t)book->cap) {
        keep = (size_t)book->cap;
    }

    for (i = 0; i < keep; i++) {
        if (printf("%s,%c,%d,%" PRId64 ",%" PRId64 ",%" PRId64 ",%s\n",
                   book->rows[i].instr_code,
                   book->rows[i].side_kbn,
                   (int)keep,
                   book->rows[i].price_amt,
                   book->rows[i].book_qty,
                   book->rows[i].order_cnt,
                   book->rows[i].entry_ts) < 0) {
            return -1;
        }
    }

    book->used = 0;
    book->active = 0;
    return 0;
}

static void mihft_start_book(MihftBookWork *book, const MihftLevel *row)
{
    memset(book, 0, sizeof(*book));
    strcpy(book->instr_code, row->instr_code);
    book->side_kbn = row->side_kbn;
    book->cap = row->level_cnt;
    book->active = 1;
}

int main(void)
{
    char line[MIHFT_LINE_MAX];
    MihftBookWork book;
    int rc = 0;

    memset(&book, 0, sizeof(book));

    while (fgets(line, sizeof(line), stdin) != NULL) {
        MihftLevel row;

        if (strchr(line, '\n') == NULL && !feof(stdin)) {
            fprintf(stderr, "入力行が長すぎます\n");
            return 2;
        }

        if (mihft_parse_level(line, &row) != 0) {
            fprintf(stderr, "SCBOOKの解析に失敗しました\n");
            return 2;
        }

        if (!mihft_same_book(&book, &row)) {
            if (mihft_flush_book(&book) != 0) {
                fprintf(stderr, "SCBOOKの出力に失敗しました\n");
                return 3;
            }
            mihft_start_book(&book, &row);
        }

        rc = mihft_store_level(&book, &row);
        if (rc < 0) {
            fprintf(stderr, "板集約で桁あふれを検出しました\n");
            return 4;
        }
        if (rc != 0) {
            return rc;
        }
    }

    if (ferror(stdin)) {
        fprintf(stderr, "SCBOOKの読込に失敗しました\n");
        return 2;
    }

    if (mihft_flush_book(&book) != 0 || fflush(stdout) != 0) {
        fprintf(stderr, "SCBOOKの出力に失敗しました\n");
        return 3;
    }

    return 0;
}
