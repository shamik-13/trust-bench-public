/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20200902  三宅 拓也 (E-241)  初版作成、板数量による需給不均衡計算を実装
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
#define MIHFT_BOOK_MAX 4096
#define MIHFT_SIDE_BUY 'B'
#define MIHFT_SIDE_SELL 'S'
#define MIHFT_DECISION_ACCEPT 0
#define MIHFT_ERR_IO 2
#define MIHFT_ERR_PARSE 3

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;
    char entry_ts[40];
} MihftBookRow;

typedef struct {
    int64_t price_amt;
    int64_t buy_cum_qty;
    int64_t sell_cum_qty;
    int64_t signed_imbalance;
    int64_t abs_imbalance;
    int64_t executable_qty;
    int64_t tie_distance;
} MihftCandidate;

static void trim_eol(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int parse_i64(const char *s, int64_t min_value, int64_t max_value, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
        return 0;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return 0;
    }
    if (v < min_value || v > max_value) {
        return 0;
    }

    *out = (int64_t)v;
    return 1;
}

static int parse_int(const char *s, int min_value, int max_value, int *out)
{
    int64_t v;

    if (!parse_i64(s, min_value, max_value, &v)) {
        return 0;
    }

    *out = (int)v;
    return 1;
}

static int parse_book_row(char *line, MihftBookRow *row)
{
    char *cols[7];
    char *p = line;
    int i;

    for (i = 0; i < 7; i++) {
        cols[i] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            if (i == 6) {
                break;
            }
            return 0;
        }
        *p++ = '\0';
    }
    if (i != 6 || strchr(cols[6], ',') != NULL) {
        return 0;
    }

    if (cols[0][0] == '\0' || strlen(cols[0]) >= sizeof(row->instr_code)) {
        return 0;
    }
    if (!((cols[1][0] == MIHFT_SIDE_BUY || cols[1][0] == MIHFT_SIDE_SELL) && cols[1][1] == '\0')) {
        return 0;
    }
    if (cols[6][0] == '\0' || strlen(cols[6]) >= sizeof(row->entry_ts)) {
        return 0;
    }

    memcpy(row->instr_code, cols[0], strlen(cols[0]) + 1);
    row->side_kbn = cols[1][0];
    memcpy(row->entry_ts, cols[6], strlen(cols[6]) + 1);

    if (!parse_int(cols[2], 1, INT_MAX, &row->level_cnt)) {
        return 0;
    }
    if (!parse_i64(cols[3], 1, INT64_MAX, &row->price_amt)) {
        return 0;
    }
    if (!parse_i64(cols[4], 0, INT64_MAX, &row->book_qty)) {
        return 0;
    }
    if (!parse_i64(cols[5], 0, INT64_MAX, &row->order_cnt)) {
        return 0;
    }

    if (row->book_qty > 0 && row->price_amt > MIHFT_MAX_NOTIONAL / row->book_qty) {
        return 0;
    }

    return 1;
}

static int same_instr_or_empty(const char *base, const char *next)
{
    return base[0] == '\0' || strcmp(base, next) == 0;
}

static int price_exists(const int64_t *prices, size_t count, int64_t price)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (prices[i] == price) {
            return 1;
        }
    }

    return 0;
}

static int cmp_i64_asc(const void *a, const void *b)
{
    const int64_t x = *(const int64_t *)a;
    const int64_t y = *(const int64_t *)b;

    return (x > y) - (x < y);
}

static int64_t abs_i64_checked(int64_t v)
{
    if (v == INT64_MIN) {
        return INT64_MAX;
    }
    return v < 0 ? -v : v;
}

static int add_i64_checked(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return 0;
    }
    *out = a + b;
    return 1;
}

static int calc_candidate(const MihftBookRow *rows, size_t row_count, int64_t price, MihftCandidate *cand)
{
    int64_t buy = 0;
    int64_t sell = 0;
    size_t i;

    for (i = 0; i < row_count; i++) {
        if (rows[i].side_kbn == MIHFT_SIDE_BUY && rows[i].price_amt >= price) {
            if (!add_i64_checked(buy, rows[i].book_qty, &buy)) {
                return 0;
            }
        } else if (rows[i].side_kbn == MIHFT_SIDE_SELL && rows[i].price_amt <= price) {
            if (!add_i64_checked(sell, rows[i].book_qty, &sell)) {
                return 0;
            }
        }
    }

    cand->price_amt = price;
    cand->buy_cum_qty = buy;
    cand->sell_cum_qty = sell;
    cand->signed_imbalance = buy - sell;
    cand->abs_imbalance = abs_i64_checked(cand->signed_imbalance);
    cand->executable_qty = buy < sell ? buy : sell;
    cand->tie_distance = abs_i64_checked(price);
    return 1;
}

static int better_candidate(const MihftCandidate *a, const MihftCandidate *b)
{
    if (a->abs_imbalance != b->abs_imbalance) {
        return a->abs_imbalance < b->abs_imbalance;
    }
    if (a->executable_qty != b->executable_qty) {
        return a->executable_qty > b->executable_qty;
    }
    if ((a->signed_imbalance >= 0) != (b->signed_imbalance >= 0)) {
        return a->signed_imbalance >= 0;
    }
    return a->price_amt < b->price_amt;
}

int main(void)
{
    MihftBookRow rows[MIHFT_BOOK_MAX];
    int64_t prices[MIHFT_BOOK_MAX];
    char line[MIHFT_LINE_MAX];
    char instr_code[32] = "";
    size_t row_count = 0;
    size_t price_count = 0;
    size_t line_no = 0;
    size_t i;
    MihftCandidate best;
    int have_best = 0;

    while (fgets(line, sizeof(line), stdin) != NULL) {
        MihftBookRow row;

        line_no++;
        trim_eol(line);

        if (line_no == 1 && strcmp(line, "INSTR-CODE,SIDE-KBN,LEVEL-CNT,PRICE-AMT,BOOK-QTY,ORDER-CNT,ENTRY-TS") == 0) {
            continue;
        }
        if (line[0] == '\0') {
            continue;
        }
        if (strlen(line) + 1 >= sizeof(line)) {
            fprintf(stderr, "入力行が長すぎます: %zu\n", line_no);
            return MIHFT_ERR_PARSE;
        }
        if (row_count >= MIHFT_BOOK_MAX) {
            fprintf(stderr, "板件数が上限を超過しました: %zu\n", line_no);
            return MIHFT_ERR_PARSE;
        }
        if (!parse_book_row(line, &row)) {
            fprintf(stderr, "入力形式不正: %zu\n", line_no);
            return MIHFT_ERR_PARSE;
        }
        if (!same_instr_or_empty(instr_code, row.instr_code)) {
            fprintf(stderr, "銘柄コード混在: %zu\n", line_no);
            return MIHFT_ERR_PARSE;
        }
        if (instr_code[0] == '\0') {
            memcpy(instr_code, row.instr_code, strlen(row.instr_code) + 1);
        }

        if (!price_exists(prices, price_count, row.price_amt)) {
            prices[price_count++] = row.price_amt;
        }
        rows[row_count++] = row;
    }

    if (ferror(stdin)) {
        fprintf(stderr, "入力読込失敗\n");
        return MIHFT_ERR_IO;
    }
    if (row_count == 0 || price_count == 0) {
        fprintf(stderr, "板入力なし\n");
        return MIHFT_ERR_PARSE;
    }

    qsort(prices, price_count, sizeof(prices[0]), cmp_i64_asc);

    printf("INSTR-CODE,CAND-PRICE,BUY-CUM-QTY,SELL-CUM-QTY,SIGNED-IMBALANCE,ABS-IMBALANCE,EXEC-QTY,TIE-RANK\n");

    for (i = 0; i < price_count; i++) {
        MihftCandidate cand;

        if (!calc_candidate(rows, row_count, prices[i], &cand)) {
            fprintf(stderr, "累積数量が上限を超過しました\n");
            return MIHFT_ERR_PARSE;
        }

        if (!have_best || better_candidate(&cand, &best)) {
            best = cand;
            have_best = 1;
        }

        printf("%s,%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
               instr_code,
               cand.price_amt,
               cand.buy_cum_qty,
               cand.sell_cum_qty,
               cand.signed_imbalance,
               cand.abs_imbalance,
               cand.executable_qty,
               cand.tie_distance);
    }

    printf("BEST,%s,%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
           instr_code,
           best.price_amt,
           best.buy_cum_qty,
           best.sell_cum_qty,
           best.signed_imbalance,
           best.abs_imbalance,
           best.executable_qty);

    return MIHFT_DECISION_ACCEPT;
}
