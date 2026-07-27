/*
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  20200310  大野 修 (E-225)  初版作成
 * 1.01  20200810  今井 彩 (E-230)  呼値丸め後の気配交差判定を追加
 * 1.02  20210110  三宅 拓也 (E-241)  影響範囲のみ再整列する板更新に変更
 */
#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_RC_OK 0
#define MIHFT_RC_IO 20
#define MIHFT_RC_PARSE 24
#define MIHFT_RC_CAPACITY 28

#define MIHFT_SIDE_BID 'B'
#define MIHFT_SIDE_ASK 'S'
#define MIHFT_BOOK_MAX 4096
#define MIHFT_LINE_MAX 512
#define MIHFT_INSTR_MAX 32
#define MIHFT_TS_MAX 32

typedef struct {
    char instr[MIHFT_INSTR_MAX];
    char side;
    int level;
    int64_t price;
    int64_t qty;
    int order_count;
    char entry_ts[MIHFT_TS_MAX];
} MihftBookRow;

typedef struct {
    char instr[MIHFT_INSTR_MAX];
    int64_t bid;
    int64_t ask;
    int64_t last;
    int64_t volume;
    char tick_ts[MIHFT_TS_MAX];
} MihftMarketRow;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t n = strlen(src);
    if (n == 0U || n >= dst_sz) {
        return -1;
    }
    memcpy(dst, src, n + 1U);
    return 0;
}

static int mihft_parse_i64(const char *s, int64_t *out)
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
    *out = (int64_t)v;
    return 0;
}

static int mihft_parse_int(const char *s, int *out)
{
    int64_t v;

    if (mihft_parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int mihft_next_token(char **cursor, char **tok)
{
    char *p = *cursor;
    char *comma;

    if (p == NULL) {
        return -1;
    }
    comma = strchr(p, ',');
    if (comma != NULL) {
        *comma = '\0';
        *cursor = comma + 1;
    } else {
        *cursor = NULL;
    }
    *tok = p;
    return 0;
}

static int mihft_parse_book_line(char *line, MihftBookRow *row)
{
    char *cur = line;
    char *tok = NULL;

    if (mihft_next_token(&cur, &tok) != 0 || mihft_copy_field(row->instr, sizeof(row->instr), tok) != 0) {
        return -1;
    }
    if (mihft_next_token(&cur, &tok) != 0 || strlen(tok) != 1U || (tok[0] != MIHFT_SIDE_BID && tok[0] != MIHFT_SIDE_ASK)) {
        return -1;
    }
    row->side = tok[0];
    if (mihft_next_token(&cur, &tok) != 0 || mihft_parse_int(tok, &row->level) != 0 || row->level <= 0) {
        return -1;
    }
    if (mihft_next_token(&cur, &tok) != 0 || mihft_parse_i64(tok, &row->price) != 0 || row->price <= 0) {
        return -1;
    }
    if (mihft_next_token(&cur, &tok) != 0 || mihft_parse_i64(tok, &row->qty) != 0 || row->qty < 0) {
        return -1;
    }
    if (mihft_next_token(&cur, &tok) != 0 || mihft_parse_int(tok, &row->order_count) != 0 || row->order_count < 0) {
        return -1;
    }
    if (mihft_next_token(&cur, &tok) != 0 || cur != NULL || mihft_copy_field(row->entry_ts, sizeof(row->entry_ts), tok) != 0) {
        return -1;
    }
    return 0;
}

static int mihft_parse_market_line(char *line, MihftMarketRow *row)
{
    char *cur = line;
    char *tok = NULL;

    if (mihft_next_token(&cur, &tok) != 0 || mihft_copy_field(row->instr, sizeof(row->instr), tok) != 0) {
        return -1;
    }
    if (mihft_next_token(&cur, &tok) != 0 || mihft_parse_i64(tok, &row->bid) != 0 || row->bid <= 0) {
        return -1;
    }
    if (mihft_next_token(&cur, &tok) != 0 || mihft_parse_i64(tok, &row->ask) != 0 || row->ask <= 0) {
        return -1;
    }
    if (mihft_next_token(&cur, &tok) != 0 || mihft_parse_i64(tok, &row->last) != 0 || row->last < 0) {
        return -1;
    }
    if (mihft_next_token(&cur, &tok) != 0 || mihft_parse_i64(tok, &row->volume) != 0 || row->volume < 0) {
        return -1;
    }
    if (mihft_next_token(&cur, &tok) != 0 || cur != NULL || mihft_copy_field(row->tick_ts, sizeof(row->tick_ts), tok) != 0) {
        return -1;
    }
    return 0;
}

static int mihft_skip_header(const char *line, const char *head)
{
    return strncmp(line, head, strlen(head)) == 0;
}

static int mihft_read_book(const char *path, MihftBookRow *rows, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "SCBOOK入力を開けません\n");
        return MIHFT_RC_IO;
    }

    *count = 0U;
    while (fgets(line, sizeof(line), fp) != NULL) {
        mihft_chomp(line);
        if (line[0] == '\0' || mihft_skip_header(line, "INSTR-CODE,")) {
            continue;
        }
        if (*count >= MIHFT_BOOK_MAX) {
            fclose(fp);
            fprintf(stderr, "SCBOOK件数が上限を超過しました\n");
            return MIHFT_RC_CAPACITY;
        }
        if (mihft_parse_book_line(line, &rows[*count]) != 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOK形式不正\n");
            return MIHFT_RC_PARSE;
        }
        ++(*count);
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCBOOK読込失敗\n");
        return MIHFT_RC_IO;
    }
    fclose(fp);
    return MIHFT_RC_OK;
}

static int mihft_read_market(const char *path, MihftMarketRow *row)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];
    int found = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCMKTD入力を開けません\n");
        return MIHFT_RC_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        mihft_chomp(line);
        if (line[0] == '\0' || mihft_skip_header(line, "INSTR-CODE,")) {
            continue;
        }
        if (found != 0) {
            fclose(fp);
            fprintf(stderr, "SCMKTD件数不正\n");
            return MIHFT_RC_PARSE;
        }
        if (mihft_parse_market_line(line, row) != 0) {
            fclose(fp);
            fprintf(stderr, "SCMKTD形式不正\n");
            return MIHFT_RC_PARSE;
        }
        found = 1;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCMKTD読込失敗\n");
        return MIHFT_RC_IO;
    }
    fclose(fp);

    if (found == 0) {
        fprintf(stderr, "SCMKTD有効行なし\n");
        return MIHFT_RC_PARSE;
    }
    return MIHFT_RC_OK;
}

static int64_t mihft_tick_size(const char *instr)
{
    size_t n = strlen(instr);
    if (n > 0U) {
        char c = instr[n - 1U];
        if (c == '1') {
            return 100;
        }
        if (c == '2') {
            return 500;
        }
    }
    return 1000;
}

static int64_t mihft_normalize_price(int64_t price, int64_t tick, char side)
{
    int64_t rem = price % tick;

    if (rem == 0) {
        return price;
    }
    if (side == MIHFT_SIDE_BID) {
        return price - rem;
    }
    return price + (tick - rem);
}

static int mihft_find_best(const MihftBookRow *rows, size_t count, const char *instr, char side)
{
    size_t i;
    int best = -1;

    for (i = 0U; i < count; ++i) {
        if (strcmp(rows[i].instr, instr) == 0 && rows[i].side == side && rows[i].level == 1) {
            best = (int)i;
            break;
        }
    }
    return best;
}

static int mihft_side_cmp(const MihftBookRow *a, const MihftBookRow *b)
{
    if (a->side != b->side) {
        return (a->side == MIHFT_SIDE_BID) ? -1 : 1;
    }
    if (a->side == MIHFT_SIDE_BID) {
        if (a->price > b->price) {
            return -1;
        }
        if (a->price < b->price) {
            return 1;
        }
    } else {
        if (a->price < b->price) {
            return -1;
        }
        if (a->price > b->price) {
            return 1;
        }
    }
    return a->level - b->level;
}

static void mihft_rewrite_side_window(MihftBookRow *rows, size_t count, const char *instr, char side)
{
    size_t idx[MIHFT_BOOK_MAX];
    size_t n = 0U;
    size_t i;
    size_t j;

    for (i = 0U; i < count; ++i) {
        if (strcmp(rows[i].instr, instr) == 0 && rows[i].side == side) {
            idx[n++] = i;
        }
    }

    for (i = 1U; i < n; ++i) {
        size_t k = idx[i];
        j = i;
        while (j > 0U && mihft_side_cmp(&rows[k], &rows[idx[j - 1U]]) < 0) {
            idx[j] = idx[j - 1U];
            --j;
        }
        idx[j] = k;
    }

    for (i = 0U; i < n; ++i) {
        rows[idx[i]].level = (int)i + 1;
    }
}

static int mihft_merge_best(MihftBookRow *rows, size_t count, const MihftMarketRow *mkt)
{
    int bid_pos;
    int ask_pos;
    int64_t tick;
    int64_t bid;
    int64_t ask;

    tick = mihft_tick_size(mkt->instr);
    bid = mihft_normalize_price(mkt->bid, tick, MIHFT_SIDE_BID);
    ask = mihft_normalize_price(mkt->ask, tick, MIHFT_SIDE_ASK);

    if (bid <= 0 || ask <= 0 || bid >= ask) {
        return 12;
    }
    if (bid > MIHFT_MAX_NOTIONAL || ask > MIHFT_MAX_NOTIONAL) {
        return 8;
    }

    bid_pos = mihft_find_best(rows, count, mkt->instr, MIHFT_SIDE_BID);
    ask_pos = mihft_find_best(rows, count, mkt->instr, MIHFT_SIDE_ASK);
    if (bid_pos < 0 || ask_pos < 0) {
        fprintf(stderr, "最良気配なし\n");
        return MIHFT_RC_PARSE;
    }

    rows[bid_pos].price = bid;
    rows[bid_pos].qty = mkt->volume > 0 ? mkt->volume : rows[bid_pos].qty;
    rows[bid_pos].order_count = rows[bid_pos].order_count > 0 ? rows[bid_pos].order_count : 1;
    if (mihft_copy_field(rows[bid_pos].entry_ts, sizeof(rows[bid_pos].entry_ts), mkt->tick_ts) != 0) {
        return MIHFT_RC_PARSE;
    }

    rows[ask_pos].price = ask;
    rows[ask_pos].qty = mkt->volume > 0 ? mkt->volume : rows[ask_pos].qty;
    rows[ask_pos].order_count = rows[ask_pos].order_count > 0 ? rows[ask_pos].order_count : 1;
    if (mihft_copy_field(rows[ask_pos].entry_ts, sizeof(rows[ask_pos].entry_ts), mkt->tick_ts) != 0) {
        return MIHFT_RC_PARSE;
    }

    mihft_rewrite_side_window(rows, count, mkt->instr, MIHFT_SIDE_BID);
    mihft_rewrite_side_window(rows, count, mkt->instr, MIHFT_SIDE_ASK);

    return 0;
}

static int mihft_write_book(const char *path, const MihftBookRow *rows, size_t count)
{
    FILE *fp = fopen(path, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "SCBOOK出力を開けません\n");
        return MIHFT_RC_IO;
    }

    fprintf(fp, "INSTR-CODE,SIDE-KBN,LEVEL-CNT,PRICE-AMT,BOOK-QTY,ORDER-CNT,ENTRY-TS\n");
    for (i = 0U; i < count; ++i) {
        if (fprintf(fp, "%s,%c,%d,%lld,%lld,%d,%s\n",
                    rows[i].instr,
                    rows[i].side,
                    rows[i].level,
                    (long long)rows[i].price,
                    (long long)rows[i].qty,
                    rows[i].order_count,
                    rows[i].entry_ts) < 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOK出力失敗\n");
            return MIHFT_RC_IO;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCBOOK出力確定失敗\n");
        return MIHFT_RC_IO;
    }
    return MIHFT_RC_OK;
}

int main(void)
{
    MihftBookRow book[MIHFT_BOOK_MAX];
    MihftMarketRow market;
    size_t book_count = 0U;
    int rc;

    rc = mihft_read_book("SCBOOK.csv", book, &book_count);
    if (rc != MIHFT_RC_OK) {
        return rc;
    }

    rc = mihft_read_market("SCMKTD.csv", &market);
    if (rc != MIHFT_RC_OK) {
        return rc;
    }

    rc = mihft_merge_best(book, book_count, &market);
    if (rc != 0) {
        return rc;
    }

    rc = mihft_write_book("SCBOOK.out.csv", book, book_count);
    if (rc != MIHFT_RC_OK) {
        return rc;
    }

    return 0;
}
