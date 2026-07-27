/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20191022  三宅 拓也 (E-241)  初版作成
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define RC_ACCEPT 0
#define RC_REJECT_MARGIN 4
#define RC_REJECT_NOTIONAL 8
#define RC_REJECT_TICK 12
#define RC_HARD_ERROR 64

#define MAX_LINE 512
#define MAX_BOOK 256
#define MAX_MARKET 128
#define LOT_QTY 100

typedef struct {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} OrderRec;

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;
    char entry_ts[32];
} BookRec;

typedef struct {
    char instr_code[32];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    char tick_ts[32];
} MarketRec;

static void trim_field(char *s)
{
    char *p = s;
    char *e;

    while (isspace((unsigned char)*p)) {
        ++p;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1);
    }

    e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) {
        --e;
    }
    *e = '\0';
}

static int split_csv(char *line, char **field, size_t need)
{
    size_t n = 0;
    char *p = line;

    while (n < need) {
        field[n++] = p;
        while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
            ++p;
        }
        if (*p == ',') {
            *p++ = '\0';
            continue;
        }
        if (*p == '\n' || *p == '\r') {
            *p = '\0';
        }
        break;
    }

    if (n != need) {
        return -1;
    }

    while (*p != '\0') {
        if (*p != '\n' && *p != '\r' && *p != ' ' && *p != '\t') {
            return -1;
        }
        ++p;
    }

    for (size_t i = 0; i < need; ++i) {
        trim_field(field[i]);
    }
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end;
    long long v;

    if (*s == '\0') {
        return -1;
    }
    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || *end != '\0') {
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

static int copy_text(char *dst, size_t cap, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= cap) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int read_order(const char *path, OrderRec *o)
{
    FILE *fp = fopen(path, "r");
    char line[MAX_LINE];
    char *f[9];

    if (fp == NULL) {
        fprintf(stderr, "入力ファイルを開けません: %s\n", path);
        return -1;
    }

    if (fgets(line, sizeof(line), fp) == NULL || fgets(line, sizeof(line), fp) == NULL) {
        fprintf(stderr, "注文入力が不足しています: %s\n", path);
        fclose(fp);
        return -1;
    }
    fclose(fp);

    if (split_csv(line, f, 9) != 0) {
        fprintf(stderr, "注文入力の項目数が不正です\n");
        return -1;
    }

    if (copy_text(o->order_id, sizeof(o->order_id), f[0]) != 0 ||
        copy_text(o->cif_no, sizeof(o->cif_no), f[1]) != 0 ||
        copy_text(o->instr_code, sizeof(o->instr_code), f[2]) != 0 ||
        strlen(f[3]) != 1 || strlen(f[4]) != 1 ||
        copy_text(o->tif_code, sizeof(o->tif_code), f[5]) != 0 ||
        parse_i64(f[6], &o->ord_qty) != 0 ||
        parse_i64(f[7], &o->price_amt) != 0 ||
        parse_int(f[8], &o->instr_tier) != 0) {
        fprintf(stderr, "注文入力の値が不正です\n");
        return -1;
    }

    o->side_kbn = f[3][0];
    o->ord_type = f[4][0];
    return 0;
}

static int read_books(const char *path, BookRec *book, size_t cap, size_t *cnt)
{
    FILE *fp = fopen(path, "r");
    char line[MAX_LINE];

    *cnt = 0;
    if (fp == NULL) {
        fprintf(stderr, "板入力ファイルを開けません: %s\n", path);
        return -1;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fprintf(stderr, "板入力が空です: %s\n", path);
        fclose(fp);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[7];
        BookRec *b;

        if (*cnt >= cap) {
            fprintf(stderr, "板入力件数が上限を超えました\n");
            fclose(fp);
            return -1;
        }
        if (split_csv(line, f, 7) != 0) {
            fprintf(stderr, "板入力の項目数が不正です\n");
            fclose(fp);
            return -1;
        }

        b = &book[(*cnt)++];
        if (copy_text(b->instr_code, sizeof(b->instr_code), f[0]) != 0 ||
            strlen(f[1]) != 1 ||
            parse_int(f[2], &b->level_cnt) != 0 ||
            parse_i64(f[3], &b->price_amt) != 0 ||
            parse_i64(f[4], &b->book_qty) != 0 ||
            parse_i64(f[5], &b->order_cnt) != 0 ||
            copy_text(b->entry_ts, sizeof(b->entry_ts), f[6]) != 0) {
            fprintf(stderr, "板入力の値が不正です\n");
            fclose(fp);
            return -1;
        }
        b->side_kbn = f[1][0];
    }

    fclose(fp);
    return 0;
}

static int read_markets(const char *path, MarketRec *mkt, size_t cap, size_t *cnt)
{
    FILE *fp = fopen(path, "r");
    char line[MAX_LINE];

    *cnt = 0;
    if (fp == NULL) {
        fprintf(stderr, "市場入力ファイルを開けません: %s\n", path);
        return -1;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fprintf(stderr, "市場入力が空です: %s\n", path);
        fclose(fp);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];
        MarketRec *m;

        if (*cnt >= cap) {
            fprintf(stderr, "市場入力件数が上限を超えました\n");
            fclose(fp);
            return -1;
        }
        if (split_csv(line, f, 6) != 0) {
            fprintf(stderr, "市場入力の項目数が不正です\n");
            fclose(fp);
            return -1;
        }

        m = &mkt[(*cnt)++];
        if (copy_text(m->instr_code, sizeof(m->instr_code), f[0]) != 0 ||
            parse_i64(f[1], &m->bid_amt) != 0 ||
            parse_i64(f[2], &m->ask_amt) != 0 ||
            parse_i64(f[3], &m->last_amt) != 0 ||
            parse_i64(f[4], &m->vol_qty) != 0 ||
            copy_text(m->tick_ts, sizeof(m->tick_ts), f[5]) != 0) {
            fprintf(stderr, "市場入力の値が不正です\n");
            fclose(fp);
            return -1;
        }
    }

    fclose(fp);
    return 0;
}

static int tier_spec(int tier, int *rate_bp, int64_t *tick)
{
    if (tier == 1) {
        *rate_bp = 1000;
        *tick = 100;
        return 0;
    }
    if (tier == 2) {
        *rate_bp = 2000;
        *tick = 500;
        return 0;
    }
    if (tier == 3) {
        *rate_bp = 4000;
        *tick = 1000;
        return 0;
    }
    return -1;
}

static const MarketRec *find_market(const MarketRec *mkt, size_t cnt, const char *instr)
{
    for (size_t i = 0; i < cnt; ++i) {
        if (strcmp(mkt[i].instr_code, instr) == 0) {
            return &mkt[i];
        }
    }
    return NULL;
}

static int price_crosses(const OrderRec *o, int64_t contra_price)
{
    if (o->ord_type == 'M') {
        return 1;
    }
    if (o->side_kbn == 'B') {
        return o->price_amt >= contra_price;
    }
    return o->price_amt <= contra_price;
}

static int64_t executable_qty(const OrderRec *o, const BookRec *book, size_t cnt)
{
    char contra_side = (o->side_kbn == 'B') ? 'S' : 'B';
    int64_t remain = o->ord_qty;
    int64_t total = 0;

    for (size_t i = 0; i < cnt && remain > 0; ++i) {
        int64_t take;

        if (strcmp(book[i].instr_code, o->instr_code) != 0 ||
            book[i].side_kbn != contra_side ||
            book[i].book_qty <= 0 ||
            book[i].price_amt <= 0) {
            continue;
        }
        if (!price_crosses(o, book[i].price_amt)) {
            continue;
        }

        take = book[i].book_qty < remain ? book[i].book_qty : remain;
        if (INT64_MAX - total < take) {
            return -1;
        }
        total += take;
        remain -= take;
    }

    return total;
}

static int validate_order(const OrderRec *o, const MarketRec *m, int rate_bp, int64_t tick)
{
    int64_t ref_price;
    int64_t notional;
    int64_t margin;

    if ((o->side_kbn != 'B' && o->side_kbn != 'S') ||
        (o->ord_type != 'L' && o->ord_type != 'M') ||
        (strcmp(o->tif_code, "DAY") != 0 && strcmp(o->tif_code, "IOC") != 0 && strcmp(o->tif_code, "FOK") != 0) ||
        o->ord_qty <= 0) {
        return RC_HARD_ERROR;
    }

    ref_price = o->price_amt;
    if (o->ord_type == 'M') {
        ref_price = (o->side_kbn == 'B') ? m->ask_amt : m->bid_amt;
    }

    if (ref_price <= 0 || o->ord_qty > INT64_MAX / ref_price) {
        return RC_REJECT_NOTIONAL;
    }
    notional = o->ord_qty * ref_price;
    if (notional > MIHFT_MAX_NOTIONAL) {
        return RC_REJECT_NOTIONAL;
    }

    if (o->ord_type == 'L' && (o->price_amt <= 0 || o->price_amt % tick != 0)) {
        return RC_REJECT_TICK;
    }

    if (notional > INT64_MAX / rate_bp) {
        return RC_REJECT_MARGIN;
    }
    margin = (notional * rate_bp + 9999) / 10000;
    if (margin > 200000000LL) {
        return RC_REJECT_MARGIN;
    }

    return RC_ACCEPT;
}

int main(void)
{
    OrderRec order;
    BookRec book[MAX_BOOK];
    MarketRec market[MAX_MARKET];
    const MarketRec *m;
    size_t book_cnt;
    size_t market_cnt;
    int rate_bp;
    int64_t tick;
    int rc;
    int64_t qty;

    if (read_order("SCORDF.csv", &order) != 0 ||
        read_books("SCBOOK.csv", book, MAX_BOOK, &book_cnt) != 0 ||
        read_markets("SCMKTD.csv", market, MAX_MARKET, &market_cnt) != 0) {
        return RC_HARD_ERROR;
    }

    if (tier_spec(order.instr_tier, &rate_bp, &tick) != 0) {
        fprintf(stderr, "商品階層が不正です\n");
        return RC_HARD_ERROR;
    }

    m = find_market(market, market_cnt, order.instr_code);
    if (m == NULL || m->bid_amt <= 0 || m->ask_amt <= 0 || m->bid_amt > m->ask_amt) {
        fprintf(stderr, "市場スナップショットが不正です\n");
        return RC_HARD_ERROR;
    }

    rc = validate_order(&order, m, rate_bp, tick);
    if (rc != RC_ACCEPT) {
        printf("%s,%d,%d\n", order.order_id, rc, 0);
        return rc;
    }

    qty = executable_qty(&order, book, book_cnt);
    if (qty < 0) {
        fprintf(stderr, "約定数量の集計で桁あふれを検出しました\n");
        return RC_HARD_ERROR;
    }

    if (qty < LOT_QTY) {
        qty = 0;
    }
    if (strcmp(order.tif_code, "FOK") == 0 && qty < order.ord_qty) {
        qty = 0;
    }

    printf("%s,%d,%" PRId64 "\n", order.order_id, RC_ACCEPT, qty);
    return RC_ACCEPT;
}
