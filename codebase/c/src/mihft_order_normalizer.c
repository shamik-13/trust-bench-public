/***************************************
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210715  小林 直樹 (E-252)   初版作成
 ***************************************/
#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_LOT_SIZE 100
#define MIHFT_LINE_MAX 1024
#define MIHFT_FIELD_MAX 9

enum mihft_hot_order {
    MIHFT_HOT_BUY_LIMIT_DAY = 101,
    MIHFT_HOT_BUY_LIMIT_IOC = 102,
    MIHFT_HOT_BUY_LIMIT_FOK = 103,
    MIHFT_HOT_BUY_MARKET_DAY = 111,
    MIHFT_HOT_BUY_MARKET_IOC = 112,
    MIHFT_HOT_BUY_MARKET_FOK = 113,
    MIHFT_HOT_SELL_LIMIT_DAY = 201,
    MIHFT_HOT_SELL_LIMIT_IOC = 202,
    MIHFT_HOT_SELL_LIMIT_FOK = 203,
    MIHFT_HOT_SELL_MARKET_DAY = 211,
    MIHFT_HOT_SELL_MARKET_IOC = 212,
    MIHFT_HOT_SELL_MARKET_FOK = 213
};

struct staged_order {
    char order_id[33];
    char cif_no[33];
    char instr_code[33];
    char side_kbn[4];
    char ord_type[4];
    char tif_code[8];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
};

static char *trim_field(char *s)
{
    char *e;

    while (*s != '\0' && isspace((unsigned char)*s)) {
        ++s;
    }

    e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) {
        --e;
    }
    *e = '\0';

    return s;
}

static int copy_token(char *dst, size_t dst_size, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dst_size) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *endp;
    long long v;

    if (*s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || *endp != '\0') {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int parse_int(const char *s, int *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v < INT32_MIN || v > INT32_MAX) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int split_csv(char *line, char *fields[], size_t max_fields)
{
    size_t n = 0;
    char *p = line;

    while (n < max_fields) {
        fields[n++] = trim_field(p);
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p = '\0';
        ++p;
    }

    return (p == NULL && n == max_fields) ? 0 : -1;
}

static int parse_order(char *line, struct staged_order *o)
{
    char *fields[MIHFT_FIELD_MAX];

    line[strcspn(line, "\r\n")] = '\0';
    if (split_csv(line, fields, MIHFT_FIELD_MAX) != 0) {
        return -1;
    }

    if (copy_token(o->order_id, sizeof(o->order_id), fields[0]) != 0 ||
        copy_token(o->cif_no, sizeof(o->cif_no), fields[1]) != 0 ||
        copy_token(o->instr_code, sizeof(o->instr_code), fields[2]) != 0 ||
        copy_token(o->side_kbn, sizeof(o->side_kbn), fields[3]) != 0 ||
        copy_token(o->ord_type, sizeof(o->ord_type), fields[4]) != 0 ||
        copy_token(o->tif_code, sizeof(o->tif_code), fields[5]) != 0 ||
        parse_i64(fields[6], &o->ord_qty) != 0 ||
        parse_i64(fields[7], &o->price_amt) != 0 ||
        parse_int(fields[8], &o->instr_tier) != 0) {
        return -1;
    }

    return 0;
}

static int tier_tick(int tier, int64_t *tick)
{
    switch (tier) {
    case 1:
        *tick = 100;
        return 0;
    case 2:
        *tick = 500;
        return 0;
    case 3:
        *tick = 1000;
        return 0;
    default:
        return -1;
    }
}

static int to_hot_enum(const char *side, const char *type, const char *tif, int *hot)
{
    int side_base;
    int type_base;
    int tif_add;

    if (strcmp(side, "B") == 0) {
        side_base = 100;
    } else if (strcmp(side, "S") == 0) {
        side_base = 200;
    } else {
        return -1;
    }

    if (strcmp(type, "L") == 0) {
        type_base = 0;
    } else if (strcmp(type, "M") == 0) {
        type_base = 10;
    } else {
        return -1;
    }

    if (strcmp(tif, "DAY") == 0) {
        tif_add = 1;
    } else if (strcmp(tif, "IOC") == 0) {
        tif_add = 2;
    } else if (strcmp(tif, "FOK") == 0) {
        tif_add = 3;
    } else {
        return -1;
    }

    *hot = side_base + type_base + tif_add;
    return 0;
}

static int checked_abs_i64(int64_t v, uint64_t *out)
{
    if (v == INT64_MIN) {
        return -1;
    }

    *out = (uint64_t)(v < 0 ? -v : v);
    return 0;
}

static int checked_notional(int64_t qty, int64_t price, uint64_t *out)
{
    uint64_t q;
    uint64_t p;

    if (checked_abs_i64(qty, &q) != 0 || checked_abs_i64(price, &p) != 0) {
        return -1;
    }
    if (q != 0 && p > UINT64_MAX / q) {
        return -1;
    }

    *out = q * p;
    return 0;
}

static int normalize_order(const struct staged_order *o, int64_t *lot_qty, int64_t *tick_price, int *hot)
{
    int64_t tick;
    uint64_t notional;

    if (tier_tick(o->instr_tier, &tick) != 0) {
        return 12;
    }

    if (o->ord_qty <= 0 || o->ord_qty % MIHFT_LOT_SIZE != 0) {
        return 12;
    }

    if (strcmp(o->ord_type, "M") == 0) {
        if (o->price_amt != 0) {
            return 12;
        }
        *tick_price = 0;
    } else {
        if (o->price_amt <= 0 || o->price_amt % tick != 0) {
            return 12;
        }
        *tick_price = o->price_amt / tick;
    }

    if (checked_notional(o->ord_qty, o->price_amt, &notional) != 0) {
        return 8;
    }
    if (notional > (uint64_t)MIHFT_MAX_NOTIONAL) {
        return 8;
    }

    if (to_hot_enum(o->side_kbn, o->ord_type, o->tif_code, hot) != 0) {
        return 12;
    }

    *lot_qty = o->ord_qty / MIHFT_LOT_SIZE;
    return 0;
}

int main(void)
{
    char line[MIHFT_LINE_MAX];
    unsigned long line_no = 0;
    int last_decision = 0;

    while (fgets(line, sizeof(line), stdin) != NULL) {
        struct staged_order order;
        int decision;
        int hot;
        int64_t lot_qty;
        int64_t tick_price;

        ++line_no;

        if (strchr(line, '\n') == NULL && !feof(stdin)) {
            fprintf(stderr, "入力行が長すぎます: %lu\n", line_no);
            return 2;
        }

        if (line_no == 1 && strncmp(line, "ORDER-ID,", 9) == 0) {
            continue;
        }

        if (parse_order(line, &order) != 0) {
            fprintf(stderr, "入力形式が不正です: %lu\n", line_no);
            return 2;
        }

        decision = normalize_order(&order, &lot_qty, &tick_price, &hot);
        last_decision = decision;

        if (decision == 0) {
            printf("%s,%s,%s,%" PRId64 ",%" PRId64 ",%d,%d\n",
                   order.order_id,
                   order.cif_no,
                   order.instr_code,
                   lot_qty,
                   tick_price,
                   hot,
                   decision);
        } else {
            printf("%s,%d\n", order.order_id, decision);
        }
    }

    if (ferror(stdin)) {
        fprintf(stderr, "入力読込に失敗しました\n");
        return 2;
    }

    return last_decision;
}
