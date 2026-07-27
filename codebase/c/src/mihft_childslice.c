/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20231114  三宅 拓也 (E-241)  子注文スライサ初版作成
 * 1.01  20240414  福田 亮太 (E-211)  板数量上限と金額スケール確認を追加
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
#include <time.h>

#define MIHFT_ACCEPT 0
#define MIHFT_REJECT_MARGIN 4
#define MIHFT_REJECT_NOTIONAL 8
#define MIHFT_REJECT_TICK 12

#define MAX_LINE_LEN 512
#define MAX_BOOK_ROWS 256
#define MAX_ROUTES 64
#define MAX_FIELD_CNT 16
#define MONEY_SCALE 100LL

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
} LocalOrder;

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int order_cnt;
    char entry_ts[32];
} LocalBook;

typedef struct {
    char route_id[32];
    char order_id[32];
    char venue_code[8];
    int64_t child_qty;
    int64_t limit_amt;
    char route_ts[32];
} LocalRoute;

static void trim_field(char *s)
{
    char *p = s;
    size_t len;

    while (*p != '\0' && isspace((unsigned char)*p)) {
        ++p;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1U);
    }

    len = strlen(s);
    while (len > 0U && isspace((unsigned char)s[len - 1U])) {
        s[--len] = '\0';
    }
}

static int split_csv(char *line, char **fields, int max_fields)
{
    int cnt = 0;
    char *p = line;

    while (cnt < max_fields) {
        fields[cnt++] = p;
        while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
            ++p;
        }
        if (*p == ',') {
            *p++ = '\0';
            continue;
        }
        *p = '\0';
        break;
    }

    for (int i = 0; i < cnt; ++i) {
        trim_field(fields[i]);
    }
    return cnt;
}

static int parse_i64(const char *s, int64_t *out)
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

static int parse_int(const char *s, int *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int parse_order_line(char *line, LocalOrder *ord)
{
    char *f[MAX_FIELD_CNT];
    int n = split_csv(line, f, MAX_FIELD_CNT);

    if (n != 9) {
        return -1;
    }

    if (strlen(f[0]) >= sizeof(ord->order_id) ||
        strlen(f[1]) >= sizeof(ord->cif_no) ||
        strlen(f[2]) >= sizeof(ord->instr_code) ||
        strlen(f[5]) >= sizeof(ord->tif_code)) {
        return -1;
    }

    strcpy(ord->order_id, f[0]);
    strcpy(ord->cif_no, f[1]);
    strcpy(ord->instr_code, f[2]);

    if ((strcmp(f[3], "B") != 0 && strcmp(f[3], "S") != 0) ||
        (strcmp(f[4], "L") != 0 && strcmp(f[4], "M") != 0)) {
        return -1;
    }
    ord->side_kbn = f[3][0];
    ord->ord_type = f[4][0];
    strcpy(ord->tif_code, f[5]);

    if (strcmp(ord->tif_code, "DAY") != 0 &&
        strcmp(ord->tif_code, "IOC") != 0 &&
        strcmp(ord->tif_code, "FOK") != 0) {
        return -1;
    }

    if (parse_i64(f[6], &ord->ord_qty) != 0 ||
        parse_i64(f[7], &ord->price_amt) != 0 ||
        parse_int(f[8], &ord->instr_tier) != 0) {
        return -1;
    }

    return 0;
}

static int parse_book_line(char *line, LocalBook *book)
{
    char *f[MAX_FIELD_CNT];
    int n = split_csv(line, f, MAX_FIELD_CNT);

    if (n != 7) {
        return -1;
    }

    if (strlen(f[0]) >= sizeof(book->instr_code) ||
        strlen(f[6]) >= sizeof(book->entry_ts)) {
        return -1;
    }

    strcpy(book->instr_code, f[0]);
    if (strcmp(f[1], "B") != 0 && strcmp(f[1], "S") != 0) {
        return -1;
    }
    book->side_kbn = f[1][0];

    if (parse_int(f[2], &book->level_cnt) != 0 ||
        parse_i64(f[3], &book->price_amt) != 0 ||
        parse_i64(f[4], &book->book_qty) != 0 ||
        parse_int(f[5], &book->order_cnt) != 0) {
        return -1;
    }
    strcpy(book->entry_ts, f[6]);

    return 0;
}

static int read_order(LocalOrder *ord)
{
    FILE *fp = fopen("SCORDF.csv", "r");
    char line[MAX_LINE_LEN];

    if (fp == NULL) {
        fprintf(stderr, "SCORDFを開けません\n");
        return -1;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCORDFが空です\n");
        return -1;
    }

    if (parse_order_line(line, ord) != 0) {
        fclose(fp);
        fprintf(stderr, "SCORDFの形式が不正です\n");
        return -1;
    }

    fclose(fp);
    return 0;
}

static int read_books(LocalBook *books, size_t *book_cnt)
{
    FILE *fp = fopen("SCBOOK.csv", "r");
    char line[MAX_LINE_LEN];
    size_t cnt = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCBOOKを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        if (cnt >= MAX_BOOK_ROWS) {
            fclose(fp);
            fprintf(stderr, "SCBOOKの件数が上限を超過しました\n");
            return -1;
        }
        if (parse_book_line(line, &books[cnt]) != 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOKの形式が不正です\n");
            return -1;
        }
        ++cnt;
    }

    fclose(fp);
    *book_cnt = cnt;
    return 0;
}

static int tier_tick(int tier, int64_t *tick)
{
    if (tier == 1) {
        *tick = 100;
    } else if (tier == 2) {
        *tick = 500;
    } else if (tier == 3) {
        *tick = 1000;
    } else {
        return -1;
    }
    return 0;
}

static int tier_margin_bp(int tier, int *rate_bp)
{
    if (tier == 1) {
        *rate_bp = 1000;
    } else if (tier == 2) {
        *rate_bp = 2000;
    } else if (tier == 3) {
        *rate_bp = 4000;
    } else {
        return -1;
    }
    return 0;
}

static int checked_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a < 0 || b < 0) {
        return -1;
    }
    if (a != 0 && b > INT64_MAX / a) {
        return -1;
    }
    *out = a * b;
    return 0;
}

static int validate_order(const LocalOrder *ord, int64_t ref_price)
{
    int64_t tick;
    int64_t px = ord->ord_type == 'M' ? ref_price : ord->price_amt;
    int64_t notional;
    int rate_bp;
    int64_t margin;

    if (ord->ord_qty <= 0 || px <= 0 || px % MONEY_SCALE != 0) {
        return MIHFT_REJECT_TICK;
    }

    if (tier_tick(ord->instr_tier, &tick) != 0 || px % tick != 0) {
        return MIHFT_REJECT_TICK;
    }

    if (checked_mul_i64(ord->ord_qty, px, &notional) != 0) {
        return MIHFT_REJECT_NOTIONAL;
    }
    if (notional > (int64_t)MIHFT_MAX_NOTIONAL * MONEY_SCALE) {
        return MIHFT_REJECT_NOTIONAL;
    }

    if (tier_margin_bp(ord->instr_tier, &rate_bp) != 0) {
        return MIHFT_REJECT_MARGIN;
    }
    margin = (notional * rate_bp) / 10000;
    if (margin <= 0 || margin > notional) {
        return MIHFT_REJECT_MARGIN;
    }

    return MIHFT_ACCEPT;
}

static int is_executable_book(const LocalOrder *ord, const LocalBook *book)
{
    if (strcmp(ord->instr_code, book->instr_code) != 0) {
        return 0;
    }
    if (ord->side_kbn == 'B' && book->side_kbn != 'S') {
        return 0;
    }
    if (ord->side_kbn == 'S' && book->side_kbn != 'B') {
        return 0;
    }
    if (book->level_cnt <= 0 || book->book_qty <= 0 || book->price_amt <= 0) {
        return 0;
    }
    if (book->price_amt % MONEY_SCALE != 0) {
        return 0;
    }
    if (ord->ord_type == 'L') {
        if (ord->side_kbn == 'B' && book->price_amt > ord->price_amt) {
            return 0;
        }
        if (ord->side_kbn == 'S' && book->price_amt < ord->price_amt) {
            return 0;
        }
    }
    return 1;
}

static int better_book(const LocalOrder *ord, const LocalBook *a, const LocalBook *b)
{
    if (a->level_cnt != b->level_cnt) {
        return a->level_cnt < b->level_cnt;
    }
    if (ord->side_kbn == 'B') {
        if (a->price_amt != b->price_amt) {
            return a->price_amt < b->price_amt;
        }
    } else {
        if (a->price_amt != b->price_amt) {
            return a->price_amt > b->price_amt;
        }
    }
    return a->book_qty > b->book_qty;
}

static void sort_books(const LocalOrder *ord, LocalBook *books, size_t cnt)
{
    for (size_t i = 1; i < cnt; ++i) {
        LocalBook key = books[i];
        size_t j = i;
        while (j > 0 && better_book(ord, &key, &books[j - 1])) {
            books[j] = books[j - 1];
            --j;
        }
        books[j] = key;
    }
}

static void make_ts(char *buf, size_t len)
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_WIN32)
    localtime_s(&tmv, &now);
#else
    localtime_r(&now, &tmv);
#endif
    strftime(buf, len, "%Y%m%d%H%M%S", &tmv);
}

static int build_routes(const LocalOrder *ord, LocalBook *books, size_t book_cnt,
                        LocalRoute *routes, size_t *route_cnt, int64_t *ref_price)
{
    size_t eligible = 0;
    int64_t remain = ord->ord_qty;
    char ts[32];

    for (size_t i = 0; i < book_cnt; ++i) {
        if (is_executable_book(ord, &books[i])) {
            books[eligible++] = books[i];
        }
    }
    if (eligible == 0) {
        return MIHFT_REJECT_NOTIONAL;
    }

    sort_books(ord, books, eligible);
    *ref_price = books[0].price_amt;
    make_ts(ts, sizeof(ts));

    for (size_t i = 0; i < eligible && remain > 0; ++i) {
        int64_t cap = books[i].book_qty;
        int64_t visible_cap = cap / 2;
        int64_t child;

        if (*route_cnt >= MAX_ROUTES) {
            fprintf(stderr, "SCROUTの件数が上限を超過しました\n");
            return -1;
        }

        if (visible_cap <= 0) {
            visible_cap = 1;
        }
        child = remain < visible_cap ? remain : visible_cap;

        snprintf(routes[*route_cnt].route_id, sizeof(routes[*route_cnt].route_id),
                 "RT%06zu", *route_cnt + 1U);
        strcpy(routes[*route_cnt].order_id, ord->order_id);

        if (i % 3U == 0U) {
            strcpy(routes[*route_cnt].venue_code, "TSE");
        } else if (i % 3U == 1U) {
            strcpy(routes[*route_cnt].venue_code, "PTS");
        } else {
            strcpy(routes[*route_cnt].venue_code, "SOR");
        }

        routes[*route_cnt].child_qty = child;
        routes[*route_cnt].limit_amt = books[i].price_amt;
        strcpy(routes[*route_cnt].route_ts, ts);

        remain -= child;
        ++*route_cnt;
    }

    if (strcmp(ord->tif_code, "FOK") == 0 && remain > 0) {
        *route_cnt = 0;
        return MIHFT_REJECT_NOTIONAL;
    }

    return MIHFT_ACCEPT;
}

static int write_routes(const LocalRoute *routes, size_t route_cnt)
{
    FILE *fp = fopen("SCROUT.dat", "w");

    if (fp == NULL) {
        fprintf(stderr, "SCROUTを開けません\n");
        return -1;
    }

    for (size_t i = 0; i < route_cnt; ++i) {
        if (routes[i].child_qty <= 0 ||
            routes[i].limit_amt <= 0 ||
            routes[i].limit_amt % MONEY_SCALE != 0) {
            fclose(fp);
            fprintf(stderr, "SCROUTの金額または数量が不正です\n");
            return -1;
        }

        if (fprintf(fp, "%s,%s,%s,%" PRId64 ",%" PRId64 ",%s\n",
                    routes[i].route_id,
                    routes[i].order_id,
                    routes[i].venue_code,
                    routes[i].child_qty,
                    routes[i].limit_amt,
                    routes[i].route_ts) < 0) {
            fclose(fp);
            fprintf(stderr, "SCROUTの書込みに失敗しました\n");
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCROUTのクローズに失敗しました\n");
        return -1;
    }
    return 0;
}

int main(void)
{
    LocalOrder ord;
    LocalBook books[MAX_BOOK_ROWS];
    LocalRoute routes[MAX_ROUTES];
    size_t book_cnt = 0;
    size_t route_cnt = 0;
    int64_t ref_price = 0;
    int decision;

    if (read_order(&ord) != 0) {
        return 20;
    }
    if (read_books(books, &book_cnt) != 0) {
        return 21;
    }

    decision = build_routes(&ord, books, book_cnt, routes, &route_cnt, &ref_price);
    if (decision < 0) {
        return 22;
    }
    if (decision != MIHFT_ACCEPT) {
        return decision;
    }

    decision = validate_order(&ord, ref_price);
    if (decision != MIHFT_ACCEPT) {
        return decision;
    }

    if (write_routes(routes, route_cnt) != 0) {
        return 23;
    }

    return MIHFT_ACCEPT;
}
