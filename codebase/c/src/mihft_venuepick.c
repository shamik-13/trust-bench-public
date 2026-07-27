/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210128  村上 健司 (E-301)     初版作成、ベニュー選択の事前判定処理を実装
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

#ifndef MIHFT_MAX_NOTIONAL
#define MIHFT_MAX_NOTIONAL 500000000LL
#endif

#define MIHFT_ACCEPT_CODE 0
#define MIHFT_REJECT_MARGIN_CODE 4
#define MIHFT_REJECT_NOTIONAL_CODE 8
#define MIHFT_REJECT_TICK_CODE 12
#define MIHFT_HARD_ERROR_CODE 2

#define MIHFT_MAX_VENUES 128
#define MIHFT_MAX_ORDERS 1024
#define MIHFT_MAX_BOOKS 4096
#define MIHFT_MAX_FIELDS 16
#define MIHFT_LINE_SIZE 1024
#define MIHFT_CODE_SIZE 32
#define MIHFT_TS_SIZE 32

typedef struct {
    char venue_code[MIHFT_CODE_SIZE];
    char board_code[MIHFT_CODE_SIZE];
    int64_t latency_us;
    int64_t fee_bps;
    int enabled;
    int64_t capacity_qty;
    size_t input_pos;
} MihftVenueRec;

typedef struct {
    char order_id[MIHFT_CODE_SIZE];
    char cif_no[MIHFT_CODE_SIZE];
    char instr_code[MIHFT_CODE_SIZE];
    char side_kbn;
    char ord_type;
    char tif_code[MIHFT_CODE_SIZE];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
    size_t input_pos;
} MihftOrderRec;

typedef struct {
    char instr_code[MIHFT_CODE_SIZE];
    char side_kbn;
    int64_t level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;
    char entry_ts[MIHFT_TS_SIZE];
    size_t input_pos;
} MihftBookRec;

typedef struct {
    size_t venue_idx;
    int compatible;
    int64_t executable_qty;
    int64_t fee_impact;
} MihftRankRec;

static void mihft_rstrip(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r' ||
                      s[n - 1U] == ' ' || s[n - 1U] == '\t')) {
        s[--n] = '\0';
    }
}

static char *mihft_lstrip(char *s)
{
    while (*s == ' ' || *s == '\t') {
        ++s;
    }
    return s;
}

static int mihft_split_csv(char *line, char **fields, size_t max_fields, size_t *count)
{
    char *p = line;
    size_t n = 0U;

    while (*p != '\0') {
        char *dst;
        int quoted = 0;

        if (n >= max_fields) {
            return -1;
        }

        fields[n++] = p;
        dst = p;

        if (*p == '"') {
            quoted = 1;
            ++p;
            fields[n - 1U] = dst;
            while (*p != '\0') {
                if (*p == '"' && p[1] == '"') {
                    *dst++ = '"';
                    p += 2;
                } else if (*p == '"') {
                    ++p;
                    break;
                } else {
                    *dst++ = *p++;
                }
            }
            if (*p != ',' && *p != '\0') {
                return -1;
            }
        } else {
            while (*p != ',' && *p != '\0') {
                *dst++ = *p++;
            }
        }

        *dst = '\0';
        mihft_rstrip(fields[n - 1U]);
        fields[n - 1U] = mihft_lstrip(fields[n - 1U]);

        if (*p == ',') {
            *p++ = '\0';
            if (*p == '\0') {
                if (n >= max_fields) {
                    return -1;
                }
                fields[n++] = p;
                break;
            }
        }
    }

    *count = n;
    return 0;
}

static int mihft_parse_i64(const char *s, int64_t *out)
{
    char *endp;
    long long v;

    if (s == NULL || *s == '\0') {
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

    memcpy(dst, src, n + 1U);
    return 0;
}

static int mihft_parse_side(const char *s, char *out)
{
    if (s == NULL || s[0] == '\0' || s[1] != '\0') {
        return -1;
    }
    if (s[0] != 'B' && s[0] != 'S') {
        return -1;
    }
    *out = s[0];
    return 0;
}

static int mihft_parse_ord_type(const char *s, char *out)
{
    if (s == NULL || s[0] == '\0' || s[1] != '\0') {
        return -1;
    }
    if (s[0] != 'L' && s[0] != 'M') {
        return -1;
    }
    *out = s[0];
    return 0;
}

static int mihft_enabled_kbn(const char *s, int *out)
{
    if (strcmp(s, "1") == 0 || strcmp(s, "Y") == 0 || strcmp(s, "E") == 0) {
        *out = 1;
        return 0;
    }
    if (strcmp(s, "0") == 0 || strcmp(s, "N") == 0 || strcmp(s, "D") == 0) {
        *out = 0;
        return 0;
    }
    return -1;
}

static int mihft_tier_rate_bp(int tier, int64_t *rate_bp)
{
    if (tier == 1) {
        *rate_bp = 1000;
        return 0;
    }
    if (tier == 2) {
        *rate_bp = 2000;
        return 0;
    }
    if (tier == 3) {
        *rate_bp = 4000;
        return 0;
    }
    return -1;
}

static int mihft_tier_tick(int tier, int64_t *tick)
{
    if (tier == 1) {
        *tick = 100;
        return 0;
    }
    if (tier == 2) {
        *tick = 500;
        return 0;
    }
    if (tier == 3) {
        *tick = 1000;
        return 0;
    }
    return -1;
}

static const char *mihft_tier_board(int tier)
{
    if (tier == 1) {
        return "T1";
    }
    if (tier == 2) {
        return "ST";
    }
    if (tier == 3) {
        return "ST";
    }
    return "";
}

static int mihft_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a != 0 && b != 0) {
        if ((a > 0 && b > 0 && a > INT64_MAX / b) ||
            (a > 0 && b < 0 && b < INT64_MIN / a) ||
            (a < 0 && b > 0 && a < INT64_MIN / b) ||
            (a < 0 && b < 0 && a < INT64_MAX / b)) {
            return -1;
        }
    }

    *out = a * b;
    return 0;
}

static int mihft_notional_x100(int64_t qty, int64_t price_amt, int64_t *out)
{
    int64_t v;

    if (qty <= 0 || price_amt < 0) {
        return -1;
    }
    if (mihft_mul_i64(qty, price_amt, &v) != 0) {
        return -1;
    }
    if (mihft_mul_i64(v, 100, out) != 0) {
        return -1;
    }
    return 0;
}

static int mihft_fee_impact(int64_t qty, int64_t price_amt, int64_t fee_bps, int64_t *out)
{
    int64_t notional;
    int64_t fee_src;

    if (mihft_notional_x100(qty, price_amt, &notional) != 0) {
        return -1;
    }
    if (mihft_mul_i64(notional, fee_bps, &fee_src) != 0) {
        return -1;
    }

    *out = fee_src / 10000;
    if (*out < 0) {
        *out = -*out;
    }
    return 0;
}

static int mihft_is_header_line(char **fields, size_t count, const char *first_name)
{
    return count > 0U && strcmp(fields[0], first_name) == 0;
}

static int mihft_load_venues(MihftVenueRec *venues, size_t *venue_count)
{
    FILE *fp = fopen("SCVENF", "r");
    char line[MIHFT_LINE_SIZE];
    size_t n = 0U;
    size_t row = 0U;

    if (fp == NULL) {
        fprintf(stderr, "SCVENFを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *fields[MIHFT_MAX_FIELDS];
        size_t count = 0U;
        int64_t enabled_tmp;

        ++row;
        mihft_rstrip(line);
        if (line[0] == '\0') {
            continue;
        }
        if (mihft_split_csv(line, fields, MIHFT_MAX_FIELDS, &count) != 0) {
            fprintf(stderr, "SCVENFのCSV形式が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }
        if (mihft_is_header_line(fields, count, "VENUE-CODE")) {
            continue;
        }
        if (count != 6U || n >= MIHFT_MAX_VENUES) {
            fprintf(stderr, "SCVENFの項目数または件数が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }

        if (mihft_copy_code(venues[n].venue_code, sizeof(venues[n].venue_code), fields[0]) != 0 ||
            mihft_copy_code(venues[n].board_code, sizeof(venues[n].board_code), fields[1]) != 0 ||
            mihft_parse_i64(fields[2], &venues[n].latency_us) != 0 ||
            mihft_parse_i64(fields[3], &venues[n].fee_bps) != 0 ||
            mihft_parse_i64(fields[5], &venues[n].capacity_qty) != 0) {
            fprintf(stderr, "SCVENFの値が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }

        if (mihft_enabled_kbn(fields[4], &venues[n].enabled) != 0) {
            if (mihft_parse_i64(fields[4], &enabled_tmp) != 0 ||
                (enabled_tmp != 0 && enabled_tmp != 1)) {
                fprintf(stderr, "SCVENFの有効区分が不正です 行=%zu\n", row);
                fclose(fp);
                return -1;
            }
            venues[n].enabled = (enabled_tmp == 1);
        }

        if (venues[n].latency_us < 0 || venues[n].capacity_qty < 0) {
            fprintf(stderr, "SCVENFの数量または遅延が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }

        venues[n].input_pos = n;
        ++n;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCVENFの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *venue_count = n;
    return 0;
}

static int mihft_load_orders(MihftOrderRec *orders, size_t *order_count)
{
    FILE *fp = fopen("SCORDF", "r");
    char line[MIHFT_LINE_SIZE];
    size_t n = 0U;
    size_t row = 0U;

    if (fp == NULL) {
        fprintf(stderr, "SCORDFを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *fields[MIHFT_MAX_FIELDS];
        size_t count = 0U;
        int64_t tier64;

        ++row;
        mihft_rstrip(line);
        if (line[0] == '\0') {
            continue;
        }
        if (mihft_split_csv(line, fields, MIHFT_MAX_FIELDS, &count) != 0) {
            fprintf(stderr, "SCORDFのCSV形式が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }
        if (mihft_is_header_line(fields, count, "ORDER-ID")) {
            continue;
        }
        if (count != 9U || n >= MIHFT_MAX_ORDERS) {
            fprintf(stderr, "SCORDFの項目数または件数が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }

        if (mihft_copy_code(orders[n].order_id, sizeof(orders[n].order_id), fields[0]) != 0 ||
            mihft_copy_code(orders[n].cif_no, sizeof(orders[n].cif_no), fields[1]) != 0 ||
            mihft_copy_code(orders[n].instr_code, sizeof(orders[n].instr_code), fields[2]) != 0 ||
            mihft_parse_side(fields[3], &orders[n].side_kbn) != 0 ||
            mihft_parse_ord_type(fields[4], &orders[n].ord_type) != 0 ||
            mihft_copy_code(orders[n].tif_code, sizeof(orders[n].tif_code), fields[5]) != 0 ||
            mihft_parse_i64(fields[6], &orders[n].ord_qty) != 0 ||
            mihft_parse_i64(fields[7], &orders[n].price_amt) != 0 ||
            mihft_parse_i64(fields[8], &tier64) != 0) {
            fprintf(stderr, "SCORDFの値が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }

        if (tier64 < 1 || tier64 > 3 || orders[n].ord_qty <= 0 ||
            orders[n].price_amt < 0 ||
            (strcmp(orders[n].tif_code, "DAY") != 0 &&
             strcmp(orders[n].tif_code, "IOC") != 0 &&
             strcmp(orders[n].tif_code, "FOK") != 0)) {
            fprintf(stderr, "SCORDFの区分または数量が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }

        orders[n].instr_tier = (int)tier64;
        orders[n].input_pos = n;
        ++n;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCORDFの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *order_count = n;
    return 0;
}

static int mihft_load_books(MihftBookRec *books, size_t *book_count)
{
    FILE *fp = fopen("SCBOOK", "r");
    char line[MIHFT_LINE_SIZE];
    size_t n = 0U;
    size_t row = 0U;

    if (fp == NULL) {
        fprintf(stderr, "SCBOOKを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *fields[MIHFT_MAX_FIELDS];
        size_t count = 0U;

        ++row;
        mihft_rstrip(line);
        if (line[0] == '\0') {
            continue;
        }
        if (mihft_split_csv(line, fields, MIHFT_MAX_FIELDS, &count) != 0) {
            fprintf(stderr, "SCBOOKのCSV形式が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }
        if (mihft_is_header_line(fields, count, "INSTR-CODE")) {
            continue;
        }
        if (count != 7U || n >= MIHFT_MAX_BOOKS) {
            fprintf(stderr, "SCBOOKの項目数または件数が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }

        if (mihft_copy_code(books[n].instr_code, sizeof(books[n].instr_code), fields[0]) != 0 ||
            mihft_parse_side(fields[1], &books[n].side_kbn) != 0 ||
            mihft_parse_i64(fields[2], &books[n].level_cnt) != 0 ||
            mihft_parse_i64(fields[3], &books[n].price_amt) != 0 ||
            mihft_parse_i64(fields[4], &books[n].book_qty) != 0 ||
            mihft_parse_i64(fields[5], &books[n].order_cnt) != 0 ||
            mihft_copy_code(books[n].entry_ts, sizeof(books[n].entry_ts), fields[6]) != 0) {
            fprintf(stderr, "SCBOOKの値が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }

        if (books[n].level_cnt <= 0 || books[n].price_amt < 0 ||
            books[n].book_qty < 0 || books[n].order_cnt < 0) {
            fprintf(stderr, "SCBOOKの数量または価格が不正です 行=%zu\n", row);
            fclose(fp);
            return -1;
        }

        books[n].input_pos = n;
        ++n;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCBOOKの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *book_count = n;
    return 0;
}

static char mihft_exec_side(char order_side)
{
    return order_side == 'B' ? 'S' : 'B';
}

static int mihft_price_executable(const MihftOrderRec *order, int64_t book_price)
{
    if (order->ord_type == 'M') {
        return 1;
    }
    if (order->side_kbn == 'B') {
        return book_price <= order->price_amt;
    }
    return book_price >= order->price_amt;
}

static int64_t mihft_executable_depth(const MihftOrderRec *order,
                                      const MihftBookRec *books,
                                      size_t book_count)
{
    char need_side = mihft_exec_side(order->side_kbn);
    int64_t depth = 0;
    size_t i;

    for (i = 0U; i < book_count; ++i) {
        if (strcmp(books[i].instr_code, order->instr_code) == 0 &&
            books[i].side_kbn == need_side &&
            mihft_price_executable(order, books[i].price_amt)) {
            if (INT64_MAX - depth < books[i].book_qty) {
                return INT64_MAX;
            }
            depth += books[i].book_qty;
        }
    }

    return depth;
}

static int mihft_board_compatible(const MihftOrderRec *order, const MihftVenueRec *venue)
{
    const char *need = mihft_tier_board(order->instr_tier);

    if (strcmp(venue->board_code, need) == 0) {
        return 1;
    }
    if (strcmp(venue->board_code, "ETF") == 0 && order->instr_tier == 1) {
        return 1;
    }
    return 0;
}

static int mihft_rank_better(const MihftRankRec *a,
                             const MihftRankRec *b,
                             const MihftVenueRec *venues)
{
    const MihftVenueRec *va = &venues[a->venue_idx];
    const MihftVenueRec *vb = &venues[b->venue_idx];

    if (a->compatible != b->compatible) {
        return a->compatible > b->compatible;
    }
    if (va->latency_us != vb->latency_us) {
        return va->latency_us < vb->latency_us;
    }
    if (va->capacity_qty != vb->capacity_qty) {
        return va->capacity_qty > vb->capacity_qty;
    }
    if (a->fee_impact != b->fee_impact) {
        return a->fee_impact < b->fee_impact;
    }
    if (a->executable_qty != b->executable_qty) {
        return a->executable_qty > b->executable_qty;
    }
    return va->input_pos < vb->input_pos;
}

static void mihft_sort_ranks(MihftRankRec *ranks, size_t rank_count, const MihftVenueRec *venues)
{
    size_t i;

    for (i = 1U; i < rank_count; ++i) {
        MihftRankRec v = ranks[i];
        size_t j = i;

        while (j > 0U && mihft_rank_better(&v, &ranks[j - 1U], venues)) {
            ranks[j] = ranks[j - 1U];
            --j;
        }
        ranks[j] = v;
    }
}

static int mihft_validate_order(const MihftOrderRec *order)
{
    int64_t tick;
    int64_t rate_bp;
    int64_t notional_x100;
    int64_t margin_x100;

    if (mihft_tier_tick(order->instr_tier, &tick) != 0 ||
        mihft_tier_rate_bp(order->instr_tier, &rate_bp) != 0) {
        return MIHFT_HARD_ERROR_CODE;
    }

    if (order->ord_type == 'L' && (order->price_amt == 0 || order->price_amt % tick != 0)) {
        return MIHFT_REJECT_TICK_CODE;
    }

    if (mihft_notional_x100(order->ord_qty, order->price_amt, &notional_x100) != 0) {
        return MIHFT_REJECT_NOTIONAL_CODE;
    }

    if (notional_x100 / 100 > MIHFT_MAX_NOTIONAL) {
        return MIHFT_REJECT_NOTIONAL_CODE;
    }

    if (mihft_mul_i64(notional_x100, rate_bp, &margin_x100) != 0) {
        return MIHFT_REJECT_MARGIN_CODE;
    }
    margin_x100 /= 10000;

    if (margin_x100 / 100 > MIHFT_MAX_NOTIONAL) {
        return MIHFT_REJECT_MARGIN_CODE;
    }

    return MIHFT_ACCEPT_CODE;
}

static void mihft_route_ts(char *buf, size_t bufsz)
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_POSIX_VERSION)
    localtime_r(&now, &tmv);
#else
    {
        struct tm *tmp = localtime(&now);
        if (tmp != NULL) {
            tmv = *tmp;
        } else {
            memset(&tmv, 0, sizeof(tmv));
        }
    }
#endif

    (void)snprintf(buf, bufsz, "%04d%02d%02d%02d%02d%02d",
                   tmv.tm_year + 1900, tmv.tm_mon + 1, tmv.tm_mday,
                   tmv.tm_hour, tmv.tm_min, tmv.tm_sec);
}

static int mihft_emit_route(FILE *out,
                            const MihftOrderRec *order,
                            const MihftVenueRec *venue,
                            int64_t child_qty,
                            int64_t limit_amt,
                            size_t seq)
{
    char route_ts[MIHFT_TS_SIZE];

    mihft_route_ts(route_ts, sizeof(route_ts));

    if (fprintf(out, "R%s%04zu,%s,%s,%" PRId64 ",%" PRId64 ",%s\n",
                order->order_id, seq, order->order_id, venue->venue_code,
                child_qty, limit_amt, route_ts) < 0) {
        return -1;
    }

    return 0;
}

static int mihft_prepare_ranks(const MihftOrderRec *order,
                               MihftVenueRec *venues,
                               size_t venue_count,
                               const MihftBookRec *books,
                               size_t book_count,
                               MihftRankRec *ranks,
                               size_t *rank_count)
{
    int64_t depth = mihft_executable_depth(order, books, book_count);
    size_t n = 0U;
    size_t i;

    for (i = 0U; i < venue_count; ++i) {
        int64_t fee_impact = 0;

        if (!venues[i].enabled || venues[i].capacity_qty <= 0) {
            continue;
        }
        if (mihft_fee_impact(order->ord_qty, order->price_amt,
                             venues[i].fee_bps, &fee_impact) != 0) {
            return -1;
        }

        ranks[n].venue_idx = i;
        ranks[n].compatible = mihft_board_compatible(order, &venues[i]);
        ranks[n].executable_qty = depth;
        ranks[n].fee_impact = fee_impact;
        ++n;
    }

    mihft_sort_ranks(ranks, n, venues);
    *rank_count = n;
    return 0;
}

static int64_t mihft_min3_i64(int64_t a, int64_t b, int64_t c)
{
    int64_t m = a < b ? a : b;
    return m < c ? m : c;
}

static int mihft_route_order(FILE *out,
                             const MihftOrderRec *order,
                             MihftVenueRec *venues,
                             size_t venue_count,
                             const MihftBookRec *books,
                             size_t book_count)
{
    MihftRankRec ranks[MIHFT_MAX_VENUES];
    size_t rank_count = 0U;
    int64_t remaining = order->ord_qty;
    int64_t depth = mihft_executable_depth(order, books, book_count);
    size_t seq = 1U;
    size_t i;

    if (mihft_prepare_ranks(order, venues, venue_count, books, book_count,
                            ranks, &rank_count) != 0) {
        fprintf(stderr, "順位計算で桁あふれを検知しました 注文=%s\n", order->order_id);
        return MIHFT_HARD_ERROR_CODE;
    }

    if (rank_count == 0U || depth <= 0) {
        return MIHFT_REJECT_NOTIONAL_CODE;
    }

    if (strcmp(order->tif_code, "FOK") == 0 && depth < order->ord_qty) {
        return MIHFT_REJECT_NOTIONAL_CODE;
    }

    for (i = 0U; i < rank_count && remaining > 0; ++i) {
        MihftVenueRec *venue = &venues[ranks[i].venue_idx];
        int64_t child_qty;

        if (!ranks[i].compatible || !venue->enabled || venue->capacity_qty <= 0 || depth <= 0) {
            continue;
        }

        child_qty = mihft_min3_i64(remaining, venue->capacity_qty, depth);
        if (child_qty <= 0) {
            continue;
        }

        if (mihft_emit_route(out, order, venue, child_qty, order->price_amt, seq) != 0) {
            fprintf(stderr, "SCROUTの書込に失敗しました 注文=%s\n", order->order_id);
            return MIHFT_HARD_ERROR_CODE;
        }

        venue->capacity_qty -= child_qty;
        depth -= child_qty;
        remaining -= child_qty;
        ++seq;

        if (strcmp(order->tif_code, "IOC") == 0) {
            break;
        }
    }

    if (remaining > 0 && strcmp(order->tif_code, "DAY") == 0) {
        for (i = 0U; i < rank_count && remaining > 0; ++i) {
            MihftVenueRec *venue = &venues[ranks[i].venue_idx];
            int64_t child_qty;

            if (ranks[i].compatible || !venue->enabled || venue->capacity_qty <= 0 || depth <= 0) {
                continue;
            }

            child_qty = mihft_min3_i64(remaining, venue->capacity_qty, depth);
            if (child_qty <= 0) {
                continue;
            }

            if (mihft_emit_route(out, order, venue, child_qty, order->price_amt, seq) != 0) {
                fprintf(stderr, "SCROUTの書込に失敗しました 注文=%s\n", order->order_id);
                return MIHFT_HARD_ERROR_CODE;
            }

            venue->capacity_qty -= child_qty;
            depth -= child_qty;
            remaining -= child_qty;
            ++seq;
        }
    }

    if (remaining == order->ord_qty) {
        return MIHFT_REJECT_NOTIONAL_CODE;
    }
    if (remaining > 0 && strcmp(order->tif_code, "FOK") == 0) {
        return MIHFT_REJECT_NOTIONAL_CODE;
    }

    return MIHFT_ACCEPT_CODE;
}

int main(void)
{
    MihftVenueRec venues[MIHFT_MAX_VENUES];
    MihftOrderRec orders[MIHFT_MAX_ORDERS];
    MihftBookRec books[MIHFT_MAX_BOOKS];
    size_t venue_count = 0U;
    size_t order_count = 0U;
    size_t book_count = 0U;
    int final_code = MIHFT_ACCEPT_CODE;
    FILE *out;
    size_t i;

    if (mihft_load_venues(venues, &venue_count) != 0 ||
        mihft_load_orders(orders, &order_count) != 0 ||
        mihft_load_books(books, &book_count) != 0) {
        return MIHFT_HARD_ERROR_CODE;
    }

    out = fopen("SCROUT", "w");
    if (out == NULL) {
        fprintf(stderr, "SCROUTを開けません\n");
        return MIHFT_HARD_ERROR_CODE;
    }

    for (i = 0U; i < order_count; ++i) {
        int code = mihft_validate_order(&orders[i]);

        if (code == MIHFT_ACCEPT_CODE) {
            code = mihft_route_order(out, &orders[i], venues, venue_count, books, book_count);
        }

        if (code == MIHFT_HARD_ERROR_CODE) {
            fclose(out);
            return MIHFT_HARD_ERROR_CODE;
        }
        if (final_code == MIHFT_ACCEPT_CODE && code != MIHFT_ACCEPT_CODE) {
            final_code = code;
        }
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "SCROUTの終了処理に失敗しました\n");
        return MIHFT_HARD_ERROR_CODE;
    }

    return final_code;
}
