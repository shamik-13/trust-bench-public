/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20191022  三宅 拓也 (E-241)     初版作成、板スイープ数量計算を実装
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_LINE_MAX 1024
#define MIHFT_BOOK_MAX 256
#define MIHFT_DEC_ACCEPT 0
#define MIHFT_DEC_REJECT_MARGIN 4
#define MIHFT_DEC_REJECT_NOTIONAL 8
#define MIHFT_DEC_REJECT_TICK 12

typedef struct {
    char order_id[64];
    char cif_no[64];
    char instr_code[64];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    uint64_t ord_qty;
    uint64_t price_amt;
    int instr_tier;
} mihft_order_rec_t;

typedef struct {
    char instr_code[64];
    char side_kbn;
    int level_cnt;
    uint64_t price_amt;
    uint64_t book_qty;
    uint64_t order_cnt;
    char entry_ts[64];
} mihft_book_rec_t;

typedef struct {
    uint64_t exec_qty;
    uint64_t residual_qty;
    uint64_t avg_price_amt;
    int decision_code;
} mihft_sweep_result_t;

static void mihft_chomp(char *s)
{
    size_t n;

    n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static char *mihft_trim(char *s)
{
    char *e;

    while (*s == ' ' || *s == '\t') {
        ++s;
    }
    e = s + strlen(s);
    while (e > s && (e[-1] == ' ' || e[-1] == '\t')) {
        *--e = '\0';
    }
    return s;
}

static int mihft_split_csv(char *line, char **cols, size_t need)
{
    size_t n;
    char *p;

    n = 0U;
    p = line;
    while (n < need) {
        cols[n++] = mihft_trim(p);
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return n == need && strchr(cols[need - 1U], ',') == NULL;
}

static int mihft_parse_u64(const char *s, uint64_t *out)
{
    char *end;
    unsigned long long v;

    if (s == NULL || *s == '\0' || *s == '-') {
        return 0;
    }
    errno = 0;
    v = strtoull(s, &end, 10);
    if (errno != 0 || end == s || *mihft_trim(end) != '\0') {
        return 0;
    }
    *out = (uint64_t)v;
    return 1;
}

static int mihft_parse_i32(const char *s, int *out)
{
    char *end;
    long v;

    if (s == NULL || *s == '\0') {
        return 0;
    }
    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || end == s || *mihft_trim(end) != '\0' || v < INT_MIN || v > INT_MAX) {
        return 0;
    }
    *out = (int)v;
    return 1;
}

static int mihft_copy_field(char *dst, size_t cap, const char *src)
{
    size_t n;

    n = strlen(src);
    if (n == 0U || n >= cap) {
        return 0;
    }
    memcpy(dst, src, n + 1U);
    return 1;
}

static int mihft_tier_tick(int tier, uint64_t *tick)
{
    if (tier == 1) {
        *tick = 100U;
        return 1;
    }
    if (tier == 2) {
        *tick = 500U;
        return 1;
    }
    if (tier == 3) {
        *tick = 1000U;
        return 1;
    }
    return 0;
}

static int mihft_read_order(const char *path, mihft_order_rec_t *ord)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    char work[MIHFT_LINE_MAX];
    char *c[9];

    fp = fopen(path, "r");
    if (fp == NULL) {
        return 0;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        memcpy(work, line, strlen(line) + 1U);
        if (!mihft_split_csv(work, c, 9U)) {
            fclose(fp);
            return -1;
        }
        if (strcmp(c[0], "ORDER-ID") == 0) {
            continue;
        }
        if (!mihft_copy_field(ord->order_id, sizeof(ord->order_id), c[0]) ||
            !mihft_copy_field(ord->cif_no, sizeof(ord->cif_no), c[1]) ||
            !mihft_copy_field(ord->instr_code, sizeof(ord->instr_code), c[2]) ||
            strlen(c[3]) != 1U || strlen(c[4]) != 1U ||
            !mihft_copy_field(ord->tif_code, sizeof(ord->tif_code), c[5]) ||
            !mihft_parse_u64(c[6], &ord->ord_qty) ||
            !mihft_parse_u64(c[7], &ord->price_amt) ||
            !mihft_parse_i32(c[8], &ord->instr_tier)) {
            fclose(fp);
            return -1;
        }
        ord->side_kbn = c[3][0];
        ord->ord_type = c[4][0];
        fclose(fp);
        return 1;
    }

    fclose(fp);
    return 0;
}

static int mihft_read_book(const char *path, mihft_book_rec_t *book, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    char work[MIHFT_LINE_MAX];
    char *c[7];
    size_t n;

    fp = fopen(path, "r");
    if (fp == NULL) {
        return 0;
    }

    n = 0U;
    while (fgets(line, sizeof(line), fp) != NULL) {
        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        memcpy(work, line, strlen(line) + 1U);
        if (!mihft_split_csv(work, c, 7U)) {
            fclose(fp);
            return -1;
        }
        if (strcmp(c[0], "INSTR-CODE") == 0) {
            continue;
        }
        if (n >= cap ||
            !mihft_copy_field(book[n].instr_code, sizeof(book[n].instr_code), c[0]) ||
            strlen(c[1]) != 1U ||
            !mihft_parse_i32(c[2], &book[n].level_cnt) ||
            !mihft_parse_u64(c[3], &book[n].price_amt) ||
            !mihft_parse_u64(c[4], &book[n].book_qty) ||
            !mihft_parse_u64(c[5], &book[n].order_cnt) ||
            !mihft_copy_field(book[n].entry_ts, sizeof(book[n].entry_ts), c[6])) {
            fclose(fp);
            return -1;
        }
        book[n].side_kbn = c[1][0];
        ++n;
    }

    fclose(fp);
    *count = n;
    return 1;
}

static const char *mihft_pick_file(const char *env_name, const char *a, const char *b)
{
    const char *e;
    FILE *fp;

    e = getenv(env_name);
    if (e != NULL && *e != '\0') {
        return e;
    }

    fp = fopen(a, "r");
    if (fp != NULL) {
        fclose(fp);
        return a;
    }
    return b;
}

static int mihft_level_cmp(const void *pa, const void *pb)
{
    const mihft_book_rec_t *a;
    const mihft_book_rec_t *b;

    a = (const mihft_book_rec_t *)pa;
    b = (const mihft_book_rec_t *)pb;
    if (a->level_cnt < b->level_cnt) {
        return -1;
    }
    if (a->level_cnt > b->level_cnt) {
        return 1;
    }
    return 0;
}

static int mihft_add_mul_u64(uint64_t *sum, uint64_t price, uint64_t qty)
{
    if (qty != 0U && price > UINT64_MAX / qty) {
        return 0;
    }
    if (*sum > UINT64_MAX - price * qty) {
        return 0;
    }
    *sum += price * qty;
    return 1;
}

static int mihft_validate_order(const mihft_order_rec_t *ord)
{
    uint64_t tick;
    uint64_t notional;

    if ((ord->side_kbn != 'B' && ord->side_kbn != 'S') ||
        (ord->ord_type != 'L' && ord->ord_type != 'M') ||
        (strcmp(ord->tif_code, "DAY") != 0 &&
         strcmp(ord->tif_code, "IOC") != 0 &&
         strcmp(ord->tif_code, "FOK") != 0) ||
        ord->ord_qty == 0U) {
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    if (ord->ord_type == 'L') {
        if (!mihft_tier_tick(ord->instr_tier, &tick) || ord->price_amt == 0U || ord->price_amt % tick != 0U) {
            return MIHFT_DEC_REJECT_TICK;
        }
        if (ord->ord_qty != 0U && ord->price_amt > UINT64_MAX / ord->ord_qty) {
            return MIHFT_DEC_REJECT_NOTIONAL;
        }
        notional = ord->price_amt * ord->ord_qty;
        if (notional > (uint64_t)MIHFT_MAX_NOTIONAL) {
            return MIHFT_DEC_REJECT_NOTIONAL;
        }
    }

    return MIHFT_DEC_ACCEPT;
}

static int mihft_is_marketable(const mihft_order_rec_t *ord, const mihft_book_rec_t *lv)
{
    if (ord->side_kbn == 'B') {
        if (lv->side_kbn != 'S') {
            return 0;
        }
        return ord->ord_type == 'M' || lv->price_amt <= ord->price_amt;
    }

    if (lv->side_kbn != 'B') {
        return 0;
    }
    return ord->ord_type == 'M' || lv->price_amt >= ord->price_amt;
}

static int mihft_sweep(const mihft_order_rec_t *ord,
                       mihft_book_rec_t *book,
                       size_t book_count,
                       mihft_sweep_result_t *res)
{
    size_t i;
    uint64_t remain;
    uint64_t take;
    uint64_t amount;

    qsort(book, book_count, sizeof(book[0]), mihft_level_cmp);

    remain = ord->ord_qty;
    amount = 0U;
    res->exec_qty = 0U;
    res->avg_price_amt = 0U;
    res->residual_qty = ord->ord_qty;
    res->decision_code = MIHFT_DEC_ACCEPT;

    for (i = 0U; i < book_count && remain > 0U; ++i) {
        if (strcmp(book[i].instr_code, ord->instr_code) != 0 || !mihft_is_marketable(ord, &book[i])) {
            continue;
        }

        take = book[i].book_qty < remain ? book[i].book_qty : remain;
        if (take == 0U) {
            continue;
        }
        if (!mihft_add_mul_u64(&amount, book[i].price_amt, take)) {
            return 0;
        }
        res->exec_qty += take;
        remain -= take;
    }

    if (strcmp(ord->tif_code, "FOK") == 0 && remain != 0U) {
        res->exec_qty = 0U;
        res->residual_qty = ord->ord_qty;
        res->avg_price_amt = 0U;
        return 1;
    }

    res->residual_qty = remain;
    if (res->exec_qty != 0U) {
        res->avg_price_amt = (amount + (res->exec_qty / 2U)) / res->exec_qty;
    }
    return 1;
}

int main(void)
{
    const char *ord_path;
    const char *book_path;
    mihft_order_rec_t ord;
    mihft_book_rec_t book[MIHFT_BOOK_MAX];
    mihft_sweep_result_t res;
    size_t book_count;
    int rc;
    int decision;

    memset(&ord, 0, sizeof(ord));
    memset(book, 0, sizeof(book));
    memset(&res, 0, sizeof(res));

    ord_path = mihft_pick_file("MIHFT_SCORDF", "SCORDF.csv", "scordf.csv");
    book_path = mihft_pick_file("MIHFT_SCBOOK", "SCBOOK.csv", "scbook.csv");

    rc = mihft_read_order(ord_path, &ord);
    if (rc <= 0) {
        fprintf(stderr, "注文ファイル読込失敗:%s\n", ord_path);
        return 20;
    }

    rc = mihft_read_book(book_path, book, MIHFT_BOOK_MAX, &book_count);
    if (rc <= 0) {
        fprintf(stderr, "板ファイル読込失敗:%s\n", book_path);
        return 21;
    }

    decision = mihft_validate_order(&ord);
    if (decision != MIHFT_DEC_ACCEPT) {
        printf("%s,%d,%" PRIu64 ",%" PRIu64 ",%" PRIu64 "\n",
               ord.order_id, decision, 0ULL, ord.ord_qty, 0ULL);
        return decision;
    }

    if (!mihft_sweep(&ord, book, book_count, &res)) {
        fprintf(stderr, "約定金額計算失敗:%s\n", ord.order_id);
        return 22;
    }

    printf("%s,%d,%" PRIu64 ",%" PRIu64 ",%" PRIu64 "\n",
           ord.order_id,
           res.decision_code,
           res.exec_qty,
           res.residual_qty,
           res.avg_price_amt);

    return res.decision_code;
}
