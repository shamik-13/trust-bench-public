/*
 * 変更履歴
 * 版数  年月日    担当    概要
 * 1.00  20190416  三宅 拓也 (E-241)  初版作成。板流動性確認の事前判定処理を実装。
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_RET_IOERR 64
#define MIHFT_RET_PARSEERR 65
#define MIHFT_MAX_LINE 1024
#define MIHFT_MAX_BOOK 8192
#define MIHFT_MAX_FIELD 16
#define MIHFT_TS_LEN 15

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    uint64_t price_amt;
    uint64_t book_qty;
    uint32_t order_cnt;
    char entry_ts[32];
} book_rec_t;

typedef struct {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    uint64_t ord_qty;
    uint64_t price_amt;
    int instr_tier;
} order_rec_t;

typedef struct {
    uint64_t fill_qty;
    uint64_t remain_qty;
    uint64_t notional_amt;
    uint64_t limit_used_amt;
    int used_levels;
} consume_result_t;

static void trim_field(char *s)
{
    char *p = s;
    char *e;

    while (*p != '\0' && isspace((unsigned char)*p)) {
        p++;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1U);
    }

    e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) {
        --e;
    }
    *e = '\0';
}

static int split_csv(char *line, char *field[], size_t cap)
{
    size_t n = 0U;
    char *p = line;

    while (n < cap) {
        field[n++] = p;
        while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
            p++;
        }
        if (*p == ',') {
            *p++ = '\0';
            continue;
        }
        *p = '\0';
        break;
    }

    for (size_t i = 0U; i < n; i++) {
        trim_field(field[i]);
    }
    return (int)n;
}

static int parse_u64(const char *s, uint64_t *out)
{
    char *end = NULL;
    unsigned long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }
    errno = 0;
    v = strtoull(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }
    *out = (uint64_t)v;
    return 0;
}

static int parse_i32(const char *s, int *out)
{
    char *end = NULL;
    long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }
    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0' || v < INT32_MIN || v > INT32_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int parse_u32(const char *s, uint32_t *out)
{
    uint64_t v;

    if (parse_u64(s, &v) != 0 || v > UINT32_MAX) {
        return -1;
    }
    *out = (uint32_t)v;
    return 0;
}

static int copy_text(char *dst, size_t dstsz, const char *src)
{
    size_t n;

    if (dstsz == 0U || src == NULL || *src == '\0') {
        return -1;
    }
    n = strlen(src);
    if (n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1U);
    return 0;
}

static int parse_side(const char *s, char *side)
{
    if (s == NULL || s[1] != '\0' || (s[0] != 'B' && s[0] != 'S')) {
        return -1;
    }
    *side = s[0];
    return 0;
}

static int parse_ord_type(const char *s, char *ord_type)
{
    if (s == NULL || s[1] != '\0' || (s[0] != 'L' && s[0] != 'M')) {
        return -1;
    }
    *ord_type = s[0];
    return 0;
}

static int parse_book(char *line, book_rec_t *rec)
{
    char *f[7];

    if (split_csv(line, f, 7U) != 7) {
        return -1;
    }
    if (copy_text(rec->instr_code, sizeof(rec->instr_code), f[0]) != 0) {
        return -1;
    }
    if (parse_side(f[1], &rec->side_kbn) != 0) {
        return -1;
    }
    if (parse_i32(f[2], &rec->level_cnt) != 0 || rec->level_cnt < 1) {
        return -1;
    }
    if (parse_u64(f[3], &rec->price_amt) != 0 || rec->price_amt == 0U) {
        return -1;
    }
    if (parse_u64(f[4], &rec->book_qty) != 0) {
        return -1;
    }
    if (parse_u32(f[5], &rec->order_cnt) != 0) {
        return -1;
    }
    if (copy_text(rec->entry_ts, sizeof(rec->entry_ts), f[6]) != 0) {
        return -1;
    }
    return 0;
}

static int parse_order(char *line, order_rec_t *rec)
{
    char *f[9];

    if (split_csv(line, f, 9U) != 9) {
        return -1;
    }
    if (copy_text(rec->order_id, sizeof(rec->order_id), f[0]) != 0) {
        return -1;
    }
    if (copy_text(rec->cif_no, sizeof(rec->cif_no), f[1]) != 0) {
        return -1;
    }
    if (copy_text(rec->instr_code, sizeof(rec->instr_code), f[2]) != 0) {
        return -1;
    }
    if (parse_side(f[3], &rec->side_kbn) != 0) {
        return -1;
    }
    if (parse_ord_type(f[4], &rec->ord_type) != 0) {
        return -1;
    }
    if (copy_text(rec->tif_code, sizeof(rec->tif_code), f[5]) != 0) {
        return -1;
    }
    if (strcmp(rec->tif_code, "DAY") != 0 && strcmp(rec->tif_code, "IOC") != 0 && strcmp(rec->tif_code, "FOK") != 0) {
        return -1;
    }
    if (parse_u64(f[6], &rec->ord_qty) != 0 || rec->ord_qty == 0U) {
        return -1;
    }
    if (parse_u64(f[7], &rec->price_amt) != 0) {
        return -1;
    }
    if (parse_i32(f[8], &rec->instr_tier) != 0 || rec->instr_tier < 1 || rec->instr_tier > 3) {
        return -1;
    }
    return 0;
}

static int tier_rate_bp(int tier)
{
    if (tier == 1) {
        return 1000;
    }
    if (tier == 2) {
        return 2000;
    }
    return 4000;
}

static uint64_t tier_tick(int tier)
{
    if (tier == 1) {
        return 100U;
    }
    if (tier == 2) {
        return 500U;
    }
    return 1000U;
}

static char opposite_side(char side)
{
    return side == 'B' ? 'S' : 'B';
}

static int mul_u64(uint64_t a, uint64_t b, uint64_t *out)
{
    if (a != 0U && b > UINT64_MAX / a) {
        return -1;
    }
    *out = a * b;
    return 0;
}

static int add_u64(uint64_t a, uint64_t b, uint64_t *out)
{
    if (b > UINT64_MAX - a) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int price_crosses(const order_rec_t *ord, const book_rec_t *book)
{
    if (ord->ord_type == 'M') {
        return 1;
    }
    if (ord->side_kbn == 'B') {
        return book->price_amt <= ord->price_amt;
    }
    return book->price_amt >= ord->price_amt;
}

static int book_before(const book_rec_t *a, const book_rec_t *b)
{
    if (a->level_cnt != b->level_cnt) {
        return a->level_cnt < b->level_cnt;
    }
    if (a->side_kbn == 'S' && a->price_amt != b->price_amt) {
        return a->price_amt < b->price_amt;
    }
    if (a->side_kbn == 'B' && a->price_amt != b->price_amt) {
        return a->price_amt > b->price_amt;
    }
    return strcmp(a->entry_ts, b->entry_ts) < 0;
}

static void sort_book(book_rec_t *book, size_t n)
{
    for (size_t i = 1U; i < n; i++) {
        book_rec_t v = book[i];
        size_t j = i;

        while (j > 0U && book_before(&v, &book[j - 1U])) {
            book[j] = book[j - 1U];
            --j;
        }
        book[j] = v;
    }
}

static int load_book(const char *path, book_rec_t *book, size_t cap, size_t *out_n)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0U;

    if (fp == NULL) {
        fprintf(stderr, "E1001 SCBOOK 入力オープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        book_rec_t rec;

        if (line[0] == '\n' || line[0] == '\r' || strncmp(line, "INSTR-CODE", 10U) == 0) {
            continue;
        }
        if (n >= cap || parse_book(line, &rec) != 0) {
            fclose(fp);
            fprintf(stderr, "E1002 SCBOOK 入力解析失敗\n");
            return -1;
        }
        book[n++] = rec;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "E1003 SCBOOK 入力読込失敗\n");
        return -1;
    }
    fclose(fp);
    sort_book(book, n);
    *out_n = n;
    return 0;
}

static int make_ts(char out[MIHFT_TS_LEN])
{
    time_t now = time(NULL);
    struct tm *tmv;

    if (now == (time_t)-1) {
        return -1;
    }
    tmv = localtime(&now);
    if (tmv == NULL) {
        return -1;
    }
    if (strftime(out, MIHFT_TS_LEN, "%Y%m%d%H%M%S", tmv) != 14U) {
        return -1;
    }
    return 0;
}

static int calc_consume(const order_rec_t *ord, const book_rec_t *book, size_t n, consume_result_t *res)
{
    uint64_t remain = ord->ord_qty;
    uint64_t notional = 0U;
    int levels = 0;

    for (size_t i = 0U; i < n && remain > 0U; i++) {
        uint64_t take;
        uint64_t part;

        if (strcmp(book[i].instr_code, ord->instr_code) != 0 || book[i].side_kbn != opposite_side(ord->side_kbn)) {
            continue;
        }
        if (!price_crosses(ord, &book[i])) {
            continue;
        }

        take = book[i].book_qty < remain ? book[i].book_qty : remain;
        if (take == 0U) {
            continue;
        }
        if (mul_u64(take, book[i].price_amt, &part) != 0 || add_u64(notional, part, &notional) != 0) {
            return -1;
        }
        remain -= take;
        levels++;
    }

    res->fill_qty = ord->ord_qty - remain;
    res->remain_qty = remain;
    res->notional_amt = notional;
    res->used_levels = levels;
    if (mul_u64(res->notional_amt, (uint64_t)tier_rate_bp(ord->instr_tier), &res->limit_used_amt) != 0) {
        return -1;
    }
    res->limit_used_amt /= 10000U;
    return 0;
}

static int decide_order(const order_rec_t *ord, const consume_result_t *res, char *reason, size_t reason_sz)
{
    uint64_t tick = tier_tick(ord->instr_tier);

    if (ord->ord_type == 'L' && (ord->price_amt == 0U || (ord->price_amt % tick) != 0U)) {
        snprintf(reason, reason_sz, "TICK");
        return 12;
    }
    if (res->notional_amt > (uint64_t)MIHFT_MAX_NOTIONAL) {
        snprintf(reason, reason_sz, "NOTIONAL");
        return 8;
    }
    if (res->fill_qty == 0U) {
        snprintf(reason, reason_sz, "NOBOOK");
        return 8;
    }
    if (strcmp(ord->tif_code, "FOK") == 0 && res->remain_qty != 0U) {
        snprintf(reason, reason_sz, "FOKSHORT");
        return 8;
    }
    if (ord->ord_type == 'M' && strcmp(ord->tif_code, "IOC") != 0 && res->remain_qty != 0U) {
        snprintf(reason, reason_sz, "THINBOOK");
        return 8;
    }
    snprintf(reason, reason_sz, "OK");
    return 0;
}

static int write_decision(FILE *fp, uint64_t seq, const order_rec_t *ord, int cd,
                          const char *reason, const consume_result_t *res, const char *ts)
{
    if (fprintf(fp, "D%012" PRIu64 ",%s,%s,%s,%d,%s,%" PRIu64 ",%" PRIu64 ",%s\n",
                seq, ord->order_id, ord->cif_no, ord->instr_code, cd, reason,
                res->notional_amt, res->limit_used_amt, ts) < 0) {
        return -1;
    }
    return 0;
}

static int write_reject(FILE *fp, uint64_t seq, const order_rec_t *ord, int cd,
                        const char *detail, const char *ts)
{
    if (fprintf(fp, "R%012" PRIu64 ",%s,%s,%s,%d,%s,%s\n",
                seq, ord->order_id, ord->cif_no, ord->instr_code, cd, detail, ts) < 0) {
        return -1;
    }
    return 0;
}

int main(void)
{
    book_rec_t book[MIHFT_MAX_BOOK];
    size_t book_n = 0U;
    FILE *ordfp;
    FILE *decfp;
    FILE *rjfp;
    char line[MIHFT_MAX_LINE];
    uint64_t seq = 1U;
    int final_cd = 0;

    if (load_book("SCBOOK.csv", book, MIHFT_MAX_BOOK, &book_n) != 0) {
        return MIHFT_RET_PARSEERR;
    }

    ordfp = fopen("SCORDF.csv", "r");
    if (ordfp == NULL) {
        fprintf(stderr, "E2001 SCORDF 入力オープン失敗\n");
        return MIHFT_RET_IOERR;
    }
    decfp = fopen("HFDEC.dat", "w");
    if (decfp == NULL) {
        fclose(ordfp);
        fprintf(stderr, "E3001 HFDEC 出力オープン失敗\n");
        return MIHFT_RET_IOERR;
    }
    rjfp = fopen("HFRJCT.dat", "w");
    if (rjfp == NULL) {
        fclose(decfp);
        fclose(ordfp);
        fprintf(stderr, "E3002 HFRJCT 出力オープン失敗\n");
        return MIHFT_RET_IOERR;
    }

    while (fgets(line, sizeof(line), ordfp) != NULL) {
        order_rec_t ord;
        consume_result_t res;
        char reason[16];
        char ts[MIHFT_TS_LEN];
        int cd;

        if (line[0] == '\n' || line[0] == '\r' || strncmp(line, "ORDER-ID", 8U) == 0) {
            continue;
        }
        if (parse_order(line, &ord) != 0 || make_ts(ts) != 0) {
            fclose(rjfp);
            fclose(decfp);
            fclose(ordfp);
            fprintf(stderr, "E2002 SCORDF 入力解析失敗\n");
            return MIHFT_RET_PARSEERR;
        }
        if (calc_consume(&ord, book, book_n, &res) != 0) {
            fclose(rjfp);
            fclose(decfp);
            fclose(ordfp);
            fprintf(stderr, "E4001 金額計算桁あふれ\n");
            return MIHFT_RET_PARSEERR;
        }

        cd = decide_order(&ord, &res, reason, sizeof(reason));
        if (write_decision(decfp, seq, &ord, cd, reason, &res, ts) != 0) {
            fclose(rjfp);
            fclose(decfp);
            fclose(ordfp);
            fprintf(stderr, "E3003 HFDEC 出力失敗\n");
            return MIHFT_RET_IOERR;
        }
        if (cd != 0 && write_reject(rjfp, seq, &ord, cd, reason, ts) != 0) {
            fclose(rjfp);
            fclose(decfp);
            fclose(ordfp);
            fprintf(stderr, "E3004 HFRJCT 出力失敗\n");
            return MIHFT_RET_IOERR;
        }
        if (cd > final_cd) {
            final_cd = cd;
        }
        seq++;
    }

    if (ferror(ordfp)) {
        fclose(rjfp);
        fclose(decfp);
        fclose(ordfp);
        fprintf(stderr, "E2003 SCORDF 入力読込失敗\n");
        return MIHFT_RET_IOERR;
    }
    if (fclose(rjfp) != 0 || fclose(decfp) != 0 || fclose(ordfp) != 0) {
        fprintf(stderr, "E3005 ファイルクローズ失敗\n");
        return MIHFT_RET_IOERR;
    }

    return final_cd;
}
