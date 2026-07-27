/*
 * 変更履歴
 * 版数  年月日      担当    概要
 * 1.00  20220906  篠原 健 (E-203)  初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_PATH_SCEXEC "SCEXEC.csv"
#define MIHFT_PATH_SCORDF "SCORDF.csv"
#define MIHFT_PATH_SCDROP "SCDROP"
#define MIHFT_LINE_MAX 1024
#define MIHFT_ID_MAX 64
#define MIHFT_CODE_MAX 32
#define MIHFT_TS_MAX 32
#define MIHFT_ERR_IO 20
#define MIHFT_ERR_PARSE 24
#define MIHFT_ERR_MEMORY 28

typedef struct {
    char order_id[MIHFT_ID_MAX];
    char cif_no[MIHFT_ID_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    uint64_t ord_qty;
    uint64_t price_amt;
    int instr_tier;
} MihftOrderRow;

typedef struct {
    char exec_id[MIHFT_ID_MAX];
    char order_id[MIHFT_ID_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    uint64_t fill_qty;
    uint64_t fill_amt;
    char exec_ts[MIHFT_TS_MAX];
} MihftExecRow;

static void trim_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t dst_size, const char *src)
{
    size_t n;

    if (dst_size == 0U) {
        return 0;
    }

    while (*src == ' ' || *src == '\t') {
        src++;
    }

    n = strlen(src);
    while (n > 0U && (src[n - 1U] == ' ' || src[n - 1U] == '\t')) {
        n--;
    }

    if (n == 0U || n >= dst_size) {
        return 0;
    }

    memcpy(dst, src, n);
    dst[n] = '\0';
    return 1;
}

static int parse_u64(const char *s, uint64_t *out)
{
    char *endp;
    unsigned long long v;

    if (s == NULL || *s == '\0') {
        return 0;
    }

    errno = 0;
    v = strtoull(s, &endp, 10);
    if (errno != 0 || endp == s) {
        return 0;
    }

    while (*endp == ' ' || *endp == '\t') {
        endp++;
    }

    if (*endp != '\0') {
        return 0;
    }

    *out = (uint64_t)v;
    return 1;
}

static int parse_i32(const char *s, int *out)
{
    char *endp;
    long v;

    if (s == NULL || *s == '\0') {
        return 0;
    }

    errno = 0;
    v = strtol(s, &endp, 10);
    if (errno != 0 || endp == s || v < INT_MIN || v > INT_MAX) {
        return 0;
    }

    while (*endp == ' ' || *endp == '\t') {
        endp++;
    }

    if (*endp != '\0') {
        return 0;
    }

    *out = (int)v;
    return 1;
}

static int split_csv(char *line, char **field, size_t need)
{
    size_t n = 0U;
    char *p = line;

    while (n < need) {
        field[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    return n == need && strchr(field[need - 1U], ',') == NULL;
}

static int valid_side(char c)
{
    return c == 'B' || c == 'S';
}

static int valid_order_attr(const MihftOrderRow *o)
{
    if (!valid_side(o->side_kbn)) {
        return 0;
    }
    if (o->ord_type != 'L' && o->ord_type != 'M') {
        return 0;
    }
    if (strcmp(o->tif_code, "DAY") != 0 &&
        strcmp(o->tif_code, "IOC") != 0 &&
        strcmp(o->tif_code, "FOK") != 0) {
        return 0;
    }
    return o->instr_tier >= 1 && o->instr_tier <= 3;
}

static int parse_order(char *line, MihftOrderRow *o)
{
    char *f[9];

    trim_eol(line);
    if (!split_csv(line, f, 9U)) {
        return 0;
    }

    if (!copy_field(o->order_id, sizeof(o->order_id), f[0]) ||
        !copy_field(o->cif_no, sizeof(o->cif_no), f[1]) ||
        !copy_field(o->instr_code, sizeof(o->instr_code), f[2]) ||
        !copy_field(o->tif_code, sizeof(o->tif_code), f[5])) {
        return 0;
    }

    o->side_kbn = f[3][0];
    o->ord_type = f[4][0];

    if (f[3][1] != '\0' || f[4][1] != '\0') {
        return 0;
    }

    if (!parse_u64(f[6], &o->ord_qty) ||
        !parse_u64(f[7], &o->price_amt) ||
        !parse_i32(f[8], &o->instr_tier)) {
        return 0;
    }

    return valid_order_attr(o);
}

static int parse_exec(char *line, MihftExecRow *e)
{
    char *f[7];

    trim_eol(line);
    if (!split_csv(line, f, 7U)) {
        return 0;
    }

    if (!copy_field(e->exec_id, sizeof(e->exec_id), f[0]) ||
        !copy_field(e->order_id, sizeof(e->order_id), f[1]) ||
        !copy_field(e->instr_code, sizeof(e->instr_code), f[2]) ||
        !copy_field(e->exec_ts, sizeof(e->exec_ts), f[6])) {
        return 0;
    }

    e->side_kbn = f[3][0];
    if (f[3][1] != '\0' || !valid_side(e->side_kbn)) {
        return 0;
    }

    return parse_u64(f[4], &e->fill_qty) && parse_u64(f[5], &e->fill_amt);
}

static int order_cmp(const void *a, const void *b)
{
    const MihftOrderRow *oa = (const MihftOrderRow *)a;
    const MihftOrderRow *ob = (const MihftOrderRow *)b;
    return strcmp(oa->order_id, ob->order_id);
}

static const MihftOrderRow *find_order(const MihftOrderRow *orders, size_t count, const char *order_id)
{
    MihftOrderRow key;

    memset(&key, 0, sizeof(key));
    if (!copy_field(key.order_id, sizeof(key.order_id), order_id)) {
        return NULL;
    }

    return (const MihftOrderRow *)bsearch(&key, orders, count, sizeof(orders[0]), order_cmp);
}

static int push_order(MihftOrderRow **orders, size_t *count, size_t *cap, const MihftOrderRow *row)
{
    MihftOrderRow *p;
    size_t next_cap;

    if (*count == *cap) {
        next_cap = (*cap == 0U) ? 1024U : (*cap * 2U);
        if (next_cap < *cap || next_cap > (SIZE_MAX / sizeof((*orders)[0]))) {
            return 0;
        }

        p = (MihftOrderRow *)realloc(*orders, next_cap * sizeof((*orders)[0]));
        if (p == NULL) {
            return 0;
        }

        *orders = p;
        *cap = next_cap;
    }

    (*orders)[(*count)++] = *row;
    return 1;
}

static int load_orders(MihftOrderRow **orders, size_t *count)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    size_t cap = 0U;
    unsigned long lineno = 0UL;

    fp = fopen(MIHFT_PATH_SCORDF, "r");
    if (fp == NULL) {
        fprintf(stderr, "SCORDFオープン失敗\n");
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        MihftOrderRow row;

        lineno++;
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0') {
            continue;
        }

        if (lineno == 1UL && strncmp(line, "ORDER-ID,", 9U) == 0) {
            continue;
        }

        if (!parse_order(line, &row)) {
            fprintf(stderr, "SCORDF解析失敗 行=%lu\n", lineno);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        if (!push_order(orders, count, &cap, &row)) {
            fprintf(stderr, "SCORDF領域不足\n");
            fclose(fp);
            return MIHFT_ERR_MEMORY;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCORDF読込失敗\n");
        fclose(fp);
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    qsort(*orders, *count, sizeof((*orders)[0]), order_cmp);
    return 0;
}

static int checked_notional(uint64_t qty, uint64_t price, uint64_t *out)
{
    if (qty != 0U && price > UINT64_MAX / qty) {
        return 0;
    }
    *out = qty * price;
    return 1;
}

static int validate_execution(const MihftExecRow *e, const MihftOrderRow *o)
{
    uint64_t order_notional;

    if (strcmp(e->instr_code, o->instr_code) != 0 || e->side_kbn != o->side_kbn) {
        return 12;
    }

    if (e->fill_qty == 0U || e->fill_qty > o->ord_qty) {
        return 8;
    }

    if (e->fill_amt > MIHFT_MAX_NOTIONAL) {
        return 8;
    }

    if (!checked_notional(o->ord_qty, o->price_amt, &order_notional)) {
        return 8;
    }

    if (order_notional > MIHFT_MAX_NOTIONAL) {
        return 8;
    }

    return 0;
}

static void make_send_ts(char *dst, size_t dst_size)
{
    time_t now = time(NULL);
    struct tm *tmv = localtime(&now);

    if (tmv == NULL) {
        snprintf(dst, dst_size, "00000000000000");
        return;
    }

    snprintf(dst, dst_size, "%04d%02d%02d%02d%02d%02d",
             tmv->tm_year + 1900,
             tmv->tm_mon + 1,
             tmv->tm_mday,
             tmv->tm_hour,
             tmv->tm_min,
             tmv->tm_sec);
}

static int write_drop_rows(const MihftOrderRow *orders, size_t order_count)
{
    FILE *in;
    FILE *out;
    char line[MIHFT_LINE_MAX];
    uint64_t seq = 1U;
    unsigned long lineno = 0UL;

    in = fopen(MIHFT_PATH_SCEXEC, "r");
    if (in == NULL) {
        fprintf(stderr, "SCEXECオープン失敗\n");
        return MIHFT_ERR_IO;
    }

    out = fopen(MIHFT_PATH_SCDROP, "w");
    if (out == NULL) {
        fprintf(stderr, "SCDROPオープン失敗\n");
        fclose(in);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        MihftExecRow exec_row;
        const MihftOrderRow *order_row;
        char send_ts[MIHFT_TS_MAX];
        int decision;

        lineno++;
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0') {
            continue;
        }

        if (lineno == 1UL && strncmp(line, "EXEC-ID,", 8U) == 0) {
            continue;
        }

        if (!parse_exec(line, &exec_row)) {
            fprintf(stderr, "SCEXEC解析失敗 行=%lu\n", lineno);
            fclose(out);
            fclose(in);
            return MIHFT_ERR_PARSE;
        }

        order_row = find_order(orders, order_count, exec_row.order_id);
        if (order_row == NULL) {
            fprintf(stderr, "注文紐付け失敗 行=%lu\n", lineno);
            fclose(out);
            fclose(in);
            return MIHFT_ERR_PARSE;
        }

        decision = validate_execution(&exec_row, order_row);
        if (decision != 0) {
            continue;
        }

        make_send_ts(send_ts, sizeof(send_ts));

        if (fprintf(out, "D%012llu,%s,%s,%s,%s,%c,%llu,%llu,%s\n",
                    (unsigned long long)seq,
                    exec_row.exec_id,
                    exec_row.order_id,
                    order_row->cif_no,
                    exec_row.instr_code,
                    exec_row.side_kbn,
                    (unsigned long long)exec_row.fill_qty,
                    (unsigned long long)exec_row.fill_amt,
                    send_ts) < 0) {
            fprintf(stderr, "SCDROP書込失敗\n");
            fclose(out);
            fclose(in);
            return MIHFT_ERR_IO;
        }

        seq++;
    }

    if (ferror(in)) {
        fprintf(stderr, "SCEXEC読込失敗\n");
        fclose(out);
        fclose(in);
        return MIHFT_ERR_IO;
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "SCDROPクローズ失敗\n");
        fclose(in);
        return MIHFT_ERR_IO;
    }

    fclose(in);
    return 0;
}

int main(void)
{
    MihftOrderRow *orders = NULL;
    size_t order_count = 0U;
    int rc;

    rc = load_orders(&orders, &order_count);
    if (rc != 0) {
        free(orders);
        return rc;
    }

    rc = write_drop_rows(orders, order_count);
    free(orders);

    if (rc != 0) {
        return rc;
    }

    return 0;
}
