/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20190416  大野 修 (E-225)    板取消反映の初版作成
 * 1.01  20190916  三宅 拓也 (E-241)    入出力検査と数量整合性検査を追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_RET_IOERR  20
#define MIHFT_RET_PARSE  24
#define MIHFT_RET_NODATA 28
#define MIHFT_RET_INCONS 32

#define MIHFT_ACCEPT 0
#define MIHFT_LINE_MAX 512
#define MIHFT_CODE_MAX 32
#define MIHFT_TS_MAX 32

typedef struct {
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int order_cnt;
    char entry_ts[MIHFT_TS_MAX];
} scbook_row_t;

typedef struct {
    char order_id[MIHFT_CODE_MAX];
    char cif_no[MIHFT_CODE_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char state_kbn;
    int64_t leaves_qty;
    int64_t cum_qty;
    int64_t avg_fill_amt;
    char last_upd_ts[MIHFT_TS_MAX];
} scords_row_t;

static void trim_eol(char *s)
{
    size_t n = strlen(s);

    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_token(char *dst, size_t dstsz, const char *src)
{
    size_t n;

    if (dstsz == 0U || src == NULL) {
        return -1;
    }

    n = strlen(src);
    if (n >= dstsz) {
        return -1;
    }

    memcpy(dst, src, n + 1U);
    return 0;
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

static int parse_int_checked(const char *s, int *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static char *next_field(char **cur)
{
    char *p;
    char *comma;

    if (cur == NULL || *cur == NULL) {
        return NULL;
    }

    p = *cur;
    comma = strchr(p, ',');
    if (comma != NULL) {
        *comma = '\0';
        *cur = comma + 1;
    } else {
        *cur = NULL;
    }

    return p;
}

static int parse_scbook_line(char *line, scbook_row_t *row)
{
    char *p = line;
    char *f[7];
    size_t i;

    for (i = 0U; i < 7U; i++) {
        f[i] = next_field(&p);
        if (f[i] == NULL) {
            return -1;
        }
    }
    if (p != NULL) {
        return -1;
    }

    if (copy_token(row->instr_code, sizeof(row->instr_code), f[0]) != 0) {
        return -1;
    }
    if (strlen(f[1]) != 1U || (f[1][0] != 'B' && f[1][0] != 'S')) {
        return -1;
    }
    row->side_kbn = f[1][0];

    if (parse_int_checked(f[2], &row->level_cnt) != 0 ||
        parse_i64(f[3], &row->price_amt) != 0 ||
        parse_i64(f[4], &row->book_qty) != 0 ||
        parse_int_checked(f[5], &row->order_cnt) != 0 ||
        copy_token(row->entry_ts, sizeof(row->entry_ts), f[6]) != 0) {
        return -1;
    }

    if (row->level_cnt < 0 || row->price_amt <= 0 ||
        row->book_qty < 0 || row->order_cnt < 0) {
        return -1;
    }

    return 0;
}

static int parse_scords_line(char *line, scords_row_t *row)
{
    char *p = line;
    char *f[8];
    size_t i;

    for (i = 0U; i < 8U; i++) {
        f[i] = next_field(&p);
        if (f[i] == NULL) {
            return -1;
        }
    }
    if (p != NULL) {
        return -1;
    }

    if (copy_token(row->order_id, sizeof(row->order_id), f[0]) != 0 ||
        copy_token(row->cif_no, sizeof(row->cif_no), f[1]) != 0 ||
        copy_token(row->instr_code, sizeof(row->instr_code), f[2]) != 0) {
        return -1;
    }
    if (strlen(f[3]) != 1U) {
        return -1;
    }
    row->state_kbn = f[3][0];

    if (parse_i64(f[4], &row->leaves_qty) != 0 ||
        parse_i64(f[5], &row->cum_qty) != 0 ||
        parse_i64(f[6], &row->avg_fill_amt) != 0 ||
        copy_token(row->last_upd_ts, sizeof(row->last_upd_ts), f[7]) != 0) {
        return -1;
    }

    if (row->leaves_qty < 0 || row->cum_qty < 0 || row->avg_fill_amt < 0) {
        return -1;
    }

    return 0;
}

static int read_one_scbook(const char *path, scbook_row_t *row)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "SCBOOK入力を開けません: %s\n", path);
        return MIHFT_RET_IOERR;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCBOOK入力が空です: %s\n", path);
        return MIHFT_RET_NODATA;
    }

    trim_eol(line);
    if (parse_scbook_line(line, row) != 0) {
        fclose(fp);
        fprintf(stderr, "SCBOOK入力の形式が不正です: %s\n", path);
        return MIHFT_RET_PARSE;
    }

    if (fgets(line, sizeof(line), fp) != NULL) {
        fclose(fp);
        fprintf(stderr, "SCBOOK入力が複数行です: %s\n", path);
        return MIHFT_RET_PARSE;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCBOOK入力の読込に失敗しました: %s\n", path);
        return MIHFT_RET_IOERR;
    }

    fclose(fp);
    return MIHFT_ACCEPT;
}

static int read_one_scords(const char *path, scords_row_t *row)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "SCORDS入力を開けません: %s\n", path);
        return MIHFT_RET_IOERR;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCORDS入力が空です: %s\n", path);
        return MIHFT_RET_NODATA;
    }

    trim_eol(line);
    if (parse_scords_line(line, row) != 0) {
        fclose(fp);
        fprintf(stderr, "SCORDS入力の形式が不正です: %s\n", path);
        return MIHFT_RET_PARSE;
    }

    if (fgets(line, sizeof(line), fp) != NULL) {
        fclose(fp);
        fprintf(stderr, "SCORDS入力が複数行です: %s\n", path);
        return MIHFT_RET_PARSE;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCORDS入力の読込に失敗しました: %s\n", path);
        return MIHFT_RET_IOERR;
    }

    fclose(fp);
    return MIHFT_ACCEPT;
}

static int write_scbook(const char *path, const scbook_row_t *row)
{
    FILE *fp = fopen(path, "w");

    if (fp == NULL) {
        fprintf(stderr, "SCBOOK出力を開けません: %s\n", path);
        return MIHFT_RET_IOERR;
    }

    if (fprintf(fp, "%s,%c,%d,%" PRId64 ",%" PRId64 ",%d,%s\n",
                row->instr_code, row->side_kbn, row->level_cnt,
                row->price_amt, row->book_qty, row->order_cnt,
                row->entry_ts) < 0) {
        fclose(fp);
        fprintf(stderr, "SCBOOK出力の書込に失敗しました: %s\n", path);
        return MIHFT_RET_IOERR;
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCBOOK出力の確定に失敗しました: %s\n", path);
        return MIHFT_RET_IOERR;
    }

    return MIHFT_ACCEPT;
}

static int write_scords(const char *path, const scords_row_t *row)
{
    FILE *fp = fopen(path, "w");

    if (fp == NULL) {
        fprintf(stderr, "SCORDS出力を開けません: %s\n", path);
        return MIHFT_RET_IOERR;
    }

    if (fprintf(fp, "%s,%s,%s,%c,%" PRId64 ",%" PRId64 ",%" PRId64 ",%s\n",
                row->order_id, row->cif_no, row->instr_code, row->state_kbn,
                row->leaves_qty, row->cum_qty, row->avg_fill_amt,
                row->last_upd_ts) < 0) {
        fclose(fp);
        fprintf(stderr, "SCORDS出力の書込に失敗しました: %s\n", path);
        return MIHFT_RET_IOERR;
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCORDS出力の確定に失敗しました: %s\n", path);
        return MIHFT_RET_IOERR;
    }

    return MIHFT_ACCEPT;
}

static void now_yyyymmddhhmmss(char *dst, size_t dstsz)
{
    time_t t = time(NULL);
    struct tm tmv;

    if (dstsz == 0U) {
        return;
    }

#if defined(_POSIX_THREAD_SAFE_FUNCTIONS) || defined(__APPLE__)
    if (localtime_r(&t, &tmv) != NULL) {
        (void)strftime(dst, dstsz, "%Y%m%d%H%M%S", &tmv);
        return;
    }
#endif

    {
        struct tm *tmp = localtime(&t);
        if (tmp != NULL) {
            tmv = *tmp;
            (void)strftime(dst, dstsz, "%Y%m%d%H%M%S", &tmv);
            return;
        }
    }

    (void)snprintf(dst, dstsz, "00000000000000");
}

static const char *env_or_default(const char *name, const char *fallback)
{
    const char *v = getenv(name);

    if (v == NULL || *v == '\0') {
        return fallback;
    }
    return v;
}

static int reflect_cancel(scbook_row_t *book, scords_row_t *order)
{
    if (strcmp(book->instr_code, order->instr_code) != 0) {
        fprintf(stderr, "銘柄コードが一致しません\n");
        return MIHFT_RET_INCONS;
    }

    if (order->state_kbn == 'C') {
        fprintf(stderr, "注文は取消済です\n");
        return MIHFT_RET_INCONS;
    }

    if (order->leaves_qty <= 0) {
        fprintf(stderr, "取消対象数量がありません\n");
        return MIHFT_RET_INCONS;
    }

    if (book->book_qty < order->leaves_qty) {
        fprintf(stderr, "板数量が取消数量を下回っています\n");
        return MIHFT_RET_INCONS;
    }

    if (book->order_cnt <= 0) {
        fprintf(stderr, "注文件数が不足しています\n");
        return MIHFT_RET_INCONS;
    }

    book->book_qty -= order->leaves_qty;
    book->order_cnt -= 1;

    if (book->book_qty == 0) {
        book->level_cnt = 0;
    } else if (book->level_cnt <= 0) {
        fprintf(stderr, "板数量と水準件数が矛盾しています\n");
        return MIHFT_RET_INCONS;
    }

    order->state_kbn = 'C';
    order->leaves_qty = 0;
    now_yyyymmddhhmmss(order->last_upd_ts, sizeof(order->last_upd_ts));

    return MIHFT_ACCEPT;
}

int main(void)
{
    const char *book_in = env_or_default("MIHFT_SCBOOK_IN", "SCBOOK.csv");
    const char *ords_in = env_or_default("MIHFT_SCORDS_IN", "SCORDS.csv");
    const char *book_out = env_or_default("MIHFT_SCBOOK_OUT", "SCBOOK.out.csv");
    const char *ords_out = env_or_default("MIHFT_SCORDS_OUT", "SCORDS.out.csv");
    scbook_row_t book;
    scords_row_t order;
    int rc;

    memset(&book, 0, sizeof(book));
    memset(&order, 0, sizeof(order));

    rc = read_one_scbook(book_in, &book);
    if (rc != MIHFT_ACCEPT) {
        return rc;
    }

    rc = read_one_scords(ords_in, &order);
    if (rc != MIHFT_ACCEPT) {
        return rc;
    }

    rc = reflect_cancel(&book, &order);
    if (rc != MIHFT_ACCEPT) {
        return rc;
    }

    rc = write_scbook(book_out, &book);
    if (rc != MIHFT_ACCEPT) {
        return rc;
    }

    rc = write_scords(ords_out, &order);
    if (rc != MIHFT_ACCEPT) {
        return rc;
    }

    return MIHFT_ACCEPT;
}
