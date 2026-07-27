/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220222  小林 直樹 (E-252)  注文状態更新の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_ERR_IO 20
#define MIHFT_ERR_PARSE 21
#define MIHFT_ERR_OVERFLOW 22

#define SCEXEC_PATH "SCEXEC.csv"
#define SCORDS_PATH "SCORDS.csv"
#define SCORDS_OUT_PATH "SCORDS.out.csv"

#define LINE_MAX_LEN 1024
#define FIELD_MAX_LEN 64
#define MAX_EXECS 4096
#define MAX_ORDS 4096

typedef struct {
    char exec_id[FIELD_MAX_LEN];
    char order_id[FIELD_MAX_LEN];
    char instr_code[FIELD_MAX_LEN];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[FIELD_MAX_LEN];
} sc_exec_rec_t;

typedef struct {
    char order_id[FIELD_MAX_LEN];
    char cif_no[FIELD_MAX_LEN];
    char instr_code[FIELD_MAX_LEN];
    char state_kbn[FIELD_MAX_LEN];
    int64_t leaves_qty;
    int64_t cum_qty;
    int64_t avg_fill_amt;
    char last_upd_ts[FIELD_MAX_LEN];
} sc_order_rec_t;

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t len;

    if (dst_len == 0) {
        return MIHFT_ERR_PARSE;
    }

    len = strlen(src);
    if (len >= dst_len) {
        return MIHFT_ERR_PARSE;
    }

    memcpy(dst, src, len + 1);
    return 0;
}

static char *trim_line(char *s)
{
    size_t len;

    while (*s == ' ' || *s == '\t') {
        s++;
    }

    len = strlen(s);
    while (len > 0 && (s[len - 1] == '\n' || s[len - 1] == '\r' ||
                       s[len - 1] == ' ' || s[len - 1] == '\t')) {
        s[--len] = '\0';
    }

    return s;
}

static int split_csv(char *line, char **fields, size_t expect)
{
    size_t count = 0;
    char *p = line;

    while (count < expect) {
        fields[count++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    if (count != expect || strchr(fields[expect - 1], ',') != NULL) {
        return MIHFT_ERR_PARSE;
    }

    for (count = 0; count < expect; count++) {
        fields[count] = trim_line(fields[count]);
    }

    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *endp;
    long long v;

    if (*s == '\0') {
        return MIHFT_ERR_PARSE;
    }

    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || *endp != '\0') {
        return MIHFT_ERR_PARSE;
    }

    *out = (int64_t)v;
    return 0;
}

static int parse_exec_line(char *line, sc_exec_rec_t *rec)
{
    char *f[7];
    int rc;

    rc = split_csv(line, f, 7);
    if (rc != 0) {
        return rc;
    }

    if (copy_field(rec->exec_id, sizeof(rec->exec_id), f[0]) != 0 ||
        copy_field(rec->order_id, sizeof(rec->order_id), f[1]) != 0 ||
        copy_field(rec->instr_code, sizeof(rec->instr_code), f[2]) != 0 ||
        copy_field(rec->exec_ts, sizeof(rec->exec_ts), f[6]) != 0) {
        return MIHFT_ERR_PARSE;
    }

    if ((f[3][0] != 'B' && f[3][0] != 'S') || f[3][1] != '\0') {
        return MIHFT_ERR_PARSE;
    }
    rec->side_kbn = f[3][0];

    if (parse_i64(f[4], &rec->fill_qty) != 0 ||
        parse_i64(f[5], &rec->fill_amt) != 0) {
        return MIHFT_ERR_PARSE;
    }

    if (rec->fill_qty < 0 || rec->fill_amt < 0) {
        return MIHFT_ERR_PARSE;
    }

    return 0;
}

static int parse_order_line(char *line, sc_order_rec_t *rec)
{
    char *f[8];
    int rc;

    rc = split_csv(line, f, 8);
    if (rc != 0) {
        return rc;
    }

    if (copy_field(rec->order_id, sizeof(rec->order_id), f[0]) != 0 ||
        copy_field(rec->cif_no, sizeof(rec->cif_no), f[1]) != 0 ||
        copy_field(rec->instr_code, sizeof(rec->instr_code), f[2]) != 0 ||
        copy_field(rec->state_kbn, sizeof(rec->state_kbn), f[3]) != 0 ||
        copy_field(rec->last_upd_ts, sizeof(rec->last_upd_ts), f[7]) != 0) {
        return MIHFT_ERR_PARSE;
    }

    if (parse_i64(f[4], &rec->leaves_qty) != 0 ||
        parse_i64(f[5], &rec->cum_qty) != 0 ||
        parse_i64(f[6], &rec->avg_fill_amt) != 0) {
        return MIHFT_ERR_PARSE;
    }

    if (rec->leaves_qty < 0 || rec->cum_qty < 0 || rec->avg_fill_amt < 0) {
        return MIHFT_ERR_PARSE;
    }

    return 0;
}

static int read_execs(sc_exec_rec_t *execs, size_t *exec_count)
{
    FILE *fp;
    char line[LINE_MAX_LEN];
    size_t n = 0;

    fp = fopen(SCEXEC_PATH, "r");
    if (fp == NULL) {
        fprintf(stderr, "SCEXEC読込失敗\n");
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *body = trim_line(line);

        if (*body == '\0') {
            continue;
        }
        if (n >= MAX_EXECS) {
            fclose(fp);
            fprintf(stderr, "SCEXEC件数超過\n");
            return MIHFT_ERR_PARSE;
        }
        if (parse_exec_line(body, &execs[n]) != 0) {
            fclose(fp);
            fprintf(stderr, "SCEXEC解析失敗\n");
            return MIHFT_ERR_PARSE;
        }
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCEXEC読込中断\n");
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    *exec_count = n;
    return 0;
}

static int read_orders(sc_order_rec_t *orders, size_t *order_count)
{
    FILE *fp;
    char line[LINE_MAX_LEN];
    size_t n = 0;

    fp = fopen(SCORDS_PATH, "r");
    if (fp == NULL) {
        fprintf(stderr, "SCORDS読込失敗\n");
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *body = trim_line(line);

        if (*body == '\0') {
            continue;
        }
        if (n >= MAX_ORDS) {
            fclose(fp);
            fprintf(stderr, "SCORDS件数超過\n");
            return MIHFT_ERR_PARSE;
        }
        if (parse_order_line(body, &orders[n]) != 0) {
            fclose(fp);
            fprintf(stderr, "SCORDS解析失敗\n");
            return MIHFT_ERR_PARSE;
        }
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCORDS読込中断\n");
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    *order_count = n;
    return 0;
}

static sc_order_rec_t *find_order(sc_order_rec_t *orders, size_t order_count,
                                  const char *order_id)
{
    size_t i;

    for (i = 0; i < order_count; i++) {
        if (strcmp(orders[i].order_id, order_id) == 0) {
            return &orders[i];
        }
    }

    return NULL;
}

static int checked_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return MIHFT_ERR_OVERFLOW;
    }

    *out = a + b;
    return 0;
}

static int checked_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a != 0 && b > INT64_MAX / a) {
        return MIHFT_ERR_OVERFLOW;
    }

    *out = a * b;
    return 0;
}

static int update_average(sc_order_rec_t *ord, const sc_exec_rec_t *exe)
{
    int64_t old_notional;
    int64_t fill_notional;
    int64_t total_notional;
    int64_t new_cum;

    if (checked_add_i64(ord->cum_qty, exe->fill_qty, &new_cum) != 0) {
        return MIHFT_ERR_OVERFLOW;
    }

    if (new_cum == 0) {
        ord->avg_fill_amt = 0;
        return 0;
    }

    if (checked_mul_i64(ord->avg_fill_amt, ord->cum_qty, &old_notional) != 0 ||
        checked_mul_i64(exe->fill_amt, exe->fill_qty, &fill_notional) != 0 ||
        checked_add_i64(old_notional, fill_notional, &total_notional) != 0) {
        return MIHFT_ERR_OVERFLOW;
    }

    if (total_notional > MIHFT_MAX_NOTIONAL) {
        return MIHFT_ERR_OVERFLOW;
    }

    ord->avg_fill_amt = (total_notional + (new_cum / 2)) / new_cum;
    return 0;
}

static int apply_exec(sc_order_rec_t *ord, const sc_exec_rec_t *exe)
{
    int rc;

    if (strcmp(ord->instr_code, exe->instr_code) != 0) {
        return MIHFT_ERR_PARSE;
    }

    if (exe->fill_qty > ord->leaves_qty) {
        copy_field(ord->state_kbn, sizeof(ord->state_kbn), "R");
        copy_field(ord->last_upd_ts, sizeof(ord->last_upd_ts), exe->exec_ts);
        return 0;
    }

    rc = update_average(ord, exe);
    if (rc != 0) {
        return rc;
    }

    ord->cum_qty += exe->fill_qty;
    ord->leaves_qty -= exe->fill_qty;

    if (ord->leaves_qty == 0) {
        copy_field(ord->state_kbn, sizeof(ord->state_kbn), "F");
    } else if (exe->fill_qty == 0) {
        copy_field(ord->state_kbn, sizeof(ord->state_kbn), "C");
    } else {
        copy_field(ord->state_kbn, sizeof(ord->state_kbn), "P");
    }

    return copy_field(ord->last_upd_ts, sizeof(ord->last_upd_ts), exe->exec_ts);
}

static int write_orders(const sc_order_rec_t *orders, size_t order_count)
{
    FILE *fp;
    size_t i;

    fp = fopen(SCORDS_OUT_PATH, "w");
    if (fp == NULL) {
        fprintf(stderr, "SCORDS出力失敗\n");
        return MIHFT_ERR_IO;
    }

    for (i = 0; i < order_count; i++) {
        if (fprintf(fp, "%s,%s,%s,%s,%" PRId64 ",%" PRId64 ",%" PRId64 ",%s\n",
                    orders[i].order_id,
                    orders[i].cif_no,
                    orders[i].instr_code,
                    orders[i].state_kbn,
                    orders[i].leaves_qty,
                    orders[i].cum_qty,
                    orders[i].avg_fill_amt,
                    orders[i].last_upd_ts) < 0) {
            fclose(fp);
            fprintf(stderr, "SCORDS書込失敗\n");
            return MIHFT_ERR_IO;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCORDS終了処理失敗\n");
        return MIHFT_ERR_IO;
    }

    return 0;
}

int main(void)
{
    sc_exec_rec_t execs[MAX_EXECS];
    sc_order_rec_t orders[MAX_ORDS];
    size_t exec_count = 0;
    size_t order_count = 0;
    size_t i;
    int rc;

    rc = read_execs(execs, &exec_count);
    if (rc != 0) {
        return rc;
    }

    rc = read_orders(orders, &order_count);
    if (rc != 0) {
        return rc;
    }

    for (i = 0; i < exec_count; i++) {
        sc_order_rec_t *ord = find_order(orders, order_count, execs[i].order_id);

        if (ord == NULL) {
            fprintf(stderr, "注文未検出\n");
            return MIHFT_ERR_PARSE;
        }

        rc = apply_exec(ord, &execs[i]);
        if (rc != 0) {
            fprintf(stderr, "注文状態更新失敗\n");
            return rc;
        }
    }

    rc = write_orders(orders, order_count);
    if (rc != 0) {
        return rc;
    }

    return 0;
}
