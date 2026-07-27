/*
 * 変更履歴
 * 版数  年月日    担当    概要
 * 1.00  20230418  藤田 和也 (E-271)  約定発生時の保有残高差分更新を追加
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_RC_OK 0
#define MIHFT_RC_REJECT_HOLDING 4
#define MIHFT_RC_REJECT_NOTIONAL 8
#define MIHFT_RC_REJECT_FORMAT 12
#define MIHFT_RC_HARD_ERROR 16

#define MIHFT_MAX_LINE 512
#define MIHFT_MAX_HOLDINGS 4096
#define MIHFT_MAX_FIELD 64

typedef struct {
    char exec_id[MIHFT_MAX_FIELD];
    char order_id[MIHFT_MAX_FIELD];
    char instr_code[MIHFT_MAX_FIELD];
    char side_kbn;
    long long fill_qty;
    long long fill_amt;
    char exec_ts[MIHFT_MAX_FIELD];
} ExecRecord;

typedef struct {
    char cif_no[MIHFT_MAX_FIELD];
    char instr_code[MIHFT_MAX_FIELD];
    char asof_dt[MIHFT_MAX_FIELD];
    long long settled_qty;
    long long trade_qty;
    long long restricted_qty;
} HoldingRecord;

static void trim_field(char *s)
{
    size_t len;
    char *p = s;

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

static int split_csv(char *line, char *fields[], size_t need)
{
    size_t n = 0U;
    char *p = line;

    while (n < need) {
        fields[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    if (n != need || strchr(fields[need - 1U], ',') != NULL) {
        return -1;
    }

    for (size_t i = 0U; i < need; ++i) {
        trim_field(fields[i]);
    }
    return 0;
}

static int parse_i64(const char *s, long long *out)
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

    *out = v;
    return 0;
}

static int copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t len = strlen(src);

    if (len == 0U || len >= dstsz) {
        return -1;
    }
    memcpy(dst, src, len + 1U);
    return 0;
}

static int read_exec_record(const char *path, ExecRecord *rec)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];
    char *f[7];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "入力ファイルを開けません:%s\n", path);
        return -1;
    }

    if (fgets(line, sizeof line, fp) == NULL) {
        fprintf(stderr, "約定入力が空です:%s\n", path);
        fclose(fp);
        return -1;
    }

    line[strcspn(line, "\r\n")] = '\0';
    if (split_csv(line, f, 7U) != 0) {
        fprintf(stderr, "約定入力の項目数が不正です\n");
        fclose(fp);
        return -1;
    }

    if (copy_field(rec->exec_id, sizeof rec->exec_id, f[0]) != 0 ||
        copy_field(rec->order_id, sizeof rec->order_id, f[1]) != 0 ||
        copy_field(rec->instr_code, sizeof rec->instr_code, f[2]) != 0 ||
        copy_field(rec->exec_ts, sizeof rec->exec_ts, f[6]) != 0 ||
        strlen(f[3]) != 1U ||
        (f[3][0] != 'B' && f[3][0] != 'S') ||
        parse_i64(f[4], &rec->fill_qty) != 0 ||
        parse_i64(f[5], &rec->fill_amt) != 0 ||
        rec->fill_qty <= 0LL ||
        rec->fill_amt <= 0LL) {
        fprintf(stderr, "約定入力の値が不正です\n");
        fclose(fp);
        return -1;
    }

    rec->side_kbn = f[3][0];
    fclose(fp);
    return 0;
}

static int read_holdings(const char *path, HoldingRecord rows[], size_t cap, size_t *count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];
    size_t n = 0U;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "保有残高を開けません:%s\n", path);
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[6];

        if (n >= cap) {
            fprintf(stderr, "保有残高の件数上限を超過しました\n");
            fclose(fp);
            return -1;
        }

        line[strcspn(line, "\r\n")] = '\0';
        if (line[0] == '\0') {
            continue;
        }

        if (split_csv(line, f, 6U) != 0 ||
            copy_field(rows[n].cif_no, sizeof rows[n].cif_no, f[0]) != 0 ||
            copy_field(rows[n].instr_code, sizeof rows[n].instr_code, f[1]) != 0 ||
            copy_field(rows[n].asof_dt, sizeof rows[n].asof_dt, f[2]) != 0 ||
            parse_i64(f[3], &rows[n].settled_qty) != 0 ||
            parse_i64(f[4], &rows[n].trade_qty) != 0 ||
            parse_i64(f[5], &rows[n].restricted_qty) != 0 ||
            rows[n].settled_qty < 0LL ||
            rows[n].restricted_qty < 0LL) {
            fprintf(stderr, "保有残高の値が不正です\n");
            fclose(fp);
            return -1;
        }

        ++n;
    }

    if (ferror(fp)) {
        fprintf(stderr, "保有残高の読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int write_holdings(const char *path, const HoldingRecord rows[], size_t count)
{
    FILE *fp = fopen(path, "w");

    if (fp == NULL) {
        fprintf(stderr, "保有残高の出力を開けません:%s\n", path);
        return -1;
    }

    for (size_t i = 0U; i < count; ++i) {
        if (fprintf(fp, "%s,%s,%s,%lld,%lld,%lld\n",
                    rows[i].cif_no,
                    rows[i].instr_code,
                    rows[i].asof_dt,
                    rows[i].settled_qty,
                    rows[i].trade_qty,
                    rows[i].restricted_qty) < 0) {
            fprintf(stderr, "保有残高の出力に失敗しました\n");
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "保有残高の確定に失敗しました\n");
        return -1;
    }

    return 0;
}

static int find_target_holding(const HoldingRecord rows[], size_t count, const ExecRecord *exec, size_t *idx)
{
    size_t found = SIZE_MAX;

    for (size_t i = 0U; i < count; ++i) {
        if (strcmp(rows[i].instr_code, exec->instr_code) == 0) {
            if (found != SIZE_MAX) {
                fprintf(stderr, "対象銘柄の保有残高が複数存在します\n");
                return -1;
            }
            found = i;
        }
    }

    if (found == SIZE_MAX) {
        fprintf(stderr, "対象銘柄の保有残高が存在しません\n");
        return -1;
    }

    *idx = found;
    return 0;
}

static int add_i64_checked(long long a, long long b, long long *out)
{
    if ((b > 0LL && a > LLONG_MAX - b) ||
        (b < 0LL && a < LLONG_MIN - b)) {
        return -1;
    }
    *out = a + b;
    return 0;
}

int main(void)
{
    ExecRecord exec;
    HoldingRecord holdings[MIHFT_MAX_HOLDINGS];
    size_t holding_count = 0U;
    size_t target = 0U;
    long long available_qty;
    long long updated_trade_qty;

    if (read_exec_record("SCEXEC.csv", &exec) != 0) {
        return MIHFT_RC_HARD_ERROR;
    }

    if (exec.fill_amt > (long long)MIHFT_MAX_NOTIONAL) {
        return MIHFT_RC_REJECT_NOTIONAL;
    }

    if (read_holdings("SCHLDF.csv", holdings, MIHFT_MAX_HOLDINGS, &holding_count) != 0) {
        return MIHFT_RC_HARD_ERROR;
    }

    if (find_target_holding(holdings, holding_count, &exec, &target) != 0) {
        return MIHFT_RC_HARD_ERROR;
    }

    if (add_i64_checked(holdings[target].settled_qty, holdings[target].trade_qty, &available_qty) != 0) {
        fprintf(stderr, "保有可能数量の計算が桁あふれです\n");
        return MIHFT_RC_HARD_ERROR;
    }

    if (exec.side_kbn == 'B') {
        if (add_i64_checked(holdings[target].trade_qty, exec.fill_qty, &updated_trade_qty) != 0) {
            fprintf(stderr, "未決済数量の加算が桁あふれです\n");
            return MIHFT_RC_HARD_ERROR;
        }
    } else if (exec.side_kbn == 'S') {
        if (available_qty < exec.fill_qty) {
            return MIHFT_RC_REJECT_HOLDING;
        }
        if (add_i64_checked(holdings[target].trade_qty, -exec.fill_qty, &updated_trade_qty) != 0) {
            fprintf(stderr, "未決済数量の減算が桁あふれです\n");
            return MIHFT_RC_HARD_ERROR;
        }
    } else {
        return MIHFT_RC_REJECT_FORMAT;
    }

    holdings[target].trade_qty = updated_trade_qty;

    if (write_holdings("SCHLDF.out.csv", holdings, holding_count) != 0) {
        return MIHFT_RC_HARD_ERROR;
    }

    return MIHFT_RC_OK;
}
