/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240310  精算連携  初版作成
 * 1.01  20240725  精算連携  CSV桁あふれ検査と精算区分差分判定を追加
 * 1.02  20241118  精算連携  出力済明細と投入CSVの最新使用日集計を統合
 */

#include "mipay_trace.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIPAY_KBN_OK_DELETE        0
#define MIPAY_KBN_OK_SHORTEN       1
#define MIPAY_KBN_NG_USED          2
#define MIPAY_KBN_ERR_IO          91
#define MIPAY_KBN_ERR_PARSE       92
#define MIPAY_KBN_ERR_CONFIG      93

#define MIPAY_NET_DIFF_NONE        0u
#define MIPAY_NET_DIFF_MST         1u
#define MIPAY_NET_DIFF_DTL         2u
#define MIPAY_NET_DIFF_STAGE       4u

#define MIPAY_MAX_LINE          1024
#define MIPAY_MAX_FIELDS          16
#define MIPAY_MAX_KBN_NAME        64
#define MIPAY_MAX_CODE            32
#define MIPAY_MAX_ID              40

typedef struct {
    int settle_kbn;
    char kbn_name[MIPAY_MAX_KBN_NAME];
    int nettable_flag;
    long fee_rate_ppm;
    int valid_from;
    int valid_to;
} pckbnf_row_t;

typedef struct {
    char detail_id[MIPAY_MAX_ID];
    char settle_txn_id[MIPAY_MAX_ID];
    char merchant_code[MIPAY_MAX_CODE];
    int64_t txn_amt;
    int settle_kbn;
    int output_status;
} pcdtlf_row_t;

typedef struct {
    char settle_txn_id[MIPAY_MAX_ID];
    char merchant_code[MIPAY_MAX_CODE];
    int64_t txn_amt;
    int settle_kbn;
} ptsetf_row_t;

typedef struct {
    int target_kbn;
    int master_seen;
    int target_nettable;
    unsigned long used_count;
    int latest_use_date;
    unsigned net_diff_mask;
} scan_result_t;

static void trim_field(char *s)
{
    size_t n;
    char *p = s;

    while (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n') {
        ++p;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1u);
    }

    n = strlen(s);
    while (n > 0u) {
        unsigned char c = (unsigned char)s[n - 1u];
        if (c != ' ' && c != '\t' && c != '\r' && c != '\n') {
            break;
        }
        s[--n] = '\0';
    }
}

static int copy_text(char *dst, size_t cap, const char *src)
{
    size_t n = strlen(src);

    if (n >= cap) {
        return -1;
    }
    memcpy(dst, src, n + 1u);
    return 0;
}

static int split_csv(char *line, char *fields[], size_t max_fields, size_t *count)
{
    size_t n = 0u;
    char *w = line;
    char *r = line;
    int quoted = 0;

    if (max_fields == 0u) {
        return -1;
    }

    fields[n++] = w;
    while (*r != '\0') {
        if (*r == '"') {
            if (quoted && r[1] == '"') {
                *w++ = '"';
                r += 2;
                continue;
            }
            quoted = !quoted;
            ++r;
            continue;
        }
        if (*r == ',' && !quoted) {
            *w++ = '\0';
            if (n >= max_fields) {
                return -1;
            }
            fields[n++] = w;
            ++r;
            continue;
        }
        *w++ = *r++;
    }
    if (quoted) {
        return -1;
    }
    *w = '\0';

    for (size_t i = 0u; i < n; ++i) {
        trim_field(fields[i]);
    }
    *count = n;
    return 0;
}

static int parse_i64(const char *s, int64_t minv, int64_t maxv, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (*s == '\0') {
        return -1;
    }
    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }
    if (v < minv || v > maxv) {
        return -1;
    }
    *out = (int64_t)v;
    return 0;
}

static int parse_int(const char *s, int minv, int maxv, int *out)
{
    int64_t v;

    if (parse_i64(s, minv, maxv, &v) != 0) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int parse_date_yyyymmdd(const char *s, int allow_zero, int *out)
{
    int y;
    int m;
    int d;
    int v;

    if (allow_zero && strcmp(s, "0") == 0) {
        *out = 0;
        return 0;
    }
    if (strlen(s) != 8u || parse_int(s, 19000101, 20991231, &v) != 0) {
        return -1;
    }

    y = v / 10000;
    m = (v / 100) % 100;
    d = v % 100;

    if (m < 1 || m > 12 || d < 1 || d > 31) {
        return -1;
    }
    if ((m == 4 || m == 6 || m == 9 || m == 11) && d > 30) {
        return -1;
    }
    if (m == 2) {
        int leap = ((y % 4 == 0 && y % 100 != 0) || (y % 400 == 0));
        if (d > (leap ? 29 : 28)) {
            return -1;
        }
    }

    *out = v;
    return 0;
}

static int parse_settle_kbn(const char *s, int *out)
{
    int v;

    if (parse_int(s, 1, 9, &v) != 0) {
        return -1;
    }
    if (v != 1 && v != 2 && v != 9) {
        return -1;
    }
    *out = v;
    return 0;
}

static int parse_pckbnf(char *line, pckbnf_row_t *row)
{
    char *f[MIPAY_MAX_FIELDS];
    size_t n = 0u;
    int64_t fee_ppm;

    if (split_csv(line, f, MIPAY_MAX_FIELDS, &n) != 0 || n != 6u) {
        return -1;
    }
    if (parse_settle_kbn(f[0], &row->settle_kbn) != 0) {
        return -1;
    }
    if (copy_text(row->kbn_name, sizeof row->kbn_name, f[1]) != 0) {
        return -1;
    }
    if (parse_int(f[2], 0, 1, &row->nettable_flag) != 0) {
        return -1;
    }
    if (parse_i64(f[3], 0, 1000000, &fee_ppm) != 0) {
        return -1;
    }
    row->fee_rate_ppm = (long)fee_ppm;
    if (parse_date_yyyymmdd(f[4], 0, &row->valid_from) != 0) {
        return -1;
    }
    if (parse_date_yyyymmdd(f[5], 1, &row->valid_to) != 0) {
        return -1;
    }
    if (row->valid_to != 0 && row->valid_from > row->valid_to) {
        return -1;
    }
    return 0;
}

static int parse_pcdtlf(char *line, pcdtlf_row_t *row)
{
    char *f[MIPAY_MAX_FIELDS];
    size_t n = 0u;

    if (split_csv(line, f, MIPAY_MAX_FIELDS, &n) != 0 || n != 6u) {
        return -1;
    }
    if (copy_text(row->detail_id, sizeof row->detail_id, f[0]) != 0) {
        return -1;
    }
    if (copy_text(row->settle_txn_id, sizeof row->settle_txn_id, f[1]) != 0) {
        return -1;
    }
    if (copy_text(row->merchant_code, sizeof row->merchant_code, f[2]) != 0) {
        return -1;
    }
    if (parse_i64(f[3], 0, INT64_C(999999999999), &row->txn_amt) != 0) {
        return -1;
    }
    if (parse_settle_kbn(f[4], &row->settle_kbn) != 0) {
        return -1;
    }
    if (parse_int(f[5], 0, 99, &row->output_status) != 0) {
        return -1;
    }
    return 0;
}

static int parse_ptsetf(char *line, ptsetf_row_t *row)
{
    char *f[MIPAY_MAX_FIELDS];
    size_t n = 0u;

    if (split_csv(line, f, MIPAY_MAX_FIELDS, &n) != 0 || n != 4u) {
        return -1;
    }
    if (copy_text(row->settle_txn_id, sizeof row->settle_txn_id, f[0]) != 0) {
        return -1;
    }
    if (copy_text(row->merchant_code, sizeof row->merchant_code, f[1]) != 0) {
        return -1;
    }
    if (parse_i64(f[2], 0, INT64_C(999999999999), &row->txn_amt) != 0) {
        return -1;
    }
    if (parse_settle_kbn(f[3], &row->settle_kbn) != 0) {
        return -1;
    }
    return 0;
}

static int txn_date_from_id(const char *id, int *yyyymmdd)
{
    char buf[9];

    if (strlen(id) < 8u) {
        return -1;
    }
    memcpy(buf, id, 8u);
    buf[8] = '\0';
    return parse_date_yyyymmdd(buf, 0, yyyymmdd);
}

static int is_data_line(const char *line)
{
    const unsigned char *p = (const unsigned char *)line;

    while (*p == ' ' || *p == '\t') {
        ++p;
    }
    return *p != '\0' && *p != '\r' && *p != '\n' && *p != '#';
}

static int read_master(scan_result_t *res)
{
    FILE *fp = fopen("PCKBNF.csv", "r");
    char line[MIPAY_MAX_LINE];
    unsigned long lineno = 0u;

    if (fp == NULL) {
        fprintf(stderr, "E91:PCKBNF読込不可:%d\n", errno);
        return MIPAY_KBN_ERR_IO;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        pckbnf_row_t row;

        ++lineno;
        if (!is_data_line(line)) {
            continue;
        }
        if (parse_pckbnf(line, &row) != 0) {
            fprintf(stderr, "E92:PCKBNF形式不正:%lu\n", lineno);
            fclose(fp);
            return MIPAY_KBN_ERR_PARSE;
        }
        if (row.settle_kbn == res->target_kbn) {
            res->master_seen = 1;
            res->target_nettable = row.nettable_flag;
            if (row.valid_to > res->latest_use_date) {
                res->latest_use_date = row.valid_to;
            }
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E91:PCKBNF読込中断:%d\n", errno);
        fclose(fp);
        return MIPAY_KBN_ERR_IO;
    }
    fclose(fp);

    if (!res->master_seen) {
        fprintf(stderr, "E93:精算区分未登録:%d\n", res->target_kbn);
        return MIPAY_KBN_ERR_CONFIG;
    }
    return 0;
}

static int read_detail(scan_result_t *res)
{
    FILE *fp = fopen("PCDTLF.csv", "r");
    char line[MIPAY_MAX_LINE];
    unsigned long lineno = 0u;

    if (fp == NULL) {
        fprintf(stderr, "E91:PCDTLF読込不可:%d\n", errno);
        return MIPAY_KBN_ERR_IO;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        pcdtlf_row_t row;
        int used_date = 0;

        ++lineno;
        if (!is_data_line(line)) {
            continue;
        }
        if (parse_pcdtlf(line, &row) != 0) {
            fprintf(stderr, "E92:PCDTLF形式不正:%lu\n", lineno);
            fclose(fp);
            return MIPAY_KBN_ERR_PARSE;
        }
        if (row.settle_kbn != res->target_kbn) {
            continue;
        }
        if (row.output_status == 20) {
            continue;
        }

        if (res->used_count == ULONG_MAX) {
            fprintf(stderr, "E92:使用件数桁あふれ\n");
            fclose(fp);
            return MIPAY_KBN_ERR_PARSE;
        }
        ++res->used_count;

        if (txn_date_from_id(row.settle_txn_id, &used_date) == 0 &&
            used_date > res->latest_use_date) {
            res->latest_use_date = used_date;
        }
        if (res->target_nettable == 0 && row.output_status == 30) {
            res->net_diff_mask |= MIPAY_NET_DIFF_DTL;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E91:PCDTLF読込中断:%d\n", errno);
        fclose(fp);
        return MIPAY_KBN_ERR_IO;
    }
    fclose(fp);
    return 0;
}

static int read_stage(scan_result_t *res)
{
    FILE *fp = fopen("PTSETF.csv", "r");
    char line[MIPAY_MAX_LINE];
    unsigned long lineno = 0u;

    if (fp == NULL) {
        fprintf(stderr, "E91:PTSETF読込不可:%d\n", errno);
        return MIPAY_KBN_ERR_IO;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        ptsetf_row_t row;
        int used_date = 0;

        ++lineno;
        if (!is_data_line(line)) {
            continue;
        }
        if (parse_ptsetf(line, &row) != 0) {
            fprintf(stderr, "E92:PTSETF形式不正:%lu\n", lineno);
            fclose(fp);
            return MIPAY_KBN_ERR_PARSE;
        }
        if (row.settle_kbn != res->target_kbn) {
            continue;
        }

        if (res->used_count == ULONG_MAX) {
            fprintf(stderr, "E92:使用件数桁あふれ\n");
            fclose(fp);
            return MIPAY_KBN_ERR_PARSE;
        }
        ++res->used_count;

        if (txn_date_from_id(row.settle_txn_id, &used_date) == 0 &&
            used_date > res->latest_use_date) {
            res->latest_use_date = used_date;
        }
        if (res->target_nettable == 0) {
            res->net_diff_mask |= MIPAY_NET_DIFF_STAGE;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E91:PTSETF読込中断:%d\n", errno);
        fclose(fp);
        return MIPAY_KBN_ERR_IO;
    }
    fclose(fp);
    return 0;
}

static int target_from_env(int *target)
{
    const char *s = getenv("SETTLE_KBN");

    if (s == NULL || *s == '\0') {
        s = getenv("MIPAY_TARGET_SETTLE_KBN");
    }
    if (s == NULL || parse_settle_kbn(s, target) != 0) {
        fprintf(stderr, "E93:対象精算区分未指定\n");
        return MIPAY_KBN_ERR_CONFIG;
    }
    return 0;
}

static int decide_result(const scan_result_t *res)
{
    if (res->used_count == 0u && res->net_diff_mask == MIPAY_NET_DIFF_NONE) {
        return MIPAY_KBN_OK_DELETE;
    }
    if (res->used_count == 0u) {
        return MIPAY_KBN_OK_SHORTEN;
    }
    return MIPAY_KBN_NG_USED;
}

int main(void)
{
    scan_result_t res;
    int rc;
    int decision;

    memset(&res, 0, sizeof res);
    res.latest_use_date = 0;

    rc = target_from_env(&res.target_kbn);
    if (rc != 0) {
        return rc;
    }

    rc = read_master(&res);
    if (rc != 0) {
        return rc;
    }
    if (res.target_nettable != 0 && res.target_kbn == 9) {
        res.net_diff_mask |= MIPAY_NET_DIFF_MST;
    }

    rc = read_detail(&res);
    if (rc != 0) {
        return rc;
    }

    rc = read_stage(&res);
    if (rc != 0) {
        return rc;
    }

    decision = decide_result(&res);
    printf("判定=%d,対象精算区分=%d,使用件数=%lu,最新使用日=%08d,ネット差分=%u\n",
           decision,
           res.target_kbn,
           res.used_count,
           res.latest_use_date,
           res.net_diff_mask);

    return decision;
}
