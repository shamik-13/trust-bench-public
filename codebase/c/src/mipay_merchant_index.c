/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240325  精算連携  初版作成
 * 1.01  20240708  精算連携  加盟店コード直接照会と停止判定を追加
 */

#include "mipay_trace.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIPAY_RC_PARSE_IO 1
#define MIPAY_RC_FOUND_ACTIVE 0
#define MIPAY_RC_FOUND_STOPPED 2
#define MIPAY_RC_NOT_FOUND 3

#define MIPAY_FIELD_MAX 128
#define MIPAY_LINE_MAX 768
#define MIPAY_NAME_MAX 96
#define MIPAY_CODE_MAX 32
#define MIPAY_BANK_MAX 16
#define MIPAY_ACCOUNT_MAX 32

typedef struct {
    char merchant_code[MIPAY_CODE_MAX];
    char merchant_name[MIPAY_NAME_MAX];
    char bank_code[MIPAY_BANK_MAX];
    char account_no[MIPAY_ACCOUNT_MAX];
    int active_flag;
    int risk_rank;
    int has_account;
} merchant_index_record_t;

static void trim_inplace(char *s)
{
    size_t len;
    char *p;

    while (isspace((unsigned char)*s)) {
        memmove(s, s + 1, strlen(s));
    }

    len = strlen(s);
    while (len > 0 && isspace((unsigned char)s[len - 1])) {
        s[--len] = '\0';
    }

    if (len >= 2 && s[0] == '"' && s[len - 1] == '"') {
        memmove(s, s + 1, len - 2);
        s[len - 2] = '\0';
        for (p = s; *p != '\0'; ++p) {
            if (p[0] == '"' && p[1] == '"') {
                memmove(p, p + 1, strlen(p));
            }
        }
    }
}

static int copy_checked(char *dst, size_t dst_size, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dst_size) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_int_field(const char *s, int min_value, int max_value, int *out)
{
    char *end = NULL;
    long v;

    if (*s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0' || v < min_value || v > max_value) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int split_csv_line(char *line, char fields[][MIPAY_FIELD_MAX], size_t expected)
{
    size_t count = 0;
    int quoted = 0;
    char cell[MIPAY_FIELD_MAX];
    size_t cell_len = 0;
    char *p;

    for (p = line; ; ++p) {
        unsigned char c = (unsigned char)*p;

        if (c == '"' && quoted && p[1] == '"') {
            if (cell_len + 1 >= sizeof(cell)) {
                return -1;
            }
            cell[cell_len++] = '"';
            ++p;
            continue;
        }

        if (c == '"') {
            quoted = !quoted;
            if (cell_len + 1 >= sizeof(cell)) {
                return -1;
            }
            cell[cell_len++] = (char)c;
            continue;
        }

        if ((c == ',' && !quoted) || c == '\0' || c == '\n' || c == '\r') {
            if (count >= expected || cell_len >= sizeof(cell)) {
                return -1;
            }
            cell[cell_len] = '\0';
            memcpy(fields[count], cell, cell_len + 1);
            trim_inplace(fields[count]);
            ++count;
            cell_len = 0;

            if (c == '\0' || c == '\n' || c == '\r') {
                break;
            }
            continue;
        }

        if (cell_len + 1 >= sizeof(cell)) {
            return -1;
        }
        cell[cell_len++] = (char)c;
    }

    return quoted || count != expected ? -1 : 0;
}

static int is_header_record(char fields[][MIPAY_FIELD_MAX])
{
    return strcmp(fields[0], "MERCHANT-CODE") == 0 ||
           strcmp(fields[0], "merchant_code") == 0 ||
           strcmp(fields[0], "加盟店コード") == 0;
}

static int parse_pjmstf_record(char *line, merchant_index_record_t *rec)
{
    char fields[6][MIPAY_FIELD_MAX];
    int active;
    int risk;

    if (split_csv_line(line, fields, 6) != 0) {
        return -1;
    }

    if (is_header_record(fields)) {
        return 1;
    }

    if (copy_checked(rec->merchant_code, sizeof(rec->merchant_code), fields[0]) != 0 ||
        copy_checked(rec->merchant_name, sizeof(rec->merchant_name), fields[1]) != 0 ||
        copy_checked(rec->bank_code, sizeof(rec->bank_code), fields[2]) != 0 ||
        copy_checked(rec->account_no, sizeof(rec->account_no), fields[3]) != 0) {
        return -1;
    }

    if (parse_int_field(fields[4], 0, 1, &active) != 0 ||
        parse_int_field(fields[5], 0, 9, &risk) != 0) {
        return -1;
    }

    rec->active_flag = active;
    rec->risk_rank = risk;
    rec->has_account = rec->bank_code[0] != '\0' && rec->account_no[0] != '\0';
    return 0;
}

static int lookup_merchant(FILE *fp, const char *target_code, merchant_index_record_t *out)
{
    char line[MIPAY_LINE_MAX];
    unsigned long line_no = 0;

    while (fgets(line, sizeof(line), fp) != NULL) {
        merchant_index_record_t rec;
        int parsed;

        ++line_no;
        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fprintf(stderr, "PJMSTF行長過大:%lu\n", line_no);
            return MIPAY_RC_PARSE_IO;
        }

        parsed = parse_pjmstf_record(line, &rec);
        if (parsed > 0) {
            continue;
        }
        if (parsed < 0) {
            fprintf(stderr, "PJMSTF形式不正:%lu\n", line_no);
            return MIPAY_RC_PARSE_IO;
        }

        if (strcmp(rec.merchant_code, target_code) == 0) {
            *out = rec;
            return rec.active_flag ? MIPAY_RC_FOUND_ACTIVE : MIPAY_RC_FOUND_STOPPED;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "PJMSTF読込失敗\n");
        return MIPAY_RC_PARSE_IO;
    }

    return MIPAY_RC_NOT_FOUND;
}

static int valid_merchant_code(const char *s)
{
    size_t i;
    size_t n = strlen(s);

    if (n == 0 || n >= MIPAY_CODE_MAX) {
        return 0;
    }

    for (i = 0; i < n; ++i) {
        unsigned char c = (unsigned char)s[i];
        if (!isalnum(c) && c != '-' && c != '_') {
            return 0;
        }
    }

    return 1;
}

int main(void)
{
    const char *path = getenv("PJMSTF_PATH");
    const char *target = getenv("MIPAY_MERCHANT_CODE");
    FILE *fp;
    merchant_index_record_t rec;
    int rc;

    if (path == NULL || *path == '\0') {
        path = "PJMSTF.csv";
    }

    if (target == NULL || !valid_merchant_code(target)) {
        fprintf(stderr, "加盟店コード指定不正\n");
        return MIPAY_RC_PARSE_IO;
    }

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "PJMSTFオープン失敗\n");
        return MIPAY_RC_PARSE_IO;
    }

    rc = lookup_merchant(fp, target, &rec);
    if (fclose(fp) != 0 && rc != MIPAY_RC_PARSE_IO) {
        fprintf(stderr, "PJMSTFクローズ失敗\n");
        return MIPAY_RC_PARSE_IO;
    }

    if (rc == MIPAY_RC_FOUND_ACTIVE || rc == MIPAY_RC_FOUND_STOPPED) {
        printf("%s,%s,%d,%d,%d\n",
               rec.merchant_code,
               rec.merchant_name,
               rec.active_flag,
               rec.has_account,
               rec.risk_rank);
        return rc;
    }

    if (rc == MIPAY_RC_NOT_FOUND) {
        printf("%s,未登録,0,0,9\n", target);
        return rc;
    }

    return MIPAY_RC_PARSE_IO;
}
