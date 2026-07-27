/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240702  精算基盤  初版作成
 * 1.01  20241203  精算基盤  金額桁あふれ検知を追加
 * 1.02  20250415  精算基盤  同一キー内の精算件数二重計上抑止を追加
 */

#include "mipay_settle.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PSTXNF_IN_ENV "MIPAY_PSTXNF_IN"
#define PSTXNF_OUT_ENV "MIPAY_PSTXNF_OUT"
#define PSTXNF_ERR_ENV "MIPAY_PSTXNF_ERR"
#define PSTXNF_IN_DEF "PSTXNF.csv"
#define PSTXNF_OUT_DEF "PSTXNF.dedup.csv"
#define PSTXNF_ERR_DEF "PSTXNF.error.csv"
#define LINE_MAX_LEN 512
#define FIELD_MAX_LEN 64
#define INIT_CAPACITY 4096
#define EXIT_PARSE_IO 2

typedef struct {
    char txn_id[FIELD_MAX_LEN];
    char merchant_code[FIELD_MAX_LEN];
    char txn_kbn;
    int64_t txn_amt;
    char txn_dt[FIELD_MAX_LEN];
    size_t seq;
} pstxnf_record;

static void rstrip(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static const char *skip_space(const char *s)
{
    while (*s != '\0' && isspace((unsigned char)*s)) {
        ++s;
    }
    return s;
}

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t len;
    const char *end;

    src = skip_space(src);
    end = src + strlen(src);
    while (end > src && isspace((unsigned char)end[-1])) {
        --end;
    }

    len = (size_t)(end - src);
    if (len == 0 || len >= dst_len) {
        return -1;
    }

    memcpy(dst, src, len);
    dst[len] = '\0';
    return 0;
}

static int parse_amount(const char *field, int64_t *out)
{
    int64_t value = 0;
    const char *p = skip_space(field);
    const char *end = field + strlen(field);

    while (end > p && isspace((unsigned char)end[-1])) {
        --end;
    }
    if (p == end) {
        return -1;
    }

    while (p < end) {
        int digit;

        if (!isdigit((unsigned char)*p)) {
            return -1;
        }
        digit = *p - '0';
        if (value > (INT64_MAX - digit) / 10) {
            return -1;
        }
        value = value * 10 + digit;
        ++p;
    }

    *out = value;
    return 0;
}

static int parse_kind(const char *field, char *out)
{
    char work[FIELD_MAX_LEN];

    if (copy_field(work, sizeof(work), field) != 0) {
        return -1;
    }
    if (work[1] != '\0' || (work[0] != 'C' && work[0] != 'R')) {
        return -1;
    }

    *out = work[0];
    return 0;
}

static int split_csv5(char *line, char *fields[5])
{
    size_t i = 0;
    char *p = line;

    fields[i++] = p;
    while (*p != '\0') {
        if (*p == ',') {
            if (i == 5) {
                return -1;
            }
            *p = '\0';
            fields[i++] = p + 1;
        }
        ++p;
    }

    return i == 5 ? 0 : -1;
}

static int parse_pstxnf_line(char *line, size_t seq, pstxnf_record *rec)
{
    char *fields[5];

    rstrip(line);
    if (split_csv5(line, fields) != 0) {
        return -1;
    }
    if (copy_field(rec->txn_id, sizeof(rec->txn_id), fields[0]) != 0) {
        return -1;
    }
    if (copy_field(rec->merchant_code, sizeof(rec->merchant_code), fields[1]) != 0) {
        return -1;
    }
    if (parse_kind(fields[2], &rec->txn_kbn) != 0) {
        return -1;
    }
    if (parse_amount(fields[3], &rec->txn_amt) != 0) {
        return -1;
    }
    if (copy_field(rec->txn_dt, sizeof(rec->txn_dt), fields[4]) != 0) {
        return -1;
    }

    rec->seq = seq;
    return 0;
}

static int key_compare(const void *a, const void *b)
{
    const pstxnf_record *ra = (const pstxnf_record *)a;
    const pstxnf_record *rb = (const pstxnf_record *)b;
    int c = strcmp(ra->txn_id, rb->txn_id);

    if (c != 0) {
        return c;
    }

    c = strcmp(ra->merchant_code, rb->merchant_code);
    if (c != 0) {
        return c;
    }

    if (ra->seq < rb->seq) {
        return -1;
    }
    if (ra->seq > rb->seq) {
        return 1;
    }
    return 0;
}

static int same_key(const pstxnf_record *a, const pstxnf_record *b)
{
    return strcmp(a->txn_id, b->txn_id) == 0 &&
           strcmp(a->merchant_code, b->merchant_code) == 0;
}

static int same_payload(const pstxnf_record *a, const pstxnf_record *b)
{
    return a->txn_kbn == b->txn_kbn &&
           a->txn_amt == b->txn_amt &&
           strcmp(a->txn_dt, b->txn_dt) == 0;
}

static int write_record(FILE *fp, const pstxnf_record *rec)
{
    return fprintf(fp, "%s,%s,%c,%lld,%s\n",
                   rec->txn_id,
                   rec->merchant_code,
                   rec->txn_kbn,
                   (long long)rec->txn_amt,
                   rec->txn_dt) < 0 ? -1 : 0;
}

static int append_record(pstxnf_record **records, size_t *count, size_t *capacity,
                         const pstxnf_record *rec)
{
    pstxnf_record *next;
    size_t new_capacity;

    if (*count < *capacity) {
        (*records)[(*count)++] = *rec;
        return 0;
    }

    new_capacity = *capacity == 0 ? INIT_CAPACITY : *capacity * 2;
    if (new_capacity < *capacity || new_capacity > SIZE_MAX / sizeof(**records)) {
        return -1;
    }

    next = (pstxnf_record *)realloc(*records, new_capacity * sizeof(**records));
    if (next == NULL) {
        return -1;
    }

    *records = next;
    *capacity = new_capacity;
    (*records)[(*count)++] = *rec;
    return 0;
}

static int read_input(const char *path, pstxnf_record **records, size_t *count)
{
    FILE *fp;
    char line[LINE_MAX_LEN];
    size_t capacity = 0;
    size_t seq = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "E001:入力ファイルを開けません:%s\n", path);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        pstxnf_record rec;

        ++seq;
        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fprintf(stderr, "E002:入力行が長すぎます:%zu\n", seq);
            fclose(fp);
            return -1;
        }
        if (seq == 1 && strncmp(line, "TXN-ID,", 7) == 0) {
            continue;
        }
        if (parse_pstxnf_line(line, seq, &rec) != 0) {
            fprintf(stderr, "E003:入力形式不正:%zu\n", seq);
            fclose(fp);
            return -1;
        }
        if (append_record(records, count, &capacity, &rec) != 0) {
            fprintf(stderr, "E004:入力領域不足:%zu\n", seq);
            fclose(fp);
            return -1;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E005:入力読取失敗:%s\n", path);
        fclose(fp);
        return -1;
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "E006:入力クローズ失敗:%s\n", path);
        return -1;
    }

    return 0;
}

static int write_outputs(const char *out_path, const char *err_path,
                         pstxnf_record *records, size_t count)
{
    FILE *out_fp;
    FILE *err_fp;
    size_t i = 0;

    out_fp = fopen(out_path, "w");
    if (out_fp == NULL) {
        fprintf(stderr, "E101:出力ファイルを開けません:%s\n", out_path);
        return -1;
    }

    err_fp = fopen(err_path, "w");
    if (err_fp == NULL) {
        fprintf(stderr, "E102:エラーファイルを開けません:%s\n", err_path);
        fclose(out_fp);
        return -1;
    }

    if (fprintf(out_fp, "TXN-ID,MERCHANT-CODE,TXN-KBN,TXN-AMT,TXN-DT\n") < 0 ||
        fprintf(err_fp, "TXN-ID,MERCHANT-CODE,TXN-KBN,TXN-AMT,TXN-DT\n") < 0) {
        fprintf(stderr, "E103:ヘッダ出力失敗\n");
        fclose(err_fp);
        fclose(out_fp);
        return -1;
    }

    while (i < count) {
        size_t j = i + 1;
        int conflict = 0;

        while (j < count && same_key(&records[i], &records[j])) {
            if (!same_payload(&records[i], &records[j])) {
                conflict = 1;
            }
            ++j;
        }

        if (conflict) {
            size_t k;

            for (k = i; k < j; ++k) {
                if (write_record(err_fp, &records[k]) != 0) {
                    fprintf(stderr, "E104:重複エラー出力失敗\n");
                    fclose(err_fp);
                    fclose(out_fp);
                    return -1;
                }
            }
        } else if (write_record(out_fp, &records[i]) != 0) {
            fprintf(stderr, "E105:重複排除出力失敗\n");
            fclose(err_fp);
            fclose(out_fp);
            return -1;
        }

        i = j;
    }

    if (fclose(err_fp) != 0) {
        fprintf(stderr, "E106:エラーファイルクローズ失敗:%s\n", err_path);
        fclose(out_fp);
        return -1;
    }
    if (fclose(out_fp) != 0) {
        fprintf(stderr, "E107:出力ファイルクローズ失敗:%s\n", out_path);
        return -1;
    }

    return 0;
}

int main(void)
{
    const char *in_path = getenv(PSTXNF_IN_ENV);
    const char *out_path = getenv(PSTXNF_OUT_ENV);
    const char *err_path = getenv(PSTXNF_ERR_ENV);
    pstxnf_record *records = NULL;
    size_t count = 0;

    if (in_path == NULL || *in_path == '\0') {
        in_path = PSTXNF_IN_DEF;
    }
    if (out_path == NULL || *out_path == '\0') {
        out_path = PSTXNF_OUT_DEF;
    }
    if (err_path == NULL || *err_path == '\0') {
        err_path = PSTXNF_ERR_DEF;
    }

    if (read_input(in_path, &records, &count) != 0) {
        free(records);
        return EXIT_PARSE_IO;
    }

    qsort(records, count, sizeof(records[0]), key_compare);

    if (write_outputs(out_path, err_path, records, count) != 0) {
        free(records);
        return EXIT_PARSE_IO;
    }

    free(records);
    return 0;
}
