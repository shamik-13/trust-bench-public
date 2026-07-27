/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240505  精算連携  初版作成、PTSETF/PCKBNF突合およびPCDTLF固定長明細出力
 */
#include "mipay_trace.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PTSETF_PATH "PTSETF.csv"
#define PCKBNF_PATH "PCKBNF.csv"
#define PCDTLF_PATH "PCDTLF.dat"

#define MAX_LINE_LEN 512
#define MAX_KBN_ROWS 32
#define MAX_TXN_ID_LEN 32
#define MAX_MERCHANT_LEN 16
#define MAX_KBN_NAME_LEN 32
#define MAX_DATE_LEN 8
#define DETAIL_ID_WIDTH 12
#define DETAIL_REC_LEN 96

#define RC_NORMAL 0
#define RC_IO_ERROR 64
#define RC_PARSE_ERROR 65
#define RC_OVERFLOW_ERROR 66

typedef struct {
    char settle_txn_id[MAX_TXN_ID_LEN + 1];
    char merchant_code[MAX_MERCHANT_LEN + 1];
    int64_t txn_amt;
    int settle_kbn;
} PtsetfRecord;

typedef struct {
    int settle_kbn;
    char kbn_name[MAX_KBN_NAME_LEN + 1];
    int nettable_flag;
    int32_t fee_rate_ppm;
    char valid_from[MAX_DATE_LEN + 1];
    char valid_to[MAX_DATE_LEN + 1];
} PckbnfRecord;

typedef struct {
    PckbnfRecord rows[MAX_KBN_ROWS];
    size_t count;
} KbnTable;

static void trim_ascii(char *s)
{
    size_t head = 0;
    size_t tail = strlen(s);

    while (s[head] != '\0' && isspace((unsigned char)s[head])) {
        ++head;
    }
    while (tail > head && isspace((unsigned char)s[tail - 1])) {
        --tail;
    }
    if (head > 0) {
        memmove(s, s + head, tail - head);
    }
    s[tail - head] = '\0';
}

static int split_csv_line(char *line, char *cols[], size_t need)
{
    size_t n = 0;
    char *p = line;

    while (n < need) {
        cols[n++] = p;
        while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
            ++p;
        }
        if (*p == ',') {
            *p++ = '\0';
            continue;
        }
        if (*p == '\n' || *p == '\r') {
            *p = '\0';
        }
        break;
    }
    if (n != need) {
        return -1;
    }
    for (size_t i = 0; i < need; ++i) {
        trim_ascii(cols[i]);
    }
    return 0;
}

static int parse_int64_strict(const char *s, int64_t minv, int64_t maxv, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s[0] == '\0') {
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

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dst_len) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_date8(const char *s)
{
    if (strlen(s) != MAX_DATE_LEN) {
        return -1;
    }
    for (size_t i = 0; i < MAX_DATE_LEN; ++i) {
        if (!isdigit((unsigned char)s[i])) {
            return -1;
        }
    }
    return 0;
}

static int parse_ptsetf(char *line, PtsetfRecord *rec)
{
    char *cols[4];
    int64_t v;

    if (split_csv_line(line, cols, 4) != 0) {
        return -1;
    }
    if (copy_field(rec->settle_txn_id, sizeof(rec->settle_txn_id), cols[0]) != 0) {
        return -1;
    }
    if (copy_field(rec->merchant_code, sizeof(rec->merchant_code), cols[1]) != 0) {
        return -1;
    }
    if (parse_int64_strict(cols[2], 0, 999999999999LL, &rec->txn_amt) != 0) {
        return -1;
    }
    if (parse_int64_strict(cols[3], 1, 9, &v) != 0) {
        return -1;
    }
    rec->settle_kbn = (int)v;
    return 0;
}

static int parse_pckbnf(char *line, PckbnfRecord *rec)
{
    char *cols[6];
    int64_t v;

    if (split_csv_line(line, cols, 6) != 0) {
        return -1;
    }
    if (parse_int64_strict(cols[0], 1, 9, &v) != 0) {
        return -1;
    }
    rec->settle_kbn = (int)v;
    if (copy_field(rec->kbn_name, sizeof(rec->kbn_name), cols[1]) != 0) {
        return -1;
    }
    if (parse_int64_strict(cols[2], 0, 1, &v) != 0) {
        return -1;
    }
    rec->nettable_flag = (int)v;
    if (parse_int64_strict(cols[3], 0, 1000000, &v) != 0) {
        return -1;
    }
    rec->fee_rate_ppm = (int32_t)v;
    if (copy_field(rec->valid_from, sizeof(rec->valid_from), cols[4]) != 0 || parse_date8(rec->valid_from) != 0) {
        return -1;
    }
    if (copy_field(rec->valid_to, sizeof(rec->valid_to), cols[5]) != 0 || parse_date8(rec->valid_to) != 0) {
        return -1;
    }
    if (strcmp(rec->valid_from, rec->valid_to) > 0) {
        return -1;
    }
    return 0;
}

static int load_kbn_table(KbnTable *table)
{
    FILE *fp = fopen(PCKBNF_PATH, "r");
    char line[MAX_LINE_LEN];
    unsigned long lineno = 0;

    if (fp == NULL) {
        fprintf(stderr, "PCKBNFオープン失敗\n");
        return RC_IO_ERROR;
    }
    table->count = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        PckbnfRecord rec;

        ++lineno;
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '#') {
            continue;
        }
        if (parse_pckbnf(line, &rec) != 0) {
            fprintf(stderr, "PCKBNF形式不正 行=%lu\n", lineno);
            fclose(fp);
            return RC_PARSE_ERROR;
        }
        if (table->count >= MAX_KBN_ROWS) {
            fprintf(stderr, "PCKBNF件数上限超過\n");
            fclose(fp);
            return RC_OVERFLOW_ERROR;
        }
        for (size_t i = 0; i < table->count; ++i) {
            if (table->rows[i].settle_kbn == rec.settle_kbn) {
                fprintf(stderr, "PCKBNF区分重複 行=%lu\n", lineno);
                fclose(fp);
                return RC_PARSE_ERROR;
            }
        }
        table->rows[table->count++] = rec;
    }
    if (ferror(fp)) {
        fprintf(stderr, "PCKBNF読込失敗\n");
        fclose(fp);
        return RC_IO_ERROR;
    }
    fclose(fp);
    return RC_NORMAL;
}

static const PckbnfRecord *find_kbn(const KbnTable *table, int settle_kbn)
{
    for (size_t i = 0; i < table->count; ++i) {
        if (table->rows[i].settle_kbn == settle_kbn) {
            return &table->rows[i];
        }
    }
    return NULL;
}

static int put_fixed(char *buf, size_t off, size_t width, const char *s)
{
    size_t n = strlen(s);

    if (n > width) {
        return -1;
    }
    memcpy(buf + off, s, n);
    memset(buf + off + n, ' ', width - n);
    return 0;
}

static int write_detail(FILE *out, uint64_t detail_id, const PtsetfRecord *pt, const PckbnfRecord *kbn)
{
    char rec[DETAIL_REC_LEN + 2];
    char num[32];
    int n;

    memset(rec, ' ', sizeof(rec));
    n = snprintf(num, sizeof(num), "%012llu", (unsigned long long)detail_id);
    if (n != DETAIL_ID_WIDTH) {
        return -1;
    }
    memcpy(rec, num, DETAIL_ID_WIDTH);
    if (put_fixed(rec, 12, 32, pt->settle_txn_id) != 0) {
        return -1;
    }
    if (put_fixed(rec, 44, 16, pt->merchant_code) != 0) {
        return -1;
    }
    n = snprintf(num, sizeof(num), "%012lld", (long long)pt->txn_amt);
    if (n != 12) {
        return -1;
    }
    memcpy(rec + 60, num, 12);
    n = snprintf(num, sizeof(num), "%01d", pt->settle_kbn);
    if (n != 1) {
        return -1;
    }
    memcpy(rec + 72, num, 1);
    memcpy(rec + 73, kbn->nettable_flag == 1 ? "00" : "30", 2);
    rec[DETAIL_REC_LEN] = '\n';
    rec[DETAIL_REC_LEN + 1] = '\0';

    return fputs(rec, out) == EOF ? -1 : 0;
}

int main(void)
{
    KbnTable table;
    FILE *in;
    FILE *out;
    char line[MAX_LINE_LEN];
    uint64_t detail_id = 0;
    unsigned long lineno = 0;
    int detail_errors = 0;
    int rc = load_kbn_table(&table);

    if (rc != RC_NORMAL) {
        return rc;
    }

    in = fopen(PTSETF_PATH, "r");
    if (in == NULL) {
        fprintf(stderr, "PTSETFオープン失敗\n");
        return RC_IO_ERROR;
    }
    out = fopen(PCDTLF_PATH, "w");
    if (out == NULL) {
        fprintf(stderr, "PCDTLFオープン失敗\n");
        fclose(in);
        return RC_IO_ERROR;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        PtsetfRecord pt;
        const PckbnfRecord *kbn;

        ++lineno;
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '#') {
            continue;
        }
        if (parse_ptsetf(line, &pt) != 0) {
            fprintf(stderr, "PTSETF形式不正 行=%lu\n", lineno);
            fclose(out);
            fclose(in);
            return RC_PARSE_ERROR;
        }
        if (detail_id == UINT64_MAX) {
            fprintf(stderr, "DETAIL-ID上限超過\n");
            fclose(out);
            fclose(in);
            return RC_OVERFLOW_ERROR;
        }
        ++detail_id;
        kbn = find_kbn(&table, pt.settle_kbn);
        if (kbn == NULL) {
            if (detail_errors < INT_MAX) {
                ++detail_errors;
            }
            continue;
        }
        if (pt.settle_kbn == 9 || kbn->nettable_flag == 0) {
            continue;
        }
        if (write_detail(out, detail_id, &pt, kbn) != 0) {
            fprintf(stderr, "PCDTLF書込失敗 DETAIL-ID=%012llu\n", (unsigned long long)detail_id);
            fclose(out);
            fclose(in);
            return RC_IO_ERROR;
        }
    }

    if (ferror(in)) {
        fprintf(stderr, "PTSETF読込失敗\n");
        fclose(out);
        fclose(in);
        return RC_IO_ERROR;
    }
    if (fclose(out) != 0) {
        fprintf(stderr, "PCDTLFクローズ失敗\n");
        fclose(in);
        return RC_IO_ERROR;
    }
    if (fclose(in) != 0) {
        fprintf(stderr, "PTSETFクローズ失敗\n");
        return RC_IO_ERROR;
    }
    if (detail_errors > 0) {
        return detail_errors > 125 ? 125 : detail_errors;
    }
    return RC_NORMAL;
}
