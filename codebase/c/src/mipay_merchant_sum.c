/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    20240610    精算連携    初版作成
 * 1.01    20240920    精算連携    金額桁あふれ検査を追加
 * 1.02    20250130    精算連携    繰越候補判定と区分有効日検査を追加
 */

#include "mipay_trace.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIPAY_LINE_MAX 1024
#define MIPAY_CODE_MAX 32
#define MIPAY_NAME_MAX 64
#define MIPAY_PATH_DETAIL "PCDTLF.csv"
#define MIPAY_PATH_KBN "PCKBNF.csv"
#define MIPAY_PATH_SUM "PCSUMF.csv"
#define MIPAY_SETTLE_DATE "20250115"

typedef struct {
    char detail_id[MIPAY_CODE_MAX];
    char settle_txn_id[MIPAY_CODE_MAX];
    char merchant_code[MIPAY_CODE_MAX];
    long long txn_amt;
    char settle_kbn[MIPAY_CODE_MAX];
    char output_status[MIPAY_CODE_MAX];
} local_detail_record;

typedef struct {
    char settle_kbn[MIPAY_CODE_MAX];
    char kbn_name[MIPAY_NAME_MAX];
    int nettable_flag;
    long fee_rate_ppm;
    int valid_from;
    int valid_to;
} local_kbn_record;

typedef struct {
    char merchant_code[MIPAY_CODE_MAX];
    char settle_kbn[MIPAY_CODE_MAX];
    long long txn_count;
    long long total_amt;
    long long carry_amt;
} local_sum_record;

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

    if (dst_size == 0U || src == NULL) {
        return -1;
    }

    n = strlen(src);
    if (n == 0U || n >= dst_size) {
        return -1;
    }

    memcpy(dst, src, n + 1U);
    return 0;
}

static int parse_ll(const char *s, long long *out)
{
    char *end;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || *end != '\0') {
        return -1;
    }

    *out = v;
    return 0;
}

static int parse_int_yyyymmdd(const char *s, int *out)
{
    long long v;

    if (parse_ll(s, &v) != 0 || v < 19000101LL || v > 29991231LL) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int parse_long_rate(const char *s, long *out)
{
    char *end;
    long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || *end != '\0' || v < 0L || v > 1000000L) {
        return -1;
    }

    *out = v;
    return 0;
}

static int split_csv(char *line, char **field, size_t want)
{
    size_t i = 0U;
    char *p = line;

    while (i < want) {
        field[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    return i == want && strchr(field[want - 1U], ',') == NULL ? 0 : -1;
}

static int parse_detail_line(char *line, local_detail_record *rec)
{
    char *f[6];

    trim_eol(line);
    if (split_csv(line, f, 6U) != 0) {
        return -1;
    }

    if (copy_field(rec->detail_id, sizeof(rec->detail_id), f[0]) != 0 ||
        copy_field(rec->settle_txn_id, sizeof(rec->settle_txn_id), f[1]) != 0 ||
        copy_field(rec->merchant_code, sizeof(rec->merchant_code), f[2]) != 0 ||
        parse_ll(f[3], &rec->txn_amt) != 0 ||
        copy_field(rec->settle_kbn, sizeof(rec->settle_kbn), f[4]) != 0 ||
        copy_field(rec->output_status, sizeof(rec->output_status), f[5]) != 0) {
        return -1;
    }

    return rec->txn_amt >= 0LL ? 0 : -1;
}

static int parse_kbn_line(char *line, local_kbn_record *rec)
{
    char *f[6];
    long long flag;

    trim_eol(line);
    if (split_csv(line, f, 6U) != 0) {
        return -1;
    }

    if (copy_field(rec->settle_kbn, sizeof(rec->settle_kbn), f[0]) != 0 ||
        copy_field(rec->kbn_name, sizeof(rec->kbn_name), f[1]) != 0 ||
        parse_ll(f[2], &flag) != 0 ||
        parse_long_rate(f[3], &rec->fee_rate_ppm) != 0 ||
        parse_int_yyyymmdd(f[4], &rec->valid_from) != 0 ||
        parse_int_yyyymmdd(f[5], &rec->valid_to) != 0) {
        return -1;
    }

    if ((flag != 0LL && flag != 1LL) || rec->valid_from > rec->valid_to) {
        return -1;
    }

    rec->nettable_flag = (int)flag;
    return 0;
}

static int load_kbn(local_kbn_record **out, size_t *out_count)
{
    const char *path = getenv("MIPAY_PCKBNF");
    FILE *fp;
    char line[MIPAY_LINE_MAX];
    local_kbn_record *rows = NULL;
    size_t used = 0U;
    size_t cap = 0U;
    unsigned long lineno = 0UL;

    fp = fopen(path != NULL && *path != '\0' ? path : MIPAY_PATH_KBN, "r");
    if (fp == NULL) {
        fprintf(stderr, "E001:PCKBNFを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        local_kbn_record rec;
        lineno++;

        if (lineno == 1UL && strncmp(line, "SETTLE-KBN,", 11U) == 0) {
            continue;
        }

        if (parse_kbn_line(line, &rec) != 0) {
            fprintf(stderr, "E002:PCKBNF解析失敗:%lu\n", lineno);
            free(rows);
            fclose(fp);
            return -1;
        }

        if (used == cap) {
            size_t next = cap == 0U ? 16U : cap * 2U;
            local_kbn_record *tmp = realloc(rows, next * sizeof(*rows));
            if (tmp == NULL) {
                fprintf(stderr, "E003:PCKBNF領域不足\n");
                free(rows);
                fclose(fp);
                return -1;
            }
            rows = tmp;
            cap = next;
        }

        rows[used++] = rec;
    }

    if (ferror(fp) || fclose(fp) != 0) {
        fprintf(stderr, "E004:PCKBNF読込失敗\n");
        free(rows);
        return -1;
    }

    *out = rows;
    *out_count = used;
    return 0;
}

static const local_kbn_record *find_kbn(const local_kbn_record *rows, size_t count, const char *settle_kbn, int yyyymmdd)
{
    size_t i;

    for (i = 0U; i < count; i++) {
        if (strcmp(rows[i].settle_kbn, settle_kbn) == 0 &&
            rows[i].valid_from <= yyyymmdd &&
            yyyymmdd <= rows[i].valid_to) {
            return &rows[i];
        }
    }

    return NULL;
}

static int add_checked(long long *dst, long long v)
{
    if (v > 0LL && *dst > LLONG_MAX - v) {
        return -1;
    }
    if (v < 0LL && *dst < LLONG_MIN - v) {
        return -1;
    }

    *dst += v;
    return 0;
}

static int same_group(const local_sum_record *sum, const local_detail_record *rec)
{
    return strcmp(sum->merchant_code, rec->merchant_code) == 0 &&
           strcmp(sum->settle_kbn, rec->settle_kbn) == 0;
}

static int write_sum(FILE *fp, const local_sum_record *sum)
{
    if (sum->txn_count == 0LL) {
        return 0;
    }

    if (fprintf(fp, "%s,%s,%s,%lld,%lld,%lld\n",
                sum->merchant_code,
                MIPAY_SETTLE_DATE,
                sum->settle_kbn,
                sum->txn_count,
                sum->total_amt,
                sum->carry_amt) < 0) {
        return -1;
    }

    return 0;
}

static int normal_decision_code(void)
{
#if defined(MIPAY_DECISION_CONTINUE)
    return MIPAY_DECISION_CONTINUE;
#elif defined(MIPAY_DECISION_OK)
    return MIPAY_DECISION_OK;
#elif defined(MIPAY_RC_OK)
    return MIPAY_RC_OK;
#else
    return 0;
#endif
}

int main(void)
{
    const char *detail_path = getenv("MIPAY_PCDTLF");
    const char *sum_path = getenv("MIPAY_PCSUMF");
    FILE *detail_fp;
    FILE *sum_fp;
    char line[MIPAY_LINE_MAX];
    local_kbn_record *kbn_rows = NULL;
    size_t kbn_count = 0U;
    local_sum_record current;
    int have_group = 0;
    unsigned long lineno = 0UL;
    int settle_date;

    if (parse_int_yyyymmdd(MIPAY_SETTLE_DATE, &settle_date) != 0) {
        fprintf(stderr, "E005:精算日不正\n");
        return 12;
    }

    if (load_kbn(&kbn_rows, &kbn_count) != 0) {
        return 12;
    }

    detail_fp = fopen(detail_path != NULL && *detail_path != '\0' ? detail_path : MIPAY_PATH_DETAIL, "r");
    if (detail_fp == NULL) {
        fprintf(stderr, "E006:PCDTLFを開けません\n");
        free(kbn_rows);
        return 12;
    }

    sum_fp = fopen(sum_path != NULL && *sum_path != '\0' ? sum_path : MIPAY_PATH_SUM, "w");
    if (sum_fp == NULL) {
        fprintf(stderr, "E007:PCSUMFを開けません\n");
        fclose(detail_fp);
        free(kbn_rows);
        return 12;
    }

    memset(&current, 0, sizeof(current));

    while (fgets(line, sizeof(line), detail_fp) != NULL) {
        local_detail_record rec;
        const local_kbn_record *kbn;
        long long fee_base;

        lineno++;

        if (lineno == 1UL && strncmp(line, "DETAIL-ID,", 10U) == 0) {
            continue;
        }

        if (parse_detail_line(line, &rec) != 0) {
            fprintf(stderr, "E008:PCDTLF解析失敗:%lu\n", lineno);
            fclose(sum_fp);
            fclose(detail_fp);
            free(kbn_rows);
            return 12;
        }

        if (strcmp(rec.output_status, "0") != 0 && strcmp(rec.output_status, "OK") != 0) {
            continue;
        }

        kbn = find_kbn(kbn_rows, kbn_count, rec.settle_kbn, settle_date);
        if (kbn == NULL) {
            fprintf(stderr, "E009:精算区分未登録:%s\n", rec.settle_kbn);
            fclose(sum_fp);
            fclose(detail_fp);
            free(kbn_rows);
            return 12;
        }

        if (!have_group || !same_group(&current, &rec)) {
            if (write_sum(sum_fp, &current) != 0) {
                fprintf(stderr, "E010:PCSUMF書込失敗\n");
                fclose(sum_fp);
                fclose(detail_fp);
                free(kbn_rows);
                return 12;
            }

            memset(&current, 0, sizeof(current));
            if (copy_field(current.merchant_code, sizeof(current.merchant_code), rec.merchant_code) != 0 ||
                copy_field(current.settle_kbn, sizeof(current.settle_kbn), rec.settle_kbn) != 0) {
                fprintf(stderr, "E011:集計キー不正\n");
                fclose(sum_fp);
                fclose(detail_fp);
                free(kbn_rows);
                return 12;
            }
            have_group = 1;
        }

        fee_base = kbn->nettable_flag != 0 ? rec.txn_amt : 0LL;

        if (add_checked(&current.txn_count, 1LL) != 0 ||
            add_checked(&current.total_amt, rec.txn_amt) != 0 ||
            add_checked(&current.carry_amt, fee_base) != 0) {
            fprintf(stderr, "E012:集計桁あふれ:%lu\n", lineno);
            fclose(sum_fp);
            fclose(detail_fp);
            free(kbn_rows);
            return 12;
        }
    }

    if (ferror(detail_fp)) {
        fprintf(stderr, "E013:PCDTLF読込失敗\n");
        fclose(sum_fp);
        fclose(detail_fp);
        free(kbn_rows);
        return 12;
    }

    if (write_sum(sum_fp, &current) != 0) {
        fprintf(stderr, "E014:PCSUMF書込失敗\n");
        fclose(sum_fp);
        fclose(detail_fp);
        free(kbn_rows);
        return 12;
    }

    if (fclose(sum_fp) != 0) {
        fprintf(stderr, "E015:PCSUMF完了失敗\n");
        fclose(detail_fp);
        free(kbn_rows);
        return 12;
    }

    if (fclose(detail_fp) != 0) {
        fprintf(stderr, "E016:PCDTLF完了失敗\n");
        free(kbn_rows);
        return 12;
    }

    free(kbn_rows);
    return normal_decision_code();
}
