/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240301  精算連携  初版作成
 * 1.01  20240712  精算連携  繰越突合と欠落明細警告を追加
 * 1.02  20241105  精算連携  CSV境界検査と金額あふれ検査を強化
 */

#include "mipay_trace.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIPAY_RC_OK 0
#define MIPAY_RC_IO 12
#define MIPAY_RC_PARSE 16
#define MIPAY_RC_LIMIT 20

#define MIPAY_MAX_LINE 1024
#define MIPAY_MAX_SUM 4096
#define MIPAY_MAX_DTL 65536
#define MIPAY_MAX_CAR 8192
#define MIPAY_MAX_MERCHANT 64
#define MIPAY_MAX_DATE 16
#define MIPAY_MAX_KBN 8
#define MIPAY_MAX_ID 64
#define MIPAY_MAX_REASON 64
#define MIPAY_MAX_STATUS 16

#define MIPAY_FEE_BPS 28
#define MIPAY_WARN_MISSING "W01"
#define MIPAY_WARN_MISMATCH "W02"
#define MIPAY_WARN_CARRY "W03"
#define MIPAY_STATUS_NORMAL "00"

typedef struct {
    char merchant_code[MIPAY_MAX_MERCHANT];
    char settle_date[MIPAY_MAX_DATE];
    char settle_kbn[MIPAY_MAX_KBN];
    long long txn_count;
    long long total_amt;
    long long carry_amt;
} pcsumf_record;

typedef struct {
    char detail_id[MIPAY_MAX_ID];
    char settle_txn_id[MIPAY_MAX_ID];
    char merchant_code[MIPAY_MAX_MERCHANT];
    long long txn_amt;
    char settle_kbn[MIPAY_MAX_KBN];
    char output_status[MIPAY_MAX_STATUS];
    int matched;
} pcdtlf_record;

typedef struct {
    char carry_id[MIPAY_MAX_ID];
    char merchant_code[MIPAY_MAX_MERCHANT];
    char settle_kbn[MIPAY_MAX_KBN];
    long long carry_amt;
    char carry_reason[MIPAY_MAX_REASON];
    char next_settle_date[MIPAY_MAX_DATE];
} pccarf_record;

typedef struct {
    char report_id[MIPAY_MAX_ID];
    char merchant_code[MIPAY_MAX_MERCHANT];
    char settle_date[MIPAY_MAX_DATE];
    long long gross_amt;
    long long fee_amt;
    long long net_amt;
    char report_status[MIPAY_MAX_STATUS];
} pjrepf_record;

static pcsumf_record g_sum[MIPAY_MAX_SUM];
static pcdtlf_record g_dtl[MIPAY_MAX_DTL];
static pccarf_record g_car[MIPAY_MAX_CAR];

static size_t g_sum_count;
static size_t g_dtl_count;
static size_t g_car_count;

static void strip_eol(char *s)
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

static int parse_i64(const char *s, long long *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0' || out == NULL) {
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

static int add_i64(long long a, long long b, long long *out)
{
    if ((b > 0 && a > LLONG_MAX - b) || (b < 0 && a < LLONG_MIN - b)) {
        return -1;
    }

    *out = a + b;
    return 0;
}

static int mul_i64(long long a, long long b, long long *out)
{
    if (a != 0 && b != 0) {
        if (a == -1 && b == LLONG_MIN) {
            return -1;
        }
        if (b == -1 && a == LLONG_MIN) {
            return -1;
        }
        if (a > 0) {
            if ((b > 0 && a > LLONG_MAX / b) ||
                (b < 0 && b < LLONG_MIN / a)) {
                return -1;
            }
        } else {
            if ((b > 0 && a < LLONG_MIN / b) ||
                (b < 0 && a < LLONG_MAX / b)) {
                return -1;
            }
        }
    }

    *out = a * b;
    return 0;
}

static int split_csv(char *line, char **field, size_t max_field, size_t *count)
{
    size_t n = 0U;
    char *p = line;

    if (line == NULL || field == NULL || count == NULL) {
        return -1;
    }

    for (;;) {
        if (n >= max_field) {
            return -1;
        }

        field[n++] = p;

        while (*p != '\0' && *p != ',') {
            ++p;
        }

        if (*p == '\0') {
            break;
        }

        *p = '\0';
        ++p;
    }

    *count = n;
    return 0;
}

static int read_pcsumf(const char *path)
{
    FILE *fp;
    char line[MIPAY_MAX_LINE];
    unsigned long row = 0UL;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "EIO:PCSUMF:%s\n", path);
        return MIPAY_RC_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];
        size_t n = 0U;
        pcsumf_record *r;

        ++row;
        strip_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (row == 1UL && strncmp(line, "MERCHANT-CODE,", 14U) == 0) {
            continue;
        }
        if (g_sum_count >= MIPAY_MAX_SUM) {
            fclose(fp);
            fprintf(stderr, "ELIMIT:PCSUMF\n");
            return MIPAY_RC_LIMIT;
        }
        if (split_csv(line, f, 6U, &n) != 0 || n != 6U) {
            fclose(fp);
            fprintf(stderr, "EPARSE:PCSUMF:%lu\n", row);
            return MIPAY_RC_PARSE;
        }

        r = &g_sum[g_sum_count];
        if (copy_field(r->merchant_code, sizeof(r->merchant_code), f[0]) != 0 ||
            copy_field(r->settle_date, sizeof(r->settle_date), f[1]) != 0 ||
            copy_field(r->settle_kbn, sizeof(r->settle_kbn), f[2]) != 0 ||
            parse_i64(f[3], &r->txn_count) != 0 ||
            parse_i64(f[4], &r->total_amt) != 0 ||
            parse_i64(f[5], &r->carry_amt) != 0 ||
            r->txn_count < 0) {
            fclose(fp);
            fprintf(stderr, "EPARSE:PCSUMF:%lu\n", row);
            return MIPAY_RC_PARSE;
        }

        ++g_sum_count;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "EIO:PCSUMF:READ\n");
        return MIPAY_RC_IO;
    }

    fclose(fp);
    return MIPAY_RC_OK;
}

static int read_pcdtlf(const char *path)
{
    FILE *fp;
    char line[MIPAY_MAX_LINE];
    unsigned long row = 0UL;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "EIO:PCDTLF:%s\n", path);
        return MIPAY_RC_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];
        size_t n = 0U;
        pcdtlf_record *r;

        ++row;
        strip_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (row == 1UL && strncmp(line, "DETAIL-ID,", 10U) == 0) {
            continue;
        }
        if (g_dtl_count >= MIPAY_MAX_DTL) {
            fclose(fp);
            fprintf(stderr, "ELIMIT:PCDTLF\n");
            return MIPAY_RC_LIMIT;
        }
        if (split_csv(line, f, 6U, &n) != 0 || n != 6U) {
            fclose(fp);
            fprintf(stderr, "EPARSE:PCDTLF:%lu\n", row);
            return MIPAY_RC_PARSE;
        }

        r = &g_dtl[g_dtl_count];
        if (copy_field(r->detail_id, sizeof(r->detail_id), f[0]) != 0 ||
            copy_field(r->settle_txn_id, sizeof(r->settle_txn_id), f[1]) != 0 ||
            copy_field(r->merchant_code, sizeof(r->merchant_code), f[2]) != 0 ||
            parse_i64(f[3], &r->txn_amt) != 0 ||
            copy_field(r->settle_kbn, sizeof(r->settle_kbn), f[4]) != 0 ||
            copy_field(r->output_status, sizeof(r->output_status), f[5]) != 0) {
            fclose(fp);
            fprintf(stderr, "EPARSE:PCDTLF:%lu\n", row);
            return MIPAY_RC_PARSE;
        }

        r->matched = 0;
        ++g_dtl_count;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "EIO:PCDTLF:READ\n");
        return MIPAY_RC_IO;
    }

    fclose(fp);
    return MIPAY_RC_OK;
}

static int read_pccarf(const char *path)
{
    FILE *fp;
    char line[MIPAY_MAX_LINE];
    unsigned long row = 0UL;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "EIO:PCCARF:%s\n", path);
        return MIPAY_RC_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];
        size_t n = 0U;
        pccarf_record *r;

        ++row;
        strip_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (row == 1UL && strncmp(line, "CARRY-ID,", 9U) == 0) {
            continue;
        }
        if (g_car_count >= MIPAY_MAX_CAR) {
            fclose(fp);
            fprintf(stderr, "ELIMIT:PCCARF\n");
            return MIPAY_RC_LIMIT;
        }
        if (split_csv(line, f, 6U, &n) != 0 || n != 6U) {
            fclose(fp);
            fprintf(stderr, "EPARSE:PCCARF:%lu\n", row);
            return MIPAY_RC_PARSE;
        }

        r = &g_car[g_car_count];
        if (copy_field(r->carry_id, sizeof(r->carry_id), f[0]) != 0 ||
            copy_field(r->merchant_code, sizeof(r->merchant_code), f[1]) != 0 ||
            copy_field(r->settle_kbn, sizeof(r->settle_kbn), f[2]) != 0 ||
            parse_i64(f[3], &r->carry_amt) != 0 ||
            copy_field(r->carry_reason, sizeof(r->carry_reason), f[4]) != 0 ||
            copy_field(r->next_settle_date, sizeof(r->next_settle_date), f[5]) != 0) {
            fclose(fp);
            fprintf(stderr, "EPARSE:PCCARF:%lu\n", row);
            return MIPAY_RC_PARSE;
        }

        ++g_car_count;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "EIO:PCCARF:READ\n");
        return MIPAY_RC_IO;
    }

    fclose(fp);
    return MIPAY_RC_OK;
}

static int same_key(const char *merchant_a, const char *kbn_a,
                    const char *merchant_b, const char *kbn_b)
{
    return strcmp(merchant_a, merchant_b) == 0 && strcmp(kbn_a, kbn_b) == 0;
}

static int collect_detail(const pcsumf_record *sum, long long *amt, long long *cnt)
{
    size_t i;
    long long total = 0;
    long long count = 0;

    for (i = 0U; i < g_dtl_count; ++i) {
        pcdtlf_record *d = &g_dtl[i];

        if (same_key(sum->merchant_code, sum->settle_kbn,
                     d->merchant_code, d->settle_kbn) &&
            strcmp(d->output_status, MIPAY_STATUS_NORMAL) == 0) {
            if (add_i64(total, d->txn_amt, &total) != 0 ||
                add_i64(count, 1, &count) != 0) {
                return -1;
            }
            d->matched = 1;
        }
    }

    *amt = total;
    *cnt = count;
    return 0;
}

static int collect_carry(const pcsumf_record *sum, long long *amt)
{
    size_t i;
    long long total = 0;

    for (i = 0U; i < g_car_count; ++i) {
        const pccarf_record *c = &g_car[i];

        if (same_key(sum->merchant_code, sum->settle_kbn,
                     c->merchant_code, c->settle_kbn)) {
            if (add_i64(total, c->carry_amt, &total) != 0) {
                return -1;
            }
        }
    }

    *amt = total;
    return 0;
}

static void set_report_status(char *dst, size_t dst_size,
                              long long sum_count, long long detail_count,
                              long long sum_total, long long detail_total,
                              long long sum_carry, long long carry_total)
{
    const char *status = MIPAY_STATUS_NORMAL;

    if (sum_count != detail_count) {
        status = MIPAY_WARN_MISSING;
    } else if (sum_total != detail_total) {
        status = MIPAY_WARN_MISMATCH;
    } else if (sum_carry != carry_total) {
        status = MIPAY_WARN_CARRY;
    }

    (void)snprintf(dst, dst_size, "%s", status);
}

static int build_report(const pcsumf_record *sum, size_t index, pjrepf_record *out)
{
    long long detail_amt;
    long long detail_count;
    long long carry_amt;
    long long gross_amt;
    long long fee_base;
    long long fee_amt;
    long long net_before_carry;
    long long net_amt;

    if (collect_detail(sum, &detail_amt, &detail_count) != 0 ||
        collect_carry(sum, &carry_amt) != 0) {
        return -1;
    }

    if (add_i64(detail_amt, carry_amt, &gross_amt) != 0 ||
        mul_i64(gross_amt, MIPAY_FEE_BPS, &fee_base) != 0) {
        return -1;
    }

    fee_amt = fee_base / 10000;
    if (gross_amt > 0 && fee_amt == 0) {
        fee_amt = 1;
    }

    if (add_i64(gross_amt, -fee_amt, &net_before_carry) != 0 ||
        add_i64(net_before_carry, 0, &net_amt) != 0) {
        return -1;
    }

    if (snprintf(out->report_id, sizeof(out->report_id), "R%08zu", index + 1U) < 0) {
        return -1;
    }

    if (copy_field(out->merchant_code, sizeof(out->merchant_code), sum->merchant_code) != 0 ||
        copy_field(out->settle_date, sizeof(out->settle_date), sum->settle_date) != 0) {
        return -1;
    }

    out->gross_amt = gross_amt;
    out->fee_amt = fee_amt;
    out->net_amt = net_amt;
    set_report_status(out->report_status, sizeof(out->report_status),
                      sum->txn_count, detail_count, sum->total_amt,
                      detail_amt, sum->carry_amt, carry_amt);

    return 0;
}

static int write_report_header(FILE *fp)
{
    if (fputs("REPORT-ID,MERCHANT-CODE,SETTLE-DATE,GROSS-AMT,FEE-AMT,NET-AMT,REPORT-STATUS\n", fp) == EOF) {
        return -1;
    }
    return 0;
}

static int write_report_record(FILE *fp, const pjrepf_record *r)
{
    if (fprintf(fp, "%s,%s,%s,%lld,%lld,%lld,%s\n",
                r->report_id,
                r->merchant_code,
                r->settle_date,
                r->gross_amt,
                r->fee_amt,
                r->net_amt,
                r->report_status) < 0) {
        return -1;
    }

    return 0;
}

static int write_unmatched_warning(FILE *fp)
{
    size_t i;
    int warned = 0;

    for (i = 0U; i < g_dtl_count; ++i) {
        const pcdtlf_record *d = &g_dtl[i];

        if (!d->matched && strcmp(d->output_status, MIPAY_STATUS_NORMAL) == 0) {
            if (fprintf(fp, "WARN,%s,%s,%s,%lld,0,0,%s\n",
                        d->detail_id,
                        d->merchant_code,
                        "00000000",
                        d->txn_amt,
                        MIPAY_WARN_MISSING) < 0) {
                return -1;
            }
            warned = 1;
        }
    }

    return warned;
}

static int process_reports(const char *path)
{
    FILE *fp;
    size_t i;

    fp = fopen(path, "w");
    if (fp == NULL) {
        fprintf(stderr, "EIO:PJREPF:%s\n", path);
        return MIPAY_RC_IO;
    }

    if (write_report_header(fp) != 0) {
        fclose(fp);
        fprintf(stderr, "EIO:PJREPF:HEADER\n");
        return MIPAY_RC_IO;
    }

    for (i = 0U; i < g_sum_count; ++i) {
        pjrepf_record report;

        if (build_report(&g_sum[i], i, &report) != 0) {
            fclose(fp);
            fprintf(stderr, "EOVER:PJREPF:%zu\n", i + 1U);
            return MIPAY_RC_PARSE;
        }

        if (write_report_record(fp, &report) != 0) {
            fclose(fp);
            fprintf(stderr, "EIO:PJREPF:WRITE\n");
            return MIPAY_RC_IO;
        }
    }

    if (write_unmatched_warning(fp) < 0) {
        fclose(fp);
        fprintf(stderr, "EIO:PJREPF:WARN\n");
        return MIPAY_RC_IO;
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "EIO:PJREPF:CLOSE\n");
        return MIPAY_RC_IO;
    }

    return MIPAY_RC_OK;
}

int main(void)
{
    int rc;

    rc = read_pcsumf("PCSUMF.csv");
    if (rc != MIPAY_RC_OK) {
        return rc;
    }

    rc = read_pcdtlf("PCDTLF.csv");
    if (rc != MIPAY_RC_OK) {
        return rc;
    }

    rc = read_pccarf("PCCARF.csv");
    if (rc != MIPAY_RC_OK) {
        return rc;
    }

    rc = process_reports("PJREPF.csv");
    if (rc != MIPAY_RC_OK) {
        return rc;
    }

    return MIPAY_RC_OK;
}
