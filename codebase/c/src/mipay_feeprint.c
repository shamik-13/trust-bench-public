/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240610  精算基盤  初版作成
 * 1.01  20241014  精算基盤  CSV検証と金額突合を追加
 * 1.02  20250324  精算基盤  手数料条件説明の出力制御を追加
 */

#include "mipay_settle.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MIPAY_DECISION_NORMAL
#define MIPAY_DECISION_NORMAL 0
#endif

#define PSSETF_PATH "PSSETF.csv"
#define PSDTLF_PATH "PSDTLF.csv"
#define PSFEEF_PATH "PSFEEF.csv"
#define PSRPTF_PATH "PSRPTF.csv"

#define MAX_LINE_LEN 1024
#define MAX_SETTLES 4096
#define MAX_DETAILS 32768
#define MAX_FEES 4096
#define MAX_MERCHANTS 4096

#define REPORT_KBN_FEE "FEE"
#define CREATE_STATUS_OK "01"
#define CREATE_STATUS_MISMATCH "02"
#define CREATE_STATUS_NOFEE "03"

typedef struct {
    char settle_id[32];
    char merchant_code[32];
    long long net_amt;
    long long charge_amt;
    long long payout_amt;
    char settle_dt[16];
} local_pssetf_record;

typedef struct {
    char detail_id[32];
    char settle_id[32];
    char merchant_code[32];
    char txn_id[40];
    long long txn_amt;
    long long charge_amt;
    char line_kbn[8];
} local_psdtlf_record;

typedef struct {
    char fee_plan_id[32];
    char merchant_code[32];
    char rate_kbn[8];
    long long rate_value;
    long long min_fee_amt;
    long long max_fee_amt;
    char apply_dt[16];
} local_psfeef_record;

typedef struct {
    char merchant_code[32];
    char period_from[16];
    char period_to[16];
    long long settle_charge_amt;
    long long detail_charge_amt;
    int settle_count;
    int detail_count;
    int fee_plan_count;
} merchant_summary;

static void chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t n;

    if (dst_len == 0 || src == NULL) {
        return -1;
    }
    n = strlen(src);
    if (n >= dst_len) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_ll(const char *s, long long *out)
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

static int next_field(char **cursor, char *dst, size_t dst_len)
{
    char *p;
    char *start;
    size_t n;

    if (cursor == NULL || *cursor == NULL || dst_len == 0) {
        return -1;
    }

    p = *cursor;
    start = p;
    while (*p != '\0' && *p != ',') {
        p++;
    }

    n = (size_t)(p - start);
    if (n >= dst_len) {
        return -1;
    }
    memcpy(dst, start, n);
    dst[n] = '\0';

    *cursor = (*p == ',') ? p + 1 : p;
    return 0;
}

static int add_ll(long long a, long long b, long long *out)
{
    if ((b > 0 && a > LLONG_MAX - b) || (b < 0 && a < LLONG_MIN - b)) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int parse_pssetf_line(char *line, local_pssetf_record *rec)
{
    char *p = line;
    char buf[64];

    if (next_field(&p, rec->settle_id, sizeof(rec->settle_id)) != 0 ||
        next_field(&p, rec->merchant_code, sizeof(rec->merchant_code)) != 0 ||
        next_field(&p, buf, sizeof(buf)) != 0 ||
        parse_ll(buf, &rec->net_amt) != 0 ||
        next_field(&p, buf, sizeof(buf)) != 0 ||
        parse_ll(buf, &rec->charge_amt) != 0 ||
        next_field(&p, buf, sizeof(buf)) != 0 ||
        parse_ll(buf, &rec->payout_amt) != 0 ||
        next_field(&p, rec->settle_dt, sizeof(rec->settle_dt)) != 0) {
        return -1;
    }
    return 0;
}

static int parse_psdtlf_line(char *line, local_psdtlf_record *rec)
{
    char *p = line;
    char buf[64];

    if (next_field(&p, rec->detail_id, sizeof(rec->detail_id)) != 0 ||
        next_field(&p, rec->settle_id, sizeof(rec->settle_id)) != 0 ||
        next_field(&p, rec->merchant_code, sizeof(rec->merchant_code)) != 0 ||
        next_field(&p, rec->txn_id, sizeof(rec->txn_id)) != 0 ||
        next_field(&p, buf, sizeof(buf)) != 0 ||
        parse_ll(buf, &rec->txn_amt) != 0 ||
        next_field(&p, buf, sizeof(buf)) != 0 ||
        parse_ll(buf, &rec->charge_amt) != 0 ||
        next_field(&p, rec->line_kbn, sizeof(rec->line_kbn)) != 0) {
        return -1;
    }
    return 0;
}

static int parse_psfeef_line(char *line, local_psfeef_record *rec)
{
    char *p = line;
    char buf[64];

    if (next_field(&p, rec->fee_plan_id, sizeof(rec->fee_plan_id)) != 0 ||
        next_field(&p, rec->merchant_code, sizeof(rec->merchant_code)) != 0 ||
        next_field(&p, rec->rate_kbn, sizeof(rec->rate_kbn)) != 0 ||
        next_field(&p, buf, sizeof(buf)) != 0 ||
        parse_ll(buf, &rec->rate_value) != 0 ||
        next_field(&p, buf, sizeof(buf)) != 0 ||
        parse_ll(buf, &rec->min_fee_amt) != 0 ||
        next_field(&p, buf, sizeof(buf)) != 0 ||
        parse_ll(buf, &rec->max_fee_amt) != 0 ||
        next_field(&p, rec->apply_dt, sizeof(rec->apply_dt)) != 0) {
        return -1;
    }
    return 0;
}

static int read_pssetf(local_pssetf_record *rows, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MAX_LINE_LEN];

    fp = fopen(PSSETF_PATH, "r");
    if (fp == NULL) {
        fprintf(stderr, "PSSETFオープン失敗\n");
        return -1;
    }

    *count = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (*count >= cap || parse_pssetf_line(line, &rows[*count]) != 0) {
            fclose(fp);
            fprintf(stderr, "PSSETF解析失敗\n");
            return -1;
        }
        (*count)++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "PSSETF読込失敗\n");
        return -1;
    }
    fclose(fp);
    return 0;
}

static int read_psdtlf(local_psdtlf_record *rows, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MAX_LINE_LEN];

    fp = fopen(PSDTLF_PATH, "r");
    if (fp == NULL) {
        fprintf(stderr, "PSDTLFオープン失敗\n");
        return -1;
    }

    *count = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (*count >= cap || parse_psdtlf_line(line, &rows[*count]) != 0) {
            fclose(fp);
            fprintf(stderr, "PSDTLF解析失敗\n");
            return -1;
        }
        (*count)++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "PSDTLF読込失敗\n");
        return -1;
    }
    fclose(fp);
    return 0;
}

static int read_psfeef(local_psfeef_record *rows, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MAX_LINE_LEN];

    fp = fopen(PSFEEF_PATH, "r");
    if (fp == NULL) {
        fprintf(stderr, "PSFEEFオープン失敗\n");
        return -1;
    }

    *count = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (*count >= cap || parse_psfeef_line(line, &rows[*count]) != 0) {
            fclose(fp);
            fprintf(stderr, "PSFEEF解析失敗\n");
            return -1;
        }
        (*count)++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "PSFEEF読込失敗\n");
        return -1;
    }
    fclose(fp);
    return 0;
}

static merchant_summary *find_or_add_summary(merchant_summary *rows, size_t *count, const char *merchant_code)
{
    size_t i;

    for (i = 0; i < *count; i++) {
        if (strcmp(rows[i].merchant_code, merchant_code) == 0) {
            return &rows[i];
        }
    }

    if (*count >= MAX_MERCHANTS) {
        return NULL;
    }

    memset(&rows[*count], 0, sizeof(rows[*count]));
    if (copy_field(rows[*count].merchant_code, sizeof(rows[*count].merchant_code), merchant_code) != 0 ||
        copy_field(rows[*count].period_from, sizeof(rows[*count].period_from), "99999999") != 0 ||
        copy_field(rows[*count].period_to, sizeof(rows[*count].period_to), "00000000") != 0) {
        return NULL;
    }

    (*count)++;
    return &rows[*count - 1];
}

static int same_settle_merchant(const local_psdtlf_record *d, const local_pssetf_record *s)
{
    return strcmp(d->settle_id, s->settle_id) == 0 &&
           strcmp(d->merchant_code, s->merchant_code) == 0;
}

static int count_fee_plans(const local_psfeef_record *fees, size_t fee_count, const char *merchant_code)
{
    size_t i;
    int n = 0;

    for (i = 0; i < fee_count; i++) {
        if (strcmp(fees[i].merchant_code, merchant_code) == 0) {
            if (n == INT_MAX) {
                return -1;
            }
            n++;
        }
    }
    return n;
}

static int build_summaries(const local_pssetf_record *settles, size_t settle_count,
                           const local_psdtlf_record *details, size_t detail_count,
                           const local_psfeef_record *fees, size_t fee_count,
                           merchant_summary *summaries, size_t *summary_count)
{
    size_t i;
    size_t j;

    *summary_count = 0;

    for (i = 0; i < settle_count; i++) {
        merchant_summary *m = find_or_add_summary(summaries, summary_count, settles[i].merchant_code);
        if (m == NULL) {
            fprintf(stderr, "加盟店集計領域不足\n");
            return -1;
        }

        if (add_ll(m->settle_charge_amt, settles[i].charge_amt, &m->settle_charge_amt) != 0) {
            fprintf(stderr, "精算手数料金額あふれ\n");
            return -1;
        }
        m->settle_count++;

        if (strcmp(settles[i].settle_dt, m->period_from) < 0 &&
            copy_field(m->period_from, sizeof(m->period_from), settles[i].settle_dt) != 0) {
            return -1;
        }
        if (strcmp(settles[i].settle_dt, m->period_to) > 0 &&
            copy_field(m->period_to, sizeof(m->period_to), settles[i].settle_dt) != 0) {
            return -1;
        }

        for (j = 0; j < detail_count; j++) {
            if (same_settle_merchant(&details[j], &settles[i]) && strcmp(details[j].line_kbn, "FEE") == 0) {
                if (add_ll(m->detail_charge_amt, details[j].charge_amt, &m->detail_charge_amt) != 0) {
                    fprintf(stderr, "明細手数料金額あふれ\n");
                    return -1;
                }
                m->detail_count++;
            }
        }
    }

    for (i = 0; i < *summary_count; i++) {
        int n = count_fee_plans(fees, fee_count, summaries[i].merchant_code);
        if (n < 0) {
            fprintf(stderr, "手数料条件件数あふれ\n");
            return -1;
        }
        summaries[i].fee_plan_count = n;
    }

    return 0;
}

static const char *create_status_of(const merchant_summary *m)
{
    if (m->fee_plan_count == 0) {
        return CREATE_STATUS_NOFEE;
    }
    if (m->settle_charge_amt != m->detail_charge_amt) {
        return CREATE_STATUS_MISMATCH;
    }
    return CREATE_STATUS_OK;
}

static int write_psrptf(const merchant_summary *summaries, size_t summary_count)
{
    FILE *fp;
    size_t i;

    fp = fopen(PSRPTF_PATH, "w");
    if (fp == NULL) {
        fprintf(stderr, "PSRPTFオープン失敗\n");
        return -1;
    }

    for (i = 0; i < summary_count; i++) {
        char report_id[64];
        char output_path[160];
        int n;

        n = snprintf(report_id, sizeof(report_id), "RPT%08lu", (unsigned long)(i + 1));
        if (n < 0 || (size_t)n >= sizeof(report_id)) {
            fclose(fp);
            fprintf(stderr, "帳票ID編集失敗\n");
            return -1;
        }

        n = snprintf(output_path, sizeof(output_path), "out/%s_%s_%s.fee",
                     summaries[i].merchant_code,
                     summaries[i].period_from,
                     summaries[i].period_to);
        if (n < 0 || (size_t)n >= sizeof(output_path)) {
            fclose(fp);
            fprintf(stderr, "出力パス編集失敗\n");
            return -1;
        }

        if (fprintf(fp, "%s,%s,%s,%s,%s,%s,%s\n",
                    report_id,
                    summaries[i].merchant_code,
                    REPORT_KBN_FEE,
                    summaries[i].period_from,
                    summaries[i].period_to,
                    output_path,
                    create_status_of(&summaries[i])) < 0) {
            fclose(fp);
            fprintf(stderr, "PSRPTF書込失敗\n");
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "PSRPTFクローズ失敗\n");
        return -1;
    }
    return 0;
}

int main(void)
{
    local_pssetf_record settles[MAX_SETTLES];
    local_psdtlf_record details[MAX_DETAILS];
    local_psfeef_record fees[MAX_FEES];
    merchant_summary summaries[MAX_MERCHANTS];
    size_t settle_count;
    size_t detail_count;
    size_t fee_count;
    size_t summary_count;

    if (read_pssetf(settles, MAX_SETTLES, &settle_count) != 0) {
        return 12;
    }
    if (read_psdtlf(details, MAX_DETAILS, &detail_count) != 0) {
        return 12;
    }
    if (read_psfeef(fees, MAX_FEES, &fee_count) != 0) {
        return 12;
    }
    if (build_summaries(settles, settle_count, details, detail_count,
                        fees, fee_count, summaries, &summary_count) != 0) {
        return 16;
    }
    if (write_psrptf(summaries, summary_count) != 0) {
        return 16;
    }

    return MIPAY_DECISION_NORMAL;
}
