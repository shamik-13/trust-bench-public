/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240617  精算基盤  初版作成
 * 1.01  20241125  精算基盤  再実行制御と件数検証を追加
 * 1.02  20250410  精算基盤  加盟店状態確認と精算集計を強化
 */
#include "mipay_settle.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef MIPAY_DECISION_OK
#define MIPAY_DECISION_OK 0
#endif

#ifndef MIPAY_DECISION_REJECT
#define MIPAY_DECISION_REJECT 8
#endif

#define MI_CONF_IN_PATH   "PSCONF.csv"
#define MI_CONF_OUT_PATH  "PSCONF.out.csv"
#define MI_TXN_PATH       "PSTXNF.csv"
#define MI_MER_PATH       "PSMERF.csv"

#define MI_LINE_MAX       1024
#define MI_KEY_MAX        128
#define MI_VALUE_MAX      256
#define MI_DATE_LEN       8
#define MI_STATUS_LEN     2
#define MI_BANK_MAX       64
#define MI_MER_CODE_MAX   32
#define MI_MER_NAME_MAX   128
#define MI_TXN_ID_MAX     64
#define MI_STEP_COUNT     6
#define MI_MAX_MERCHANTS  4096

typedef struct {
    char key[MI_KEY_MAX];
    char value[MI_VALUE_MAX];
    char apply_dt[MI_DATE_LEN + 1];
    char expire_dt[MI_DATE_LEN + 1];
    char updated_at[32];
} MiConfRec;

typedef struct {
    char code[MI_MER_CODE_MAX];
    char name[MI_MER_NAME_MAX];
    char status[MI_STATUS_LEN + 1];
    char bank_acct[MI_BANK_MAX];
} MiMerchantRec;

typedef struct {
    char txn_id[MI_TXN_ID_MAX];
    char merchant_code[MI_MER_CODE_MAX];
    char txn_kbn;
    int64_t amount;
    char txn_dt[MI_DATE_LEN + 1];
} MiTxnRec;

typedef struct {
    char code[MI_MER_CODE_MAX];
    int64_t net;
    int64_t charge;
    int64_t payout;
    uint64_t capture_count;
    uint64_t refund_count;
} MiMerchantAgg;

typedef struct {
    const char *step;
    uint64_t input_count;
    int rc;
} MiStepLog;

static void mi_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static char *mi_trim(char *s)
{
    char *e;

    while (*s != '\0' && isspace((unsigned char)*s) != 0) {
        ++s;
    }
    e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1]) != 0) {
        *--e = '\0';
    }
    return s;
}

static int mi_copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (dstsz == 0U || n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1U);
    return 0;
}

static int mi_split_csv(char *line, char **field, size_t max_field, size_t *out_count)
{
    size_t count = 0U;
    char *p = line;

    while (*p != '\0') {
        if (count >= max_field) {
            return -1;
        }
        field[count++] = p;
        while (*p != '\0' && *p != ',') {
            ++p;
        }
        if (*p == ',') {
            *p++ = '\0';
        }
    }
    *out_count = count;
    return 0;
}

static int mi_is_yyyymmdd(const char *s)
{
    size_t i;

    if (strlen(s) != MI_DATE_LEN) {
        return 0;
    }
    for (i = 0U; i < MI_DATE_LEN; ++i) {
        if (isdigit((unsigned char)s[i]) == 0) {
            return 0;
        }
    }
    return 1;
}

static int mi_parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *mi_trim(end) != '\0') {
        return -1;
    }
    *out = (int64_t)v;
    return 0;
}

static int mi_now(char *dst, size_t dstsz)
{
    time_t now = time(NULL);
    struct tm tmv;

    if (now == (time_t)-1) {
        return -1;
    }
#if defined(_WIN32)
    if (localtime_s(&tmv, &now) != 0) {
        return -1;
    }
#else
    {
        struct tm *tmp = localtime(&now);
        if (tmp == NULL) {
            return -1;
        }
        tmv = *tmp;
    }
#endif
    if (strftime(dst, dstsz, "%Y%m%d%H%M%S", &tmv) == 0U) {
        return -1;
    }
    return 0;
}

static int mi_is_business_day(const char *yyyymmdd)
{
    struct tm tmv;
    char buf[5];
    time_t t;

    if (!mi_is_yyyymmdd(yyyymmdd)) {
        return 0;
    }
    memset(&tmv, 0, sizeof(tmv));
    memcpy(buf, yyyymmdd, 4U);
    buf[4] = '\0';
    tmv.tm_year = atoi(buf) - 1900;
    memcpy(buf, yyyymmdd + 4, 2U);
    buf[2] = '\0';
    tmv.tm_mon = atoi(buf) - 1;
    memcpy(buf, yyyymmdd + 6, 2U);
    buf[2] = '\0';
    tmv.tm_mday = atoi(buf);
    tmv.tm_isdst = -1;

    t = mktime(&tmv);
    if (t == (time_t)-1) {
        return 0;
    }
    return tmv.tm_wday != 0 && tmv.tm_wday != 6;
}

static int mi_parse_conf_line(char *line, MiConfRec *rec)
{
    char *f[5];
    size_t n = 0U;

    if (mi_split_csv(line, f, 5U, &n) != 0 || n != 5U) {
        return -1;
    }
    if (mi_copy_field(rec->key, sizeof(rec->key), mi_trim(f[0])) != 0 ||
        mi_copy_field(rec->value, sizeof(rec->value), mi_trim(f[1])) != 0 ||
        mi_copy_field(rec->apply_dt, sizeof(rec->apply_dt), mi_trim(f[2])) != 0 ||
        mi_copy_field(rec->expire_dt, sizeof(rec->expire_dt), mi_trim(f[3])) != 0 ||
        mi_copy_field(rec->updated_at, sizeof(rec->updated_at), mi_trim(f[4])) != 0) {
        return -1;
    }
    return mi_is_yyyymmdd(rec->apply_dt) && mi_is_yyyymmdd(rec->expire_dt) ? 0 : -1;
}

static int mi_parse_merchant_line(char *line, MiMerchantRec *rec)
{
    char *f[4];
    size_t n = 0U;

    if (mi_split_csv(line, f, 4U, &n) != 0 || n != 4U) {
        return -1;
    }
    if (mi_copy_field(rec->code, sizeof(rec->code), mi_trim(f[0])) != 0 ||
        mi_copy_field(rec->name, sizeof(rec->name), mi_trim(f[1])) != 0 ||
        mi_copy_field(rec->status, sizeof(rec->status), mi_trim(f[2])) != 0 ||
        mi_copy_field(rec->bank_acct, sizeof(rec->bank_acct), mi_trim(f[3])) != 0) {
        return -1;
    }
    return rec->code[0] != '\0' && strlen(rec->status) == MI_STATUS_LEN ? 0 : -1;
}

static int mi_parse_txn_line(char *line, MiTxnRec *rec)
{
    char *f[5];
    size_t n = 0U;

    if (mi_split_csv(line, f, 5U, &n) != 0 || n != 5U) {
        return -1;
    }
    if (mi_copy_field(rec->txn_id, sizeof(rec->txn_id), mi_trim(f[0])) != 0 ||
        mi_copy_field(rec->merchant_code, sizeof(rec->merchant_code), mi_trim(f[1])) != 0 ||
        mi_copy_field(rec->txn_dt, sizeof(rec->txn_dt), mi_trim(f[4])) != 0 ||
        mi_parse_i64(mi_trim(f[3]), &rec->amount) != 0) {
        return -1;
    }
    f[2] = mi_trim(f[2]);
    if (strlen(f[2]) != 1U || (f[2][0] != 'C' && f[2][0] != 'R') ||
        rec->amount <= 0 || !mi_is_yyyymmdd(rec->txn_dt)) {
        return -1;
    }
    rec->txn_kbn = f[2][0];
    return 0;
}

static int mi_read_target_dt(char *target_dt, size_t target_sz)
{
    FILE *fp = fopen(MI_CONF_IN_PATH, "r");
    char line[MI_LINE_MAX];

    if (fp == NULL) {
        return -1;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        MiConfRec rec;
        mi_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (mi_parse_conf_line(line, &rec) != 0) {
            fclose(fp);
            return -1;
        }
        if (strcmp(rec.key, "MIPAY_CLOSE_TARGET_DT") == 0) {
            if (mi_copy_field(target_dt, target_sz, rec.value) != 0 || !mi_is_yyyymmdd(target_dt)) {
                fclose(fp);
                return -1;
            }
            fclose(fp);
            return 0;
        }
    }
    fclose(fp);
    return -1;
}

static int mi_completed_step(const char *target_dt, const char *step)
{
    FILE *fp = fopen(MI_CONF_IN_PATH, "r");
    char line[MI_LINE_MAX];
    char key[MI_KEY_MAX];

    if (fp == NULL) {
        return 0;
    }
    if (snprintf(key, sizeof(key), "MIPAY_CLOSECTL.%s.%s.RC", target_dt, step) < 0) {
        fclose(fp);
        return 0;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        MiConfRec rec;
        mi_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (mi_parse_conf_line(line, &rec) == 0 &&
            strcmp(rec.key, key) == 0 &&
            strcmp(rec.value, "0") == 0) {
            fclose(fp);
            return 1;
        }
    }
    fclose(fp);
    return 0;
}

static int mi_append_conf(const char *target_dt, const char *step, uint64_t count, int rc)
{
    FILE *fp = fopen(MI_CONF_OUT_PATH, "a");
    char now[32];
    char key[MI_KEY_MAX];

    if (fp == NULL || mi_now(now, sizeof(now)) != 0) {
        if (fp != NULL) {
            fclose(fp);
        }
        return -1;
    }
    if (snprintf(key, sizeof(key), "MIPAY_CLOSECTL.%s.%s.RC", target_dt, step) < 0 ||
        fprintf(fp, "%s,%d,%s,99991231,%s\n", key, rc, target_dt, now) < 0 ||
        snprintf(key, sizeof(key), "MIPAY_CLOSECTL.%s.%s.COUNT", target_dt, step) < 0 ||
        fprintf(fp, "%s,%" PRIu64 ",%s,99991231,%s\n", key, count, target_dt, now) < 0) {
        fclose(fp);
        return -1;
    }
    return fclose(fp) == 0 ? 0 : -1;
}

static int mi_find_merchant(const MiMerchantRec *merchants, size_t merchant_count, const char *code)
{
    size_t i;

    for (i = 0U; i < merchant_count; ++i) {
        if (strcmp(merchants[i].code, code) == 0) {
            return (int)i;
        }
    }
    return -1;
}

static int mi_load_merchants(MiMerchantRec *merchants, size_t *merchant_count)
{
    FILE *fp = fopen(MI_MER_PATH, "r");
    char line[MI_LINE_MAX];
    size_t count = 0U;

    if (fp == NULL) {
        return -1;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        MiMerchantRec rec;
        mi_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (count >= MI_MAX_MERCHANTS || mi_parse_merchant_line(line, &rec) != 0) {
            fclose(fp);
            return -1;
        }
        if (strcmp(rec.status, "01") != 0 && strcmp(rec.status, "02") != 0 && strcmp(rec.status, "09") != 0) {
            fclose(fp);
            return -1;
        }
        merchants[count++] = rec;
    }
    *merchant_count = count;
    fclose(fp);
    return count > 0U ? 0 : -1;
}

static int mi_add_amount(int64_t base, int64_t delta, int64_t *out)
{
    if ((delta > 0 && base > INT64_MAX - delta) ||
        (delta < 0 && base < INT64_MIN - delta)) {
        return -1;
    }
    *out = base + delta;
    return 0;
}

static int mi_add_agg(MiMerchantAgg *aggs, size_t *agg_count, const char *code, char kbn, int64_t amount)
{
    size_t i;
    int64_t signed_amount = kbn == 'C' ? amount : -amount;

    for (i = 0U; i < *agg_count; ++i) {
        if (strcmp(aggs[i].code, code) == 0) {
            if (mi_add_amount(aggs[i].net, signed_amount, &aggs[i].net) != 0) {
                return -1;
            }
            if (kbn == 'C') {
                ++aggs[i].capture_count;
            } else {
                ++aggs[i].refund_count;
            }
            return 0;
        }
    }
    if (*agg_count >= MI_MAX_MERCHANTS) {
        return -1;
    }
    memset(&aggs[*agg_count], 0, sizeof(aggs[*agg_count]));
    if (mi_copy_field(aggs[*agg_count].code, sizeof(aggs[*agg_count].code), code) != 0) {
        return -1;
    }
    aggs[*agg_count].net = signed_amount;
    if (kbn == 'C') {
        aggs[*agg_count].capture_count = 1U;
    } else {
        aggs[*agg_count].refund_count = 1U;
    }
    ++*agg_count;
    return 0;
}

static int mi_scan_txns(const char *target_dt, const MiMerchantRec *merchants, size_t merchant_count,
                        MiMerchantAgg *aggs, size_t *agg_count, uint64_t *input_count)
{
    FILE *fp = fopen(MI_TXN_PATH, "r");
    char line[MI_LINE_MAX];

    if (fp == NULL) {
        return -1;
    }
    *input_count = 0U;
    *agg_count = 0U;

    while (fgets(line, sizeof(line), fp) != NULL) {
        MiTxnRec rec;
        int pos;

        mi_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (mi_parse_txn_line(line, &rec) != 0) {
            fclose(fp);
            return -1;
        }
        if (strcmp(rec.txn_dt, target_dt) != 0) {
            continue;
        }
        ++*input_count;
        pos = mi_find_merchant(merchants, merchant_count, rec.merchant_code);
        if (pos < 0 || strcmp(merchants[pos].status, "01") != 0) {
            continue;
        }
        if (mi_add_agg(aggs, agg_count, rec.merchant_code, rec.txn_kbn, rec.amount) != 0) {
            fclose(fp);
            return -1;
        }
    }
    fclose(fp);
    return *input_count > 0U ? 0 : -1;
}

static int mi_compute_settlement(MiMerchantAgg *aggs, size_t agg_count)
{
    size_t i;

    for (i = 0U; i < agg_count; ++i) {
        int64_t payout;

        aggs[i].charge = mipay_proc_charge(aggs[i].net);
        payout = mipay_merchant_payout(aggs[i].net);
        if ((aggs[i].net >= 0 && (aggs[i].charge < 0 || payout < 0)) ||
            (aggs[i].net < 0 && payout > 0)) {
            return -1;
        }
        aggs[i].payout = payout;
    }
    return 0;
}

static int mi_write_detail(const char *target_dt, const MiMerchantAgg *aggs, size_t agg_count)
{
    FILE *fp = fopen("MIPAY_DETAIL.csv", "w");
    size_t i;

    if (fp == NULL) {
        return -1;
    }
    for (i = 0U; i < agg_count; ++i) {
        if (fprintf(fp, "%s,%s,%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRIu64 ",%" PRIu64 "\n",
                    target_dt, aggs[i].code, aggs[i].net, aggs[i].charge, aggs[i].payout,
                    aggs[i].capture_count, aggs[i].refund_count) < 0) {
            fclose(fp);
            return -1;
        }
    }
    return fclose(fp) == 0 ? 0 : -1;
}

static int mi_write_summary(const char *target_dt, const MiMerchantAgg *aggs, size_t agg_count)
{
    FILE *fp = fopen("MIPAY_SUMMARY.csv", "w");
    size_t i;
    int64_t net = 0;
    int64_t charge = 0;
    int64_t payout = 0;
    uint64_t captures = 0U;
    uint64_t refunds = 0U;

    if (fp == NULL) {
        return -1;
    }
    for (i = 0U; i < agg_count; ++i) {
        if (mi_add_amount(net, aggs[i].net, &net) != 0 ||
            mi_add_amount(charge, aggs[i].charge, &charge) != 0 ||
            mi_add_amount(payout, aggs[i].payout, &payout) != 0) {
            fclose(fp);
            return -1;
        }
        captures += aggs[i].capture_count;
        refunds += aggs[i].refund_count;
    }
    if (fprintf(fp, "%s,%" PRIu64 ",%" PRIu64 ",%" PRIu64 ",%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
                target_dt, (uint64_t)agg_count, captures, refunds, net, charge, payout) < 0) {
        fclose(fp);
        return -1;
    }
    return fclose(fp) == 0 ? 0 : -1;
}

int main(void)
{
    static const char *steps[MI_STEP_COUNT] = {
        "CALENDAR",
        "INPUT",
        "MERCHANT",
        "IMPORT",
        "SETTLE",
        "OUTPUT"
    };
    MiMerchantRec merchants[MI_MAX_MERCHANTS];
    MiMerchantAgg aggs[MI_MAX_MERCHANTS];
    MiStepLog logs[MI_STEP_COUNT];
    char target_dt[MI_DATE_LEN + 1];
    size_t merchant_count = 0U;
    size_t agg_count = 0U;
    uint64_t input_count = 0U;
    size_t i;

    memset(logs, 0, sizeof(logs));
    for (i = 0U; i < MI_STEP_COUNT; ++i) {
        logs[i].step = steps[i];
        logs[i].rc = MIPAY_DECISION_OK;
    }

    if (mi_read_target_dt(target_dt, sizeof(target_dt)) != 0) {
        fprintf(stderr, "E1001 対象日取得失敗\n");
        return 12;
    }

    if (!mi_completed_step(target_dt, steps[0])) {
        if (!mi_is_business_day(target_dt)) {
            logs[0].rc = MIPAY_DECISION_REJECT;
            if (mi_append_conf(target_dt, steps[0], 0U, logs[0].rc) != 0) {
                fprintf(stderr, "E1002 制御記録失敗\n");
                return 12;
            }
            return logs[0].rc;
        }
        if (mi_append_conf(target_dt, steps[0], 0U, logs[0].rc) != 0) {
            fprintf(stderr, "E1003 制御記録失敗\n");
            return 12;
        }
    }

    if (mi_load_merchants(merchants, &merchant_count) != 0) {
        fprintf(stderr, "E2001 加盟店ファイル検証失敗\n");
        return 12;
    }

    if (mi_scan_txns(target_dt, merchants, merchant_count, aggs, &agg_count, &input_count) != 0) {
        fprintf(stderr, "E3001 取引ファイル検証失敗\n");
        return 12;
    }

    logs[1].input_count = input_count;
    if (!mi_completed_step(target_dt, steps[1]) &&
        mi_append_conf(target_dt, steps[1], logs[1].input_count, logs[1].rc) != 0) {
        fprintf(stderr, "E3002 件数記録失敗\n");
        return 12;
    }

    logs[2].input_count = (uint64_t)merchant_count;
    if (!mi_completed_step(target_dt, steps[2]) &&
        mi_append_conf(target_dt, steps[2], logs[2].input_count, logs[2].rc) != 0) {
        fprintf(stderr, "E2002 加盟店記録失敗\n");
        return 12;
    }

    logs[3].input_count = input_count;
    if (!mi_completed_step(target_dt, steps[3]) &&
        mi_append_conf(target_dt, steps[3], logs[3].input_count, logs[3].rc) != 0) {
        fprintf(stderr, "E3003 取込記録失敗\n");
        return 12;
    }

    if (!mi_completed_step(target_dt, steps[4])) {
        if (mi_compute_settlement(aggs, agg_count) != 0) {
            fprintf(stderr, "E4001 精算計算失敗\n");
            return 12;
        }
        logs[4].input_count = (uint64_t)agg_count;
        if (mi_append_conf(target_dt, steps[4], logs[4].input_count, logs[4].rc) != 0) {
            fprintf(stderr, "E4002 精算記録失敗\n");
            return 12;
        }
    } else if (mi_compute_settlement(aggs, agg_count) != 0) {
        fprintf(stderr, "E4003 精算再構成失敗\n");
        return 12;
    }

    if (!mi_completed_step(target_dt, steps[5])) {
        if (mi_write_detail(target_dt, aggs, agg_count) != 0 ||
            mi_write_summary(target_dt, aggs, agg_count) != 0) {
            fprintf(stderr, "E5001 出力ファイル作成失敗\n");
            return 12;
        }
        logs[5].input_count = (uint64_t)agg_count;
        if (mi_append_conf(target_dt, steps[5], logs[5].input_count, logs[5].rc) != 0) {
            fprintf(stderr, "E5002 出力記録失敗\n");
            return 12;
        }
    }

    return MIPAY_DECISION_OK;
}
