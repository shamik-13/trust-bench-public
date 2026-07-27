/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20250206  精算基盤    初版作成、PSFEEF手数料条件参照処理を追加
 */

#include "mipay_settle.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIPAY_LOCAL_RC_OK 0
#define MIPAY_LOCAL_RC_MERCHANT_STOPPED 1
#define MIPAY_LOCAL_RC_PLAN_NOT_FOUND 2
#define MIPAY_LOCAL_RC_PARSE_IO 3

#define LINE_MAX_LEN 512
#define MERCHANT_CODE_LEN 32
#define FEE_PLAN_LEN 32
#define MERCHANT_NAME_LEN 96
#define ACCOUNT_NO_LEN 32
#define KBN_LEN 8

#define STATUS_SETTLE_TARGET "01"
#define STATUS_SETTLE_HOLD "02"
#define STATUS_CANCELED "09"

typedef struct {
    char fee_plan_id[FEE_PLAN_LEN];
    char merchant_code[MERCHANT_CODE_LEN];
    char rate_kbn[KBN_LEN];
    int64_t rate_value;
    int64_t min_fee_amt;
    int64_t max_fee_amt;
    int apply_dt;
} psfeef_record;

typedef struct {
    char merchant_code[MERCHANT_CODE_LEN];
    char merchant_name[MERCHANT_NAME_LEN];
    char mer_status[KBN_LEN];
    char bank_acct_no[ACCOUNT_NO_LEN];
} psmerf_record;

static void chomp(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (n >= dstsz) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static char *next_csv_field(char **cursor)
{
    char *p;
    char *start;

    if (cursor == NULL || *cursor == NULL) {
        return NULL;
    }

    p = *cursor;
    start = p;

    while (*p != '\0' && *p != ',') {
        p++;
    }

    if (*p == ',') {
        *p = '\0';
        *cursor = p + 1;
    } else {
        *cursor = NULL;
    }

    return start;
}

static int parse_i64(const char *s, int64_t minv, int64_t maxv, int64_t *out)
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
    if (v < minv || v > maxv) {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int parse_date_yyyymmdd(const char *s, int *out)
{
    int64_t v;
    int y;
    int m;
    int d;
    static const int mdays[] = {
        31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    };

    if (s == NULL || strlen(s) != 8 ||
        parse_i64(s, 19000101, 29991231, &v) != 0) {
        return -1;
    }

    y = (int)(v / 10000);
    m = (int)((v / 100) % 100);
    d = (int)(v % 100);

    if (m < 1 || m > 12) {
        return -1;
    }

    if (m == 2 && ((y % 400 == 0) || (y % 4 == 0 && y % 100 != 0))) {
        if (d < 1 || d > 29) {
            return -1;
        }
    } else if (d < 1 || d > mdays[m - 1]) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int parse_psmerf_line(char *line, psmerf_record *rec)
{
    char *cur = line;
    char *f1 = next_csv_field(&cur);
    char *f2 = next_csv_field(&cur);
    char *f3 = next_csv_field(&cur);
    char *f4 = next_csv_field(&cur);

    if (f1 == NULL || f2 == NULL || f3 == NULL || f4 == NULL || cur != NULL) {
        return -1;
    }

    if (copy_field(rec->merchant_code, sizeof(rec->merchant_code), f1) != 0 ||
        copy_field(rec->merchant_name, sizeof(rec->merchant_name), f2) != 0 ||
        copy_field(rec->mer_status, sizeof(rec->mer_status), f3) != 0 ||
        copy_field(rec->bank_acct_no, sizeof(rec->bank_acct_no), f4) != 0) {
        return -1;
    }

    if (strcmp(rec->mer_status, STATUS_SETTLE_TARGET) != 0 &&
        strcmp(rec->mer_status, STATUS_SETTLE_HOLD) != 0 &&
        strcmp(rec->mer_status, STATUS_CANCELED) != 0) {
        return -1;
    }

    return 0;
}

static int parse_psfeef_line(char *line, psfeef_record *rec)
{
    char *cur = line;
    char *f1 = next_csv_field(&cur);
    char *f2 = next_csv_field(&cur);
    char *f3 = next_csv_field(&cur);
    char *f4 = next_csv_field(&cur);
    char *f5 = next_csv_field(&cur);
    char *f6 = next_csv_field(&cur);
    char *f7 = next_csv_field(&cur);
    int64_t rate_value;
    int64_t min_fee;
    int64_t max_fee;
    int apply_dt;

    if (f1 == NULL || f2 == NULL || f3 == NULL || f4 == NULL ||
        f5 == NULL || f6 == NULL || f7 == NULL || cur != NULL) {
        return -1;
    }

    if (parse_i64(f4, 0, 1000000, &rate_value) != 0 ||
        parse_i64(f5, 0, INT64_MAX / 4, &min_fee) != 0 ||
        parse_i64(f6, 0, INT64_MAX / 4, &max_fee) != 0 ||
        parse_date_yyyymmdd(f7, &apply_dt) != 0) {
        return -1;
    }

    if (max_fee < min_fee) {
        return -1;
    }

    if (copy_field(rec->fee_plan_id, sizeof(rec->fee_plan_id), f1) != 0 ||
        copy_field(rec->merchant_code, sizeof(rec->merchant_code), f2) != 0 ||
        copy_field(rec->rate_kbn, sizeof(rec->rate_kbn), f3) != 0) {
        return -1;
    }

    rec->rate_value = rate_value;
    rec->min_fee_amt = min_fee;
    rec->max_fee_amt = max_fee;
    rec->apply_dt = apply_dt;
    return 0;
}

static int load_merchant_status(const char *path,
                                const char *merchant_code,
                                psmerf_record *found)
{
    FILE *fp = fopen(path, "r");
    char line[LINE_MAX_LEN];
    int line_no = 0;
    int matched = 0;

    if (fp == NULL) {
        fprintf(stderr, "E201 PSMERFオープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        psmerf_record rec;

        line_no++;
        chomp(line);
        if (line[0] == '\0') {
            continue;
        }

        if (parse_psmerf_line(line, &rec) != 0) {
            fprintf(stderr, "E202 PSMERF解析失敗 行=%d\n", line_no);
            fclose(fp);
            return -1;
        }

        if (strcmp(rec.merchant_code, merchant_code) == 0) {
            *found = rec;
            matched = 1;
            break;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E203 PSMERF読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return matched;
}

static int find_effective_fee(const char *path,
                              const char *merchant_code,
                              int apply_dt,
                              psfeef_record *best)
{
    FILE *fp = fopen(path, "r");
    char line[LINE_MAX_LEN];
    int line_no = 0;
    int matched = 0;

    if (fp == NULL) {
        fprintf(stderr, "E301 PSFEEFオープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        psfeef_record rec;

        line_no++;
        chomp(line);
        if (line[0] == '\0') {
            continue;
        }

        if (parse_psfeef_line(line, &rec) != 0) {
            fprintf(stderr, "E302 PSFEEF解析失敗 行=%d\n", line_no);
            fclose(fp);
            return -1;
        }

        if (strcmp(rec.merchant_code, merchant_code) == 0 && rec.apply_dt <= apply_dt) {
            if (!matched ||
                rec.apply_dt > best->apply_dt ||
                (rec.apply_dt == best->apply_dt &&
                 strcmp(rec.fee_plan_id, best->fee_plan_id) < 0)) {
                *best = rec;
                matched = 1;
            }
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E303 PSFEEF読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return matched;
}

int main(void)
{
    const char *psfeef_path = getenv("PSFEEF_PATH");
    const char *psmerf_path = getenv("PSMERF_PATH");
    const char *merchant_code = getenv("MIPAY_MERCHANT_CODE");
    const char *apply_dt_text = getenv("MIPAY_APPLY_DT");
    psmerf_record merchant;
    psfeef_record fee;
    int apply_dt;
    int merchant_hit;
    int fee_hit;

    if (psfeef_path == NULL || psmerf_path == NULL ||
        merchant_code == NULL || apply_dt_text == NULL) {
        fprintf(stderr, "E101 環境変数不足\n");
        return MIPAY_LOCAL_RC_PARSE_IO;
    }

    if (strlen(merchant_code) == 0 ||
        strlen(merchant_code) >= MERCHANT_CODE_LEN ||
        parse_date_yyyymmdd(apply_dt_text, &apply_dt) != 0) {
        fprintf(stderr, "E102 入力条件不正\n");
        return MIPAY_LOCAL_RC_PARSE_IO;
    }

    merchant_hit = load_merchant_status(psmerf_path, merchant_code, &merchant);
    if (merchant_hit < 0) {
        return MIPAY_LOCAL_RC_PARSE_IO;
    }

    if (merchant_hit == 0) {
        fprintf(stdout, "W101 加盟店未登録\n");
        return MIPAY_LOCAL_RC_PLAN_NOT_FOUND;
    }

    if (strcmp(merchant.mer_status, STATUS_SETTLE_TARGET) != 0) {
        fprintf(stdout, "W102 停止加盟店\n");
        return MIPAY_LOCAL_RC_MERCHANT_STOPPED;
    }

    fee_hit = find_effective_fee(psfeef_path, merchant_code, apply_dt, &fee);
    if (fee_hit < 0) {
        return MIPAY_LOCAL_RC_PARSE_IO;
    }

    if (fee_hit == 0) {
        fprintf(stdout, "W103 手数料プラン未登録\n");
        return MIPAY_LOCAL_RC_PLAN_NOT_FOUND;
    }

    fprintf(stdout,
            "I101 手数料条件一致 PLAN=%s MERCHANT=%s RATEKBN=%s RATE=%lld MIN=%lld MAX=%lld APPLY=%08d\n",
            fee.fee_plan_id,
            fee.merchant_code,
            fee.rate_kbn,
            (long long)fee.rate_value,
            (long long)fee.min_fee_amt,
            (long long)fee.max_fee_amt,
            fee.apply_dt);

    return MIPAY_LOCAL_RC_OK;
}
