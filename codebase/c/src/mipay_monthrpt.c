/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20250115  MIYAZAKI  初版作成
 * 1.01  20250722  MIYAZAKI  消込照合と月次作成単位登録を追加
 */

#include "mipay_settle.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MIPAY_DECISION_OK
#define MIPAY_DECISION_OK 0
#endif

#ifndef MIPAY_DECISION_ERROR
#define MIPAY_DECISION_ERROR 12
#endif

#ifndef MIPAY_DECISION_WARN
#define MIPAY_DECISION_WARN 4
#endif

#define 入力上限 4096
#define 文字列上限 128
#define パス上限 256
#define 精算上限 20000
#define 明細上限 100000
#define 既存報告上限 20000
#define 入金上限 50000
#define 加盟店上限 20000

typedef struct {
    char settle_id[文字列上限];
    char merchant_code[文字列上限];
    int64_t net_amt;
    int64_t charge_amt;
    int64_t payout_amt;
    int settle_dt;
} 精算行;

typedef struct {
    char detail_id[文字列上限];
    char settle_id[文字列上限];
    char merchant_code[文字列上限];
    char txn_id[文字列上限];
    int64_t txn_amt;
    int64_t charge_amt;
    char line_kbn;
} 明細行;

typedef struct {
    char report_id[文字列上限];
    char merchant_code[文字列上限];
    char report_kbn[文字列上限];
    int period_from;
    int period_to;
    char output_path[パス上限];
    char create_status[文字列上限];
} 報告行;

typedef struct {
    char receipt_id[文字列上限];
    char merchant_code[文字列上限];
    int64_t receipt_amt;
    int receipt_dt;
    char match_status[文字列上限];
    char settle_id[文字列上限];
} 入金行;

typedef struct {
    char merchant_code[文字列上限];
    int64_t net_sum;
    int64_t charge_sum;
    int64_t payout_sum;
    int64_t receipt_sum;
    int64_t detail_amt_sum;
    int64_t detail_charge_sum;
    size_t settle_count;
    size_t receipt_count;
    size_t fail_count;
    size_t unmatch_count;
    int first_dt;
    int last_dt;
    int detail_mismatch;
} 加盟店集計;

static void 改行除去(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static char *空白除去(char *s)
{
    char *e;
    while (*s != '\0' && isspace((unsigned char)*s)) {
        ++s;
    }
    e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) {
        *--e = '\0';
    }
    return s;
}

static int 項目分割(char *line, char **cols, size_t need)
{
    size_t n = 0;
    char *p = line;

    while (n < need) {
        char *comma = strchr(p, ',');
        if (comma != NULL) {
            *comma = '\0';
        }
        cols[n++] = 空白除去(p);
        if (comma == NULL) {
            break;
        }
        p = comma + 1;
    }
    return n == need && strchr(cols[need - 1], ',') == NULL;
}

static int 文字列設定(char *dst, size_t cap, const char *src)
{
    size_t n = strlen(src);
    if (n == 0 || n >= cap) {
        return 0;
    }
    memcpy(dst, src, n + 1);
    return 1;
}

static int 金額読取(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *空白除去(end) != '\0') {
        return 0;
    }
    *out = (int64_t)v;
    return 1;
}

static int 日付読取(const char *s, int *out)
{
    char buf[9];
    size_t i;
    int y, m, d;

    if (strlen(s) != 8) {
        return 0;
    }
    for (i = 0; i < 8; ++i) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
        buf[i] = s[i];
    }
    buf[8] = '\0';
    y = (buf[0] - '0') * 1000 + (buf[1] - '0') * 100 + (buf[2] - '0') * 10 + (buf[3] - '0');
    m = (buf[4] - '0') * 10 + (buf[5] - '0');
    d = (buf[6] - '0') * 10 + (buf[7] - '0');
    if (y < 2000 || m < 1 || m > 12 || d < 1 || d > 31) {
        return 0;
    }
    *out = atoi(buf);
    return 1;
}

static int 精算読込(const char *path, 精算行 *rows, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[入力上限];

    if (fp == NULL) {
        fprintf(stderr, "E100:PSSETFを開けません:%s\n", path);
        return 0;
    }

    *count = 0;
    while (fgets(line, sizeof line, fp) != NULL) {
        char *cols[6];
        改行除去(line);
        if (line[0] == '\0') {
            continue;
        }
        if (*count >= 精算上限 || !項目分割(line, cols, 6)) {
            fprintf(stderr, "E101:PSSETF項目異常\n");
            fclose(fp);
            return 0;
        }
        if (!文字列設定(rows[*count].settle_id, sizeof rows[*count].settle_id, cols[0]) ||
            !文字列設定(rows[*count].merchant_code, sizeof rows[*count].merchant_code, cols[1]) ||
            !金額読取(cols[2], &rows[*count].net_amt) ||
            !金額読取(cols[3], &rows[*count].charge_amt) ||
            !金額読取(cols[4], &rows[*count].payout_amt) ||
            !日付読取(cols[5], &rows[*count].settle_dt)) {
            fprintf(stderr, "E102:PSSETF値異常\n");
            fclose(fp);
            return 0;
        }
        ++*count;
    }
    if (ferror(fp)) {
        fprintf(stderr, "E103:PSSETF読込異常\n");
        fclose(fp);
        return 0;
    }
    fclose(fp);
    return 1;
}

static int 明細読込(const char *path, 明細行 *rows, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[入力上限];

    if (fp == NULL) {
        fprintf(stderr, "E200:PSDTLFを開けません:%s\n", path);
        return 0;
    }

    *count = 0;
    while (fgets(line, sizeof line, fp) != NULL) {
        char *cols[7];
        改行除去(line);
        if (line[0] == '\0') {
            continue;
        }
        if (*count >= 明細上限 || !項目分割(line, cols, 7)) {
            fprintf(stderr, "E201:PSDTLF項目異常\n");
            fclose(fp);
            return 0;
        }
        if (!文字列設定(rows[*count].detail_id, sizeof rows[*count].detail_id, cols[0]) ||
            !文字列設定(rows[*count].settle_id, sizeof rows[*count].settle_id, cols[1]) ||
            !文字列設定(rows[*count].merchant_code, sizeof rows[*count].merchant_code, cols[2]) ||
            !文字列設定(rows[*count].txn_id, sizeof rows[*count].txn_id, cols[3]) ||
            !金額読取(cols[4], &rows[*count].txn_amt) ||
            !金額読取(cols[5], &rows[*count].charge_amt) ||
            strlen(cols[6]) != 1 || (cols[6][0] != 'C' && cols[6][0] != 'R')) {
            fprintf(stderr, "E202:PSDTLF値異常\n");
            fclose(fp);
            return 0;
        }
        rows[*count].line_kbn = cols[6][0];
        ++*count;
    }
    if (ferror(fp)) {
        fprintf(stderr, "E203:PSDTLF読込異常\n");
        fclose(fp);
        return 0;
    }
    fclose(fp);
    return 1;
}

static int 報告読込(const char *path, 報告行 *rows, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[入力上限];

    *count = 0;
    if (fp == NULL) {
        return 1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *cols[7];
        改行除去(line);
        if (line[0] == '\0') {
            continue;
        }
        if (*count >= 既存報告上限 || !項目分割(line, cols, 7)) {
            fprintf(stderr, "E301:PSRPTF項目異常\n");
            fclose(fp);
            return 0;
        }
        if (!文字列設定(rows[*count].report_id, sizeof rows[*count].report_id, cols[0]) ||
            !文字列設定(rows[*count].merchant_code, sizeof rows[*count].merchant_code, cols[1]) ||
            !文字列設定(rows[*count].report_kbn, sizeof rows[*count].report_kbn, cols[2]) ||
            !日付読取(cols[3], &rows[*count].period_from) ||
            !日付読取(cols[4], &rows[*count].period_to) ||
            !文字列設定(rows[*count].output_path, sizeof rows[*count].output_path, cols[5]) ||
            !文字列設定(rows[*count].create_status, sizeof rows[*count].create_status, cols[6])) {
            fprintf(stderr, "E302:PSRPTF値異常\n");
            fclose(fp);
            return 0;
        }
        ++*count;
    }
    if (ferror(fp)) {
        fprintf(stderr, "E303:PSRPTF読込異常\n");
        fclose(fp);
        return 0;
    }
    fclose(fp);
    return 1;
}

static int 入金読込(const char *path, 入金行 *rows, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[入力上限];

    if (fp == NULL) {
        fprintf(stderr, "E400:PSRCVFを開けません:%s\n", path);
        return 0;
    }

    *count = 0;
    while (fgets(line, sizeof line, fp) != NULL) {
        char *cols[6];
        改行除去(line);
        if (line[0] == '\0') {
            continue;
        }
        if (*count >= 入金上限 || !項目分割(line, cols, 6)) {
            fprintf(stderr, "E401:PSRCVF項目異常\n");
            fclose(fp);
            return 0;
        }
        if (!文字列設定(rows[*count].receipt_id, sizeof rows[*count].receipt_id, cols[0]) ||
            !文字列設定(rows[*count].merchant_code, sizeof rows[*count].merchant_code, cols[1]) ||
            !金額読取(cols[2], &rows[*count].receipt_amt) ||
            !日付読取(cols[3], &rows[*count].receipt_dt) ||
            !文字列設定(rows[*count].match_status, sizeof rows[*count].match_status, cols[4]) ||
            !文字列設定(rows[*count].settle_id, sizeof rows[*count].settle_id, cols[5])) {
            fprintf(stderr, "E402:PSRCVF値異常\n");
            fclose(fp);
            return 0;
        }
        ++*count;
    }
    if (ferror(fp)) {
        fprintf(stderr, "E403:PSRCVF読込異常\n");
        fclose(fp);
        return 0;
    }
    fclose(fp);
    return 1;
}

static 加盟店集計 *集計取得(加盟店集計 *rows, size_t *count, const char *merchant_code)
{
    size_t i;

    for (i = 0; i < *count; ++i) {
        if (strcmp(rows[i].merchant_code, merchant_code) == 0) {
            return &rows[i];
        }
    }
    if (*count >= 加盟店上限) {
        return NULL;
    }
    memset(&rows[*count], 0, sizeof rows[*count]);
    if (!文字列設定(rows[*count].merchant_code, sizeof rows[*count].merchant_code, merchant_code)) {
        return NULL;
    }
    rows[*count].first_dt = INT_MAX;
    return &rows[(*count)++];
}

static int 加算検査(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return 0;
    }
    *out = a + b;
    return 1;
}

static int 精算集計(加盟店集計 *acc, const 精算行 *s)
{
    int64_t v;

    if (!加算検査(acc->net_sum, s->net_amt, &v)) {
        return 0;
    }
    acc->net_sum = v;
    if (!加算検査(acc->charge_sum, s->charge_amt, &v)) {
        return 0;
    }
    acc->charge_sum = v;
    if (!加算検査(acc->payout_sum, s->payout_amt, &v)) {
        return 0;
    }
    acc->payout_sum = v;
    if (s->settle_dt < acc->first_dt) {
        acc->first_dt = s->settle_dt;
    }
    if (s->settle_dt > acc->last_dt) {
        acc->last_dt = s->settle_dt;
    }
    ++acc->settle_count;
    return 1;
}

static int 明細集計(加盟店集計 *groups, size_t group_count, const 明細行 *details, size_t detail_count)
{
    size_t i;

    for (i = 0; i < detail_count; ++i) {
        size_t g;
        for (g = 0; g < group_count; ++g) {
            if (strcmp(groups[g].merchant_code, details[i].merchant_code) == 0) {
                int64_t amt = details[i].line_kbn == 'R' ? -details[i].txn_amt : details[i].txn_amt;
                int64_t v;
                if (!加算検査(groups[g].detail_amt_sum, amt, &v)) {
                    return 0;
                }
                groups[g].detail_amt_sum = v;
                if (!加算検査(groups[g].detail_charge_sum, details[i].charge_amt, &v)) {
                    return 0;
                }
                groups[g].detail_charge_sum = v;
                break;
            }
        }
    }

    /* 手数料額は精算ファイル(PSSETF)の登録値を正として集計する。月次では
       手数料の丸め直しは行わず、精算合計と明細合計の整合、および
       純額-手数料=支払額 の関係のみを確認する。 */
    for (i = 0; i < group_count; ++i) {
        if (groups[i].charge_sum != groups[i].detail_charge_sum ||
            groups[i].net_sum - groups[i].charge_sum != groups[i].payout_sum) {
            groups[i].detail_mismatch = 1;
        }
    }
    return 1;
}

static int 入金集計(加盟店集計 *groups, size_t group_count, const 入金行 *receipts, size_t receipt_count)
{
    size_t i;

    for (i = 0; i < receipt_count; ++i) {
        size_t g;
        for (g = 0; g < group_count; ++g) {
            if (strcmp(groups[g].merchant_code, receipts[i].merchant_code) == 0) {
                int64_t v;
                if (strcmp(receipts[i].match_status, "01") == 0) {
                    if (!加算検査(groups[g].receipt_sum, receipts[i].receipt_amt, &v)) {
                        return 0;
                    }
                    groups[g].receipt_sum = v;
                    ++groups[g].receipt_count;
                } else if (strcmp(receipts[i].match_status, "09") == 0) {
                    ++groups[g].fail_count;
                } else {
                    ++groups[g].unmatch_count;
                }
                break;
            }
        }
    }
    return 1;
}

static int 既存報告あり(const 報告行 *reports, size_t count, const char *merchant_code, int from, int to)
{
    size_t i;

    for (i = 0; i < count; ++i) {
        if (strcmp(reports[i].merchant_code, merchant_code) == 0 &&
            reports[i].period_from == from &&
            reports[i].period_to == to &&
            strcmp(reports[i].report_kbn, "MONTH") == 0) {
            return 1;
        }
    }
    return 0;
}

static int 報告追記(const char *path, const 加盟店集計 *groups, size_t count, const 報告行 *reports, size_t report_count)
{
    FILE *fp = fopen(path, "a");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "E500:PSRPTFを更新できません:%s\n", path);
        return 0;
    }

    for (i = 0; i < count; ++i) {
        const 加盟店集計 *g = &groups[i];
        char status[16];
        char report_id[文字列上限];
        char output_path[パス上限];

        if (g->settle_count == 0 || 既存報告あり(reports, report_count, g->merchant_code, g->first_dt, g->last_dt)) {
            continue;
        }
        if (g->detail_mismatch) {
            strcpy(status, "ERR-CALC");
        } else if (g->fail_count > 0) {
            strcpy(status, "ERR-BANK");
        } else if (g->receipt_sum != g->payout_sum || g->unmatch_count > 0) {
            strcpy(status, "WAIT-RCV");
        } else {
            strcpy(status, "READY");
        }

        if (snprintf(report_id, sizeof report_id, "MR%08d%04zu", g->last_dt, i + 1) >= (int)sizeof report_id ||
            snprintf(output_path, sizeof output_path, "out/%s/%08d_%08d.csv", g->merchant_code, g->first_dt, g->last_dt) >= (int)sizeof output_path) {
            fprintf(stderr, "E501:PSRPTF生成値が長すぎます\n");
            fclose(fp);
            return 0;
        }

        if (fprintf(fp, "%s,%s,MONTH,%08d,%08d,%s,%s\n",
                    report_id, g->merchant_code, g->first_dt, g->last_dt, output_path, status) < 0) {
            fprintf(stderr, "E502:PSRPTF書込異常\n");
            fclose(fp);
            return 0;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "E503:PSRPTF終了異常\n");
        return 0;
    }
    return 1;
}

int main(void)
{
    static 精算行 settles[精算上限];
    static 明細行 details[明細上限];
    static 報告行 reports[既存報告上限];
    static 入金行 receipts[入金上限];
    static 加盟店集計 groups[加盟店上限];

    size_t settle_count = 0;
    size_t detail_count = 0;
    size_t report_count = 0;
    size_t receipt_count = 0;
    size_t group_count = 0;
    size_t i;
    int warn = 0;

    const char *pssetf = getenv("PSSETF");
    const char *psdtlf = getenv("PSDTLF");
    const char *psrptf = getenv("PSRPTF");
    const char *psrcvf = getenv("PSRCVF");

    if (pssetf == NULL) {
        pssetf = "PSSETF.csv";
    }
    if (psdtlf == NULL) {
        psdtlf = "PSDTLF.csv";
    }
    if (psrptf == NULL) {
        psrptf = "PSRPTF.csv";
    }
    if (psrcvf == NULL) {
        psrcvf = "PSRCVF.csv";
    }

    if (!精算読込(pssetf, settles, &settle_count) ||
        !明細読込(psdtlf, details, &detail_count) ||
        !報告読込(psrptf, reports, &report_count) ||
        !入金読込(psrcvf, receipts, &receipt_count)) {
        return MIPAY_DECISION_ERROR;
    }

    for (i = 0; i < settle_count; ++i) {
        加盟店集計 *g = 集計取得(groups, &group_count, settles[i].merchant_code);
        if (g == NULL || !精算集計(g, &settles[i])) {
            fprintf(stderr, "E600:加盟店集計異常\n");
            return MIPAY_DECISION_ERROR;
        }
    }

    if (!明細集計(groups, group_count, details, detail_count) ||
        !入金集計(groups, group_count, receipts, receipt_count)) {
        fprintf(stderr, "E601:照合集計異常\n");
        return MIPAY_DECISION_ERROR;
    }

    for (i = 0; i < group_count; ++i) {
        if (groups[i].detail_mismatch || groups[i].fail_count > 0 ||
            groups[i].unmatch_count > 0 || groups[i].receipt_sum != groups[i].payout_sum) {
            warn = 1;
        }
    }

    if (!報告追記(psrptf, groups, group_count, reports, report_count)) {
        return MIPAY_DECISION_ERROR;
    }

    return warn ? MIPAY_DECISION_WARN : MIPAY_DECISION_OK;
}
