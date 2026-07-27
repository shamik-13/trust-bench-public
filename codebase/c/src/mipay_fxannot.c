/************************************************************
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20250120  精算基盤    外貨取引注記生成の初版作成
 ************************************************************/

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

#define 入力取引ファイル "PSTXNF.csv"
#define 入力為替ファイル "PSFXRF.csv"
#define 出力明細ファイル "PSDTLF.csv"

#define 行最大 1024
#define 項目最大 8
#define 通貨組最大 16
#define 日付最大 9
#define 店舗最大 32
#define 取引最大 40
#define 精算最大 32
#define 明細最大 40
#define 金額最大 18
#define 注記区分 "FX"
#define 有効ロード "0"

typedef struct {
    char txn_id[取引最大];
    char merchant_code[店舗最大];
    char txn_kbn;
    int64_t txn_amt;
    char txn_dt[日付最大];
} TxnRec;

typedef struct {
    char ccy_pair[通貨組最大];
    char rate_dt[日付最大];
    int64_t ttm_rate_micros;
    char source_cd[8];
    char load_status[8];
} FxRateRec;

static void chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t cap, const char *src)
{
    size_t n = strlen(src);
    if (cap == 0 || n >= cap) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static char *trim_ascii(char *s)
{
    char *end;
    while (*s != '\0' && isspace((unsigned char)*s)) {
        s++;
    }
    end = s + strlen(s);
    while (end > s && isspace((unsigned char)end[-1])) {
        *--end = '\0';
    }
    return s;
}

static int split_csv(char *line, char *fields[], size_t max_fields, size_t *count)
{
    size_t n = 0;
    char *p = line;

    while (*p != '\0') {
        if (n >= max_fields) {
            return -1;
        }

        if (*p == '"') {
            char *out = p;
            char *start = ++p;
            fields[n++] = out;
            while (*p != '\0') {
                if (*p == '"' && p[1] == '"') {
                    *out++ = '"';
                    p += 2;
                } else if (*p == '"') {
                    p++;
                    break;
                } else {
                    *out++ = *p++;
                }
            }
            *out = '\0';
            if (*p == ',') {
                p++;
            } else if (*p != '\0') {
                return -1;
            }
            (void)start;
        } else {
            fields[n++] = p;
            while (*p != '\0' && *p != ',') {
                p++;
            }
            if (*p == ',') {
                *p++ = '\0';
            }
        }
    }

    *count = n;
    return 0;
}

static int parse_amount(const char *s, int64_t *out)
{
    const unsigned char *p = (const unsigned char *)s;
    int neg = 0;
    int seen = 0;
    int64_t v = 0;

    if (*p == '-') {
        neg = 1;
        p++;
    }

    while (*p != '\0') {
        if (!isdigit(*p)) {
            return -1;
        }
        if (v > (INT64_MAX - (*p - '0')) / 10) {
            return -1;
        }
        v = v * 10 + (*p - '0');
        seen = 1;
        p++;
    }

    if (!seen) {
        return -1;
    }
    *out = neg ? -v : v;
    return 0;
}

static int parse_rate_micros(const char *s, int64_t *out)
{
    const unsigned char *p = (const unsigned char *)s;
    int64_t whole = 0;
    int64_t frac = 0;
    int scale = 0;
    int seen = 0;

    while (*p != '\0' && *p != '.') {
        if (!isdigit(*p)) {
            return -1;
        }
        if (whole > (INT64_MAX / 1000000 - (*p - '0')) / 10) {
            return -1;
        }
        whole = whole * 10 + (*p - '0');
        seen = 1;
        p++;
    }

    if (*p == '.') {
        p++;
        while (*p != '\0' && scale < 6) {
            if (!isdigit(*p)) {
                return -1;
            }
            frac = frac * 10 + (*p - '0');
            scale++;
            seen = 1;
            p++;
        }
        while (*p != '\0') {
            if (!isdigit(*p)) {
                return -1;
            }
            p++;
        }
    }

    while (scale++ < 6) {
        frac *= 10;
    }

    if (!seen) {
        return -1;
    }
    *out = whole * 1000000 + frac;
    return 0;
}

static int valid_yyyymmdd(const char *s)
{
    int i;
    if (strlen(s) != 8) {
        return 0;
    }
    for (i = 0; i < 8; i++) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
    }
    return 1;
}

static int parse_txn(char *line, TxnRec *rec)
{
    char *fields[項目最大];
    size_t n = 0;
    char *f0;
    char *f1;
    char *f2;
    char *f3;
    char *f4;

    chomp(line);
    if (split_csv(line, fields, 項目最大, &n) != 0 || n != 5) {
        return -1;
    }

    f0 = trim_ascii(fields[0]);
    f1 = trim_ascii(fields[1]);
    f2 = trim_ascii(fields[2]);
    f3 = trim_ascii(fields[3]);
    f4 = trim_ascii(fields[4]);

    if (copy_field(rec->txn_id, sizeof(rec->txn_id), f0) != 0 ||
        copy_field(rec->merchant_code, sizeof(rec->merchant_code), f1) != 0 ||
        copy_field(rec->txn_dt, sizeof(rec->txn_dt), f4) != 0) {
        return -1;
    }
    if ((strcmp(f2, "C") != 0 && strcmp(f2, "R") != 0) || !valid_yyyymmdd(rec->txn_dt)) {
        return -1;
    }
    if (parse_amount(f3, &rec->txn_amt) != 0) {
        return -1;
    }

    rec->txn_kbn = f2[0];
    return 0;
}

static int parse_fx(char *line, FxRateRec *rec)
{
    char *fields[項目最大];
    size_t n = 0;
    char *f0;
    char *f1;
    char *f2;
    char *f3;
    char *f4;

    chomp(line);
    if (split_csv(line, fields, 項目最大, &n) != 0 || n != 5) {
        return -1;
    }

    f0 = trim_ascii(fields[0]);
    f1 = trim_ascii(fields[1]);
    f2 = trim_ascii(fields[2]);
    f3 = trim_ascii(fields[3]);
    f4 = trim_ascii(fields[4]);

    if (copy_field(rec->ccy_pair, sizeof(rec->ccy_pair), f0) != 0 ||
        copy_field(rec->rate_dt, sizeof(rec->rate_dt), f1) != 0 ||
        copy_field(rec->source_cd, sizeof(rec->source_cd), f3) != 0 ||
        copy_field(rec->load_status, sizeof(rec->load_status), f4) != 0) {
        return -1;
    }
    if (!valid_yyyymmdd(rec->rate_dt) || parse_rate_micros(f2, &rec->ttm_rate_micros) != 0) {
        return -1;
    }
    if (rec->ttm_rate_micros <= 0) {
        return -1;
    }

    return 0;
}

static int is_header_line(const char *line, const char *first_name)
{
    char buf[行最大];
    char *fields[項目最大];
    size_t n = 0;

    if (strlen(line) >= sizeof(buf)) {
        return 0;
    }
    strcpy(buf, line);
    chomp(buf);
    if (split_csv(buf, fields, 項目最大, &n) != 0 || n == 0) {
        return 0;
    }
    return strcmp(trim_ascii(fields[0]), first_name) == 0;
}

static int extract_pair_from_txn(const TxnRec *txn, char *pair, size_t cap)
{
    static const char *ccys[] = { "USD", "EUR", "GBP", "AUD", "NZD", "CAD", "CHF", "HKD", "SGD", "CNH" };
    size_t i;

    for (i = 0; i < sizeof(ccys) / sizeof(ccys[0]); i++) {
        if (strstr(txn->txn_id, ccys[i]) != NULL || strstr(txn->merchant_code, ccys[i]) != NULL) {
            if (snprintf(pair, cap, "%sJPY", ccys[i]) >= (int)cap) {
                return -1;
            }
            return 1;
        }
    }
    return 0;
}

static int load_rates(FxRateRec *rates, size_t cap, size_t *count)
{
    FILE *fp;
    char line[行最大];
    size_t n = 0;
    unsigned long lineno = 0;

    fp = fopen(入力為替ファイル, "r");
    if (fp == NULL) {
        fprintf(stderr, "E001 為替参照ファイルを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        FxRateRec rec;
        lineno++;

        if (lineno == 1 && is_header_line(line, "CCY-PAIR")) {
            continue;
        }
        if (parse_fx(line, &rec) != 0) {
            fprintf(stderr, "E002 為替参照ファイルの形式不正 行=%lu\n", lineno);
            fclose(fp);
            return -1;
        }
        if (strcmp(rec.load_status, 有効ロード) != 0) {
            continue;
        }
        if (n >= cap) {
            fprintf(stderr, "E003 為替参照件数が上限を超過\n");
            fclose(fp);
            return -1;
        }
        rates[n++] = rec;
    }

    if (ferror(fp)) {
        fprintf(stderr, "E004 為替参照ファイルの読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static const FxRateRec *find_rate(const FxRateRec *rates, size_t count, const char *pair, const char *txn_dt)
{
    const FxRateRec *best = NULL;
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(rates[i].ccy_pair, pair) != 0) {
            continue;
        }
        if (strcmp(rates[i].rate_dt, txn_dt) > 0) {
            continue;
        }
        if (best == NULL || strcmp(rates[i].rate_dt, best->rate_dt) > 0) {
            best = &rates[i];
        }
    }
    return best;
}

static int build_settle_id(char *dst, size_t cap, const TxnRec *txn, const FxRateRec *rate)
{
    int n = snprintf(dst, cap, "FX%s%s", rate->ccy_pair, txn->txn_dt);
    return (n < 0 || (size_t)n >= cap) ? -1 : 0;
}

static int build_detail_id(char *dst, size_t cap, unsigned long seq)
{
    int n = snprintf(dst, cap, "FXN%012lu", seq);
    return (n < 0 || (size_t)n >= cap) ? -1 : 0;
}

int main(void)
{
    FxRateRec rates[4096];
    size_t rate_count = 0;
    FILE *in;
    FILE *out;
    char line[行最大];
    unsigned long lineno = 0;
    unsigned long detail_seq = 0;

    if (load_rates(rates, sizeof(rates) / sizeof(rates[0]), &rate_count) != 0) {
        return 20;
    }

    in = fopen(入力取引ファイル, "r");
    if (in == NULL) {
        fprintf(stderr, "E101 取引入力ファイルを開けません\n");
        return 21;
    }

    out = fopen(出力明細ファイル, "w");
    if (out == NULL) {
        fprintf(stderr, "E102 明細出力ファイルを開けません\n");
        fclose(in);
        return 22;
    }

    if (fprintf(out, "DETAIL-ID,SETTLE-ID,MERCHANT-CODE,TXN-ID,TXN-AMT,CHARGE-AMT,LINE-KBN\n") < 0) {
        fprintf(stderr, "E103 明細出力ファイルの書込失敗\n");
        fclose(out);
        fclose(in);
        return 23;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        TxnRec txn;
        char pair[通貨組最大];
        int pair_status;
        const FxRateRec *rate;
        char settle_id[精算最大];
        char detail_id[明細最大];

        lineno++;

        if (lineno == 1 && is_header_line(line, "TXN-ID")) {
            continue;
        }
        if (parse_txn(line, &txn) != 0) {
            fprintf(stderr, "E104 取引入力ファイルの形式不正 行=%lu\n", lineno);
            fclose(out);
            fclose(in);
            return 24;
        }
        if (txn.txn_kbn != 'C') {
            continue;
        }

        pair_status = extract_pair_from_txn(&txn, pair, sizeof(pair));
        if (pair_status < 0) {
            fprintf(stderr, "E105 通貨組の編集失敗 行=%lu\n", lineno);
            fclose(out);
            fclose(in);
            return 25;
        }
        if (pair_status == 0) {
            continue;
        }

        rate = find_rate(rates, rate_count, pair, txn.txn_dt);
        if (rate == NULL) {
            fprintf(stderr, "E106 有効な為替参照レートなし 行=%lu\n", lineno);
            fclose(out);
            fclose(in);
            return 26;
        }

        detail_seq++;
        if (build_detail_id(detail_id, sizeof(detail_id), detail_seq) != 0 ||
            build_settle_id(settle_id, sizeof(settle_id), &txn, rate) != 0) {
            fprintf(stderr, "E107 明細キーの編集失敗 行=%lu\n", lineno);
            fclose(out);
            fclose(in);
            return 27;
        }

        if (fprintf(out, "%s,%s,%s,%s,%lld,%lld,%s\n",
                    detail_id,
                    settle_id,
                    txn.merchant_code,
                    txn.txn_id,
                    (long long)txn.txn_amt,
                    (long long)rate->ttm_rate_micros,
                    注記区分) < 0) {
            fprintf(stderr, "E108 明細出力ファイルの書込失敗 行=%lu\n", lineno);
            fclose(out);
            fclose(in);
            return 28;
        }
    }

    if (ferror(in)) {
        fprintf(stderr, "E109 取引入力ファイルの読込失敗\n");
        fclose(out);
        fclose(in);
        return 29;
    }
    if (fclose(out) != 0) {
        fprintf(stderr, "E110 明細出力ファイルの終了失敗\n");
        fclose(in);
        return 30;
    }
    if (fclose(in) != 0) {
        fprintf(stderr, "E111 取引入力ファイルの終了失敗\n");
        return 31;
    }

    return MIPAY_DECISION_OK;
}
