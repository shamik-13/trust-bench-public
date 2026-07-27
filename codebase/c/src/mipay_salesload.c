/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20240603  MIYAZAKI   初版作成
 * 1.01  20241007  MIYAZAKI   締め日範囲判定と金額符号検査を追加
 * 1.02  20250317  MIYAZAKI   CSV項目長検査と整数あふれ検査を強化
 */

#include "mipay_settle.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define RC_OK 0
#define RC_WARN 4
#define RC_ABEND 8

#define MAX_LINE 512
#define MAX_FIELDS 8
#define MAX_KEY 32
#define MAX_VALUE 64
#define MAX_TXN_ID 32
#define MAX_MERCHANT 16
#define MAX_DATE 9
#define MAX_STATUS 3
#define MAX_MERCHANTS 4096
#define RATE_BP 30LL

typedef struct {
    char cutoff_from[MAX_DATE];
    char cutoff_to[MAX_DATE];
    int merchk_required;
} BatchConfig;

typedef struct {
    char code[MAX_MERCHANT];
    char status[MAX_STATUS];
} MerchantEntry;

typedef struct {
    MerchantEntry rows[MAX_MERCHANTS];
    size_t count;
} MerchantTable;

typedef struct {
    char txn_id[MAX_TXN_ID];
    char merchant_code[MAX_MERCHANT];
    char txn_kbn;
    long long txn_amt;
    char txn_dt[MAX_DATE];
} SalesRecord;

static void trim_field(char *s)
{
    size_t len;
    char *p;

    while (*s != '\0' && isspace((unsigned char)*s)) {
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
            if (*p == '"' && p[1] == '"') {
                memmove(p, p + 1, strlen(p));
            }
        }
    }
}

static int split_csv(char *line, char *fields[], size_t need)
{
    size_t n = 0;
    int quoted = 0;
    char *start = line;
    char *p;

    for (p = line; *p != '\0'; ++p) {
        if (*p == '"') {
            if (quoted && p[1] == '"') {
                ++p;
            } else {
                quoted = !quoted;
            }
        } else if (*p == ',' && !quoted) {
            if (n >= MAX_FIELDS) {
                return 0;
            }
            *p = '\0';
            fields[n++] = start;
            start = p + 1;
        } else if ((*p == '\n' || *p == '\r') && !quoted) {
            *p = '\0';
            break;
        }
    }

    if (quoted || n >= MAX_FIELDS) {
        return 0;
    }
    fields[n++] = start;

    if (n != need) {
        return 0;
    }

    for (size_t i = 0; i < n; ++i) {
        trim_field(fields[i]);
    }
    return 1;
}

static int copy_checked(char *dst, size_t dstsz, const char *src)
{
    size_t len = strlen(src);

    if (len == 0 || len >= dstsz) {
        return 0;
    }
    memcpy(dst, src, len + 1);
    return 1;
}

static int parse_int64(const char *s, long long *out)
{
    char *end;
    long long v;

    if (*s == '\0') {
        return 0;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno == ERANGE || *end != '\0') {
        return 0;
    }

    *out = v;
    return 1;
}

static int valid_yyyymmdd(const char *s)
{
    static const int mdays[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    int y;
    int m;
    int d;
    int leap;

    if (strlen(s) != 8) {
        return 0;
    }
    for (size_t i = 0; i < 8; ++i) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
    }

    y = (s[0] - '0') * 1000 + (s[1] - '0') * 100 + (s[2] - '0') * 10 + (s[3] - '0');
    m = (s[4] - '0') * 10 + (s[5] - '0');
    d = (s[6] - '0') * 10 + (s[7] - '0');

    if (y < 2000 || y > 2099 || m < 1 || m > 12 || d < 1) {
        return 0;
    }

    leap = (y % 400 == 0) || (y % 4 == 0 && y % 100 != 0);
    if (m == 2) {
        return d <= mdays[m - 1] + leap;
    }
    return d <= mdays[m - 1];
}

static int valid_code_text(const char *s)
{
    size_t len = strlen(s);

    if (len < 4 || len >= MAX_MERCHANT) {
        return 0;
    }
    for (size_t i = 0; i < len; ++i) {
        if (!isalnum((unsigned char)s[i]) && s[i] != '-' && s[i] != '_') {
            return 0;
        }
    }
    return 1;
}

static int date_in_range(const char *dt, const BatchConfig *cfg)
{
    return strcmp(cfg->cutoff_from, dt) <= 0 && strcmp(dt, cfg->cutoff_to) <= 0;
}

static int load_config(const char *path, BatchConfig *cfg)
{
    FILE *fp;
    char line[MAX_LINE];
    unsigned long lineno = 0;
    int have_from = 0;
    int have_to = 0;

    cfg->cutoff_from[0] = '\0';
    cfg->cutoff_to[0] = '\0';
    cfg->merchk_required = 1;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "PSCONFオープン失敗:%s\n", path);
        return 0;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[MAX_FIELDS];

        ++lineno;
        if (lineno == 1 && strncmp(line, "CONF-KEY", 8) == 0) {
            continue;
        }
        if (!split_csv(line, f, 5)) {
            fprintf(stderr, "PSCONF形式不正:%lu\n", lineno);
            fclose(fp);
            return 0;
        }

        if (strcmp(f[0], "SETTLE-FROM-DT") == 0) {
            if (!valid_yyyymmdd(f[1]) || !copy_checked(cfg->cutoff_from, sizeof cfg->cutoff_from, f[1])) {
                fprintf(stderr, "PSCONF開始日不正:%lu\n", lineno);
                fclose(fp);
                return 0;
            }
            have_from = 1;
        } else if (strcmp(f[0], "SETTLE-TO-DT") == 0) {
            if (!valid_yyyymmdd(f[1]) || !copy_checked(cfg->cutoff_to, sizeof cfg->cutoff_to, f[1])) {
                fprintf(stderr, "PSCONF終了日不正:%lu\n", lineno);
                fclose(fp);
                return 0;
            }
            have_to = 1;
        } else if (strcmp(f[0], "MERCHK-REQ") == 0) {
            if (strcmp(f[1], "0") == 0) {
                cfg->merchk_required = 0;
            } else if (strcmp(f[1], "1") == 0) {
                cfg->merchk_required = 1;
            } else {
                fprintf(stderr, "PSCONF加盟店検査区分不正:%lu\n", lineno);
                fclose(fp);
                return 0;
            }
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "PSCONF読込失敗:%s\n", path);
        fclose(fp);
        return 0;
    }
    fclose(fp);

    if (!have_from || !have_to || strcmp(cfg->cutoff_from, cfg->cutoff_to) > 0) {
        fprintf(stderr, "PSCONF締め日キー不正\n");
        return 0;
    }
    return 1;
}

static int load_merchants(const char *path, MerchantTable *table)
{
    FILE *fp;
    char line[MAX_LINE];
    unsigned long lineno = 0;

    table->count = 0;
    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "加盟店ファイルオープン失敗:%s\n", path);
        return 0;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[MAX_FIELDS];

        ++lineno;
        if (lineno == 1 && strncmp(line, "MERCHANT-CODE", 13) == 0) {
            continue;
        }
        if (!split_csv(line, f, 2)) {
            fprintf(stderr, "加盟店ファイル形式不正:%lu\n", lineno);
            fclose(fp);
            return 0;
        }
        if (table->count >= MAX_MERCHANTS) {
            fprintf(stderr, "加盟店件数上限超過\n");
            fclose(fp);
            return 0;
        }
        if (!valid_code_text(f[0]) || strlen(f[1]) != 2 || !isdigit((unsigned char)f[1][0]) || !isdigit((unsigned char)f[1][1])) {
            fprintf(stderr, "加盟店ファイル項目不正:%lu\n", lineno);
            fclose(fp);
            return 0;
        }

        copy_checked(table->rows[table->count].code, sizeof table->rows[table->count].code, f[0]);
        copy_checked(table->rows[table->count].status, sizeof table->rows[table->count].status, f[1]);
        ++table->count;
    }

    if (ferror(fp)) {
        fprintf(stderr, "加盟店ファイル読込失敗:%s\n", path);
        fclose(fp);
        return 0;
    }
    fclose(fp);
    return 1;
}

static int merchant_settleable(const MerchantTable *table, const char *code)
{
    for (size_t i = 0; i < table->count; ++i) {
        if (strcmp(table->rows[i].code, code) == 0) {
            return strcmp(table->rows[i].status, "01") == 0;
        }
    }
    return 0;
}

static int parse_sales(char *line, unsigned long lineno, SalesRecord *rec)
{
    char *f[MAX_FIELDS];
    long long amt;

    if (!split_csv(line, f, 5)) {
        fprintf(stderr, "売上確定形式不正:%lu\n", lineno);
        return 0;
    }

    if (!copy_checked(rec->txn_id, sizeof rec->txn_id, f[0])) {
        fprintf(stderr, "取引ID不正:%lu\n", lineno);
        return 0;
    }
    if (!valid_code_text(f[1]) || !copy_checked(rec->merchant_code, sizeof rec->merchant_code, f[1])) {
        fprintf(stderr, "加盟店コード不正:%lu\n", lineno);
        return 0;
    }
    if (strlen(f[2]) != 1 || (f[2][0] != 'C' && f[2][0] != 'R')) {
        fprintf(stderr, "取引区分不正:%lu\n", lineno);
        return 0;
    }
    if (!parse_int64(f[3], &amt) || amt == 0) {
        fprintf(stderr, "取引金額不正:%lu\n", lineno);
        return 0;
    }
    if ((f[2][0] == 'C' && amt < 0) || (f[2][0] == 'R' && amt > 0)) {
        fprintf(stderr, "取引金額符号不正:%lu\n", lineno);
        return 0;
    }
    if (llabs(amt) > LLONG_MAX / RATE_BP) {
        fprintf(stderr, "手数料計算あふれ:%lu\n", lineno);
        return 0;
    }
    if (!valid_yyyymmdd(f[4]) || !copy_checked(rec->txn_dt, sizeof rec->txn_dt, f[4])) {
        fprintf(stderr, "取引日不正:%lu\n", lineno);
        return 0;
    }

    rec->txn_kbn = f[2][0];
    rec->txn_amt = amt;
    return 1;
}

static int write_pstxnf(FILE *out, const SalesRecord *rec)
{
    long long charge;
    long long net_amt;

    charge = (llabs(rec->txn_amt) * RATE_BP + 9999LL) / 10000LL;
    if (rec->txn_amt > 0) {
        net_amt = rec->txn_amt - charge;
    } else {
        net_amt = rec->txn_amt + charge;
    }

    if (fprintf(out, "%s,%s,%c,%lld,%s\n",
                rec->txn_id,
                rec->merchant_code,
                rec->txn_kbn,
                net_amt,
                rec->txn_dt) < 0) {
        return 0;
    }
    return 1;
}

int main(void)
{
    const char *conf_path = getenv("MIPAY_PSCONF");
    const char *sales_path = getenv("MIPAY_SALESLOAD");
    const char *merchant_path = getenv("MIPAY_MERCHANT");
    const char *out_path = getenv("MIPAY_PSTXNF");
    BatchConfig cfg;
    MerchantTable merchants;
    FILE *in;
    FILE *out;
    char line[MAX_LINE];
    unsigned long lineno = 0;
    unsigned long written = 0;
    unsigned long held = 0;
    unsigned long rejected = 0;

    if (conf_path == NULL || *conf_path == '\0') {
        conf_path = "psconf.csv";
    }
    if (sales_path == NULL || *sales_path == '\0') {
        sales_path = "salesload.csv";
    }
    if (merchant_path == NULL || *merchant_path == '\0') {
        merchant_path = "merchant.csv";
    }
    if (out_path == NULL || *out_path == '\0') {
        out_path = "pstxnf.csv";
    }

    if (!load_config(conf_path, &cfg)) {
        return RC_ABEND;
    }

    merchants.count = 0;
    if (cfg.merchk_required && !load_merchants(merchant_path, &merchants)) {
        return RC_ABEND;
    }

    in = fopen(sales_path, "r");
    if (in == NULL) {
        fprintf(stderr, "売上確定元ファイルオープン失敗:%s\n", sales_path);
        return RC_ABEND;
    }

    out = fopen(out_path, "w");
    if (out == NULL) {
        fprintf(stderr, "PSTXNFオープン失敗:%s\n", out_path);
        fclose(in);
        return RC_ABEND;
    }

    if (fprintf(out, "TXN-ID,MERCHANT-CODE,TXN-KBN,TXN-AMT,TXN-DT\n") < 0) {
        fprintf(stderr, "PSTXNFヘッダ出力失敗\n");
        fclose(out);
        fclose(in);
        return RC_ABEND;
    }

    while (fgets(line, sizeof line, in) != NULL) {
        SalesRecord rec;

        ++lineno;
        if (lineno == 1 && strncmp(line, "TXN-ID", 6) == 0) {
            continue;
        }

        if (!parse_sales(line, lineno, &rec)) {
            ++rejected;
            continue;
        }

        if (!date_in_range(rec.txn_dt, &cfg)) {
            ++held;
            continue;
        }

        if (cfg.merchk_required && !merchant_settleable(&merchants, rec.merchant_code)) {
            fprintf(stderr, "加盟店精算不可:%lu:%s\n", lineno, rec.merchant_code);
            ++rejected;
            continue;
        }

        if (!write_pstxnf(out, &rec)) {
            fprintf(stderr, "PSTXNF明細出力失敗:%lu\n", lineno);
            fclose(out);
            fclose(in);
            return RC_ABEND;
        }
        ++written;
    }

    if (ferror(in)) {
        fprintf(stderr, "売上確定元ファイル読込失敗:%s\n", sales_path);
        fclose(out);
        fclose(in);
        return RC_ABEND;
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "PSTXNFクローズ失敗:%s\n", out_path);
        fclose(in);
        return RC_ABEND;
    }
    fclose(in);

    fprintf(stderr, "処理件数:%lu 出力件数:%lu 保留件数:%lu 棄却件数:%lu\n",
            lineno, written, held, rejected);

    if (rejected > 0) {
        return RC_WARN;
    }
    return RC_OK;
}
