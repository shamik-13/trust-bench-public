/* 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240320  精算連携  初版作成
 * 1.01  20240701  精算連携  金額桁あふれ検査と再送区分判定を追加
 * 1.02  20241015  精算連携  銀行参照番号採番と口座状態検査を分離
 */

#include "mipay_trace.h"

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

#ifndef MIPAY_DECISION_RETRY
#define MIPAY_DECISION_RETRY 10
#endif

#ifndef MIPAY_DECISION_IO_ERROR
#define MIPAY_DECISION_IO_ERROR 40
#endif

#ifndef MIPAY_DECISION_PARSE_ERROR
#define MIPAY_DECISION_PARSE_ERROR 41
#endif

#define 入力通知ファイル "PJNTCF.csv"
#define 入力加盟店ファイル "PJMSTF.csv"
#define 出力通知ファイル "PJNTCF.out.csv"

enum {
    行最大長 = 1024,
    項目最大数 = 8,
    通知番号長 = 32,
    加盟店コード長 = 32,
    決済日長 = 16,
    銀行参照番号長 = 48,
    通知状態長 = 16,
    加盟店名長 = 96,
    銀行コード長 = 8,
    口座番号長 = 32,
    有効区分長 = 8,
    リスクランク長 = 8,
    最大通知件数 = 20000,
    最大加盟店件数 = 20000
};

typedef struct {
    char notice_id[通知番号長];
    char merchant_code[加盟店コード長];
    char settle_date[決済日長];
    long long payment_amt;
    char bank_ref_no[銀行参照番号長];
    char notice_status[通知状態長];
} 通知行;

typedef struct {
    char merchant_code[加盟店コード長];
    char merchant_name[加盟店名長];
    char bank_code[銀行コード長];
    char account_no[口座番号長];
    char active_flag[有効区分長];
    char risk_rank[リスクランク長];
} 加盟店行;

static void 改行除去(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static char *空白除去(char *s)
{
    unsigned char *p = (unsigned char *)s;
    char *末尾;

    while (*p != '\0' && isspace(*p)) {
        ++p;
    }

    s = (char *)p;
    末尾 = s + strlen(s);
    while (末尾 > s && isspace((unsigned char)末尾[-1])) {
        *--末尾 = '\0';
    }
    return s;
}

static int 複写項目(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);
    if (dstsz == 0 || n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int csv分割(char *line, char *fields[], int max_fields)
{
    int count = 0;
    char *p = line;

    while (*p != '\0' && count < max_fields) {
        char *start = p;
        int 引用中 = 0;
        char *out = p;

        while (*p != '\0') {
            if (*p == '"') {
                if (引用中 && p[1] == '"') {
                    *out++ = '"';
                    p += 2;
                    continue;
                }
                引用中 = !引用中;
                ++p;
                continue;
            }
            if (!引用中 && *p == ',') {
                ++p;
                break;
            }
            *out++ = *p++;
        }

        *out = '\0';
        fields[count++] = 空白除去(start);

        if (引用中) {
            return -1;
        }
    }

    if (*p != '\0') {
        return -1;
    }
    return count;
}

static int 金額解析(const char *s, long long *out)
{
    long long v = 0;
    const unsigned char *p = (const unsigned char *)s;

    if (*p == '\0') {
        return -1;
    }

    while (*p != '\0') {
        int d;
        if (!isdigit(*p)) {
            return -1;
        }
        d = *p - '0';
        if (v > (LLONG_MAX - d) / 10) {
            return -1;
        }
        v = v * 10 + d;
        ++p;
    }

    *out = v;
    return 0;
}

static int 日付妥当(const char *s)
{
    int y, m, d;
    int mdays[] = { 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

    if (strlen(s) != 8) {
        return 0;
    }
    for (int i = 0; i < 8; ++i) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
    }

    y = (s[0] - '0') * 1000 + (s[1] - '0') * 100 + (s[2] - '0') * 10 + (s[3] - '0');
    m = (s[4] - '0') * 10 + (s[5] - '0');
    d = (s[6] - '0') * 10 + (s[7] - '0');

    if (y < 2000 || y > 2099 || m < 1 || m > 12) {
        return 0;
    }
    if ((y % 400 == 0) || (y % 4 == 0 && y % 100 != 0)) {
        mdays[2] = 29;
    }
    return d >= 1 && d <= mdays[m];
}

static int 通知読込(FILE *fp, 通知行 *rows, size_t *count)
{
    char line[行最大長];
    size_t n = 0;
    long 行番号 = 0;

    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[項目最大数];
        int fc;

        ++行番号;
        改行除去(line);
        if (line[0] == '\0') {
            continue;
        }

        fc = csv分割(line, f, 項目最大数);
        if (fc < 0 || fc != 6 || n >= 最大通知件数) {
            fprintf(stderr, "通知CSV形式異常 行=%ld\n", 行番号);
            return -1;
        }

        if (複写項目(rows[n].notice_id, sizeof rows[n].notice_id, f[0]) != 0 ||
            複写項目(rows[n].merchant_code, sizeof rows[n].merchant_code, f[1]) != 0 ||
            複写項目(rows[n].settle_date, sizeof rows[n].settle_date, f[2]) != 0 ||
            複写項目(rows[n].bank_ref_no, sizeof rows[n].bank_ref_no, f[4]) != 0 ||
            複写項目(rows[n].notice_status, sizeof rows[n].notice_status, f[5]) != 0 ||
            金額解析(f[3], &rows[n].payment_amt) != 0 ||
            !日付妥当(rows[n].settle_date)) {
            fprintf(stderr, "通知CSV値異常 行=%ld\n", 行番号);
            return -1;
        }

        ++n;
    }

    if (ferror(fp)) {
        fprintf(stderr, "通知CSV読込異常 errno=%d\n", errno);
        return -1;
    }

    *count = n;
    return 0;
}

static int 加盟店読込(FILE *fp, 加盟店行 *rows, size_t *count)
{
    char line[行最大長];
    size_t n = 0;
    long 行番号 = 0;

    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[項目最大数];
        int fc;

        ++行番号;
        改行除去(line);
        if (line[0] == '\0') {
            continue;
        }

        fc = csv分割(line, f, 項目最大数);
        if (fc < 0 || fc != 6 || n >= 最大加盟店件数) {
            fprintf(stderr, "加盟店CSV形式異常 行=%ld\n", 行番号);
            return -1;
        }

        if (複写項目(rows[n].merchant_code, sizeof rows[n].merchant_code, f[0]) != 0 ||
            複写項目(rows[n].merchant_name, sizeof rows[n].merchant_name, f[1]) != 0 ||
            複写項目(rows[n].bank_code, sizeof rows[n].bank_code, f[2]) != 0 ||
            複写項目(rows[n].account_no, sizeof rows[n].account_no, f[3]) != 0 ||
            複写項目(rows[n].active_flag, sizeof rows[n].active_flag, f[4]) != 0 ||
            複写項目(rows[n].risk_rank, sizeof rows[n].risk_rank, f[5]) != 0) {
            fprintf(stderr, "加盟店CSV値異常 行=%ld\n", 行番号);
            return -1;
        }

        ++n;
    }

    if (ferror(fp)) {
        fprintf(stderr, "加盟店CSV読込異常 errno=%d\n", errno);
        return -1;
    }

    *count = n;
    return 0;
}

static const 加盟店行 *加盟店検索(const 加盟店行 *rows, size_t count, const char *merchant_code)
{
    for (size_t i = 0; i < count; ++i) {
        if (strcmp(rows[i].merchant_code, merchant_code) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static int 数字固定長(const char *s, size_t n)
{
    if (strlen(s) != n) {
        return 0;
    }
    for (size_t i = 0; i < n; ++i) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
    }
    return 1;
}

static int 送信対象判定(const 通知行 *notice, const 加盟店行 *merchant)
{
    if (strcmp(notice->notice_status, "UNSENT") != 0 &&
        strcmp(notice->notice_status, "RETRY") != 0) {
        return 0;
    }
    if (merchant == NULL) {
        return 0;
    }
    if (strcmp(merchant->active_flag, "1") != 0 &&
        strcmp(merchant->active_flag, "Y") != 0) {
        return 0;
    }
    if (!数字固定長(merchant->bank_code, 4) || strlen(merchant->account_no) < 7) {
        return 0;
    }
    if (notice->payment_amt <= 0 || notice->payment_amt > 999999999999LL) {
        return 0;
    }
    if (strcmp(merchant->risk_rank, "D") == 0 && notice->payment_amt > 30000000LL) {
        return 0;
    }
    return 1;
}

static int 銀行参照番号作成(char *dst, size_t dstsz, const 通知行 *notice, const 加盟店行 *merchant)
{
    int n = snprintf(dst, dstsz, "JP%s%s%012lld",
                     merchant->bank_code,
                     notice->settle_date,
                     notice->payment_amt % 1000000000000LL);
    return n > 0 && (size_t)n < dstsz ? 0 : -1;
}

static int 通知出力(FILE *fp, const 通知行 *rows, size_t count)
{
    for (size_t i = 0; i < count; ++i) {
        if (fprintf(fp, "%s,%s,%s,%lld,%s,%s\n",
                    rows[i].notice_id,
                    rows[i].merchant_code,
                    rows[i].settle_date,
                    rows[i].payment_amt,
                    rows[i].bank_ref_no,
                    rows[i].notice_status) < 0) {
            fprintf(stderr, "通知CSV書込異常\n");
            return -1;
        }
    }
    return 0;
}

int main(void)
{
    FILE *通知fp;
    FILE *加盟店fp;
    FILE *出力fp;
    通知行 *通知;
    加盟店行 *加盟店;
    size_t 通知件数 = 0;
    size_t 加盟店件数 = 0;
    int 再送あり = 0;

    通知 = calloc(最大通知件数, sizeof *通知);
    加盟店 = calloc(最大加盟店件数, sizeof *加盟店);
    if (通知 == NULL || 加盟店 == NULL) {
        fprintf(stderr, "作業領域確保異常\n");
        free(通知);
        free(加盟店);
        return MIPAY_DECISION_IO_ERROR;
    }

    通知fp = fopen(入力通知ファイル, "r");
    if (通知fp == NULL) {
        fprintf(stderr, "通知CSVオープン異常 errno=%d\n", errno);
        free(通知);
        free(加盟店);
        return MIPAY_DECISION_IO_ERROR;
    }

    if (通知読込(通知fp, 通知, &通知件数) != 0) {
        fclose(通知fp);
        free(通知);
        free(加盟店);
        return MIPAY_DECISION_PARSE_ERROR;
    }
    fclose(通知fp);

    加盟店fp = fopen(入力加盟店ファイル, "r");
    if (加盟店fp == NULL) {
        fprintf(stderr, "加盟店CSVオープン異常 errno=%d\n", errno);
        free(通知);
        free(加盟店);
        return MIPAY_DECISION_IO_ERROR;
    }

    if (加盟店読込(加盟店fp, 加盟店, &加盟店件数) != 0) {
        fclose(加盟店fp);
        free(通知);
        free(加盟店);
        return MIPAY_DECISION_PARSE_ERROR;
    }
    fclose(加盟店fp);

    for (size_t i = 0; i < 通知件数; ++i) {
        const 加盟店行 *m = 加盟店検索(加盟店, 加盟店件数, 通知[i].merchant_code);

        if (送信対象判定(&通知[i], m)) {
            if (銀行参照番号作成(通知[i].bank_ref_no, sizeof 通知[i].bank_ref_no, &通知[i], m) != 0 ||
                複写項目(通知[i].notice_status, sizeof 通知[i].notice_status, "SENT") != 0) {
                fprintf(stderr, "通知更新領域異常 通知ID=%s\n", 通知[i].notice_id);
                free(通知);
                free(加盟店);
                return MIPAY_DECISION_PARSE_ERROR;
            }
        } else if (strcmp(通知[i].notice_status, "UNSENT") == 0 ||
                   strcmp(通知[i].notice_status, "RETRY") == 0) {
            通知[i].bank_ref_no[0] = '\0';
            if (複写項目(通知[i].notice_status, sizeof 通知[i].notice_status, "RETRY") != 0) {
                fprintf(stderr, "再送区分設定異常 通知ID=%s\n", 通知[i].notice_id);
                free(通知);
                free(加盟店);
                return MIPAY_DECISION_PARSE_ERROR;
            }
            再送あり = 1;
        }
    }

    出力fp = fopen(出力通知ファイル, "w");
    if (出力fp == NULL) {
        fprintf(stderr, "通知CSV出力オープン異常 errno=%d\n", errno);
        free(通知);
        free(加盟店);
        return MIPAY_DECISION_IO_ERROR;
    }

    if (通知出力(出力fp, 通知, 通知件数) != 0 || fclose(出力fp) != 0) {
        fprintf(stderr, "通知CSV出力確定異常 errno=%d\n", errno);
        free(通知);
        free(加盟店);
        return MIPAY_DECISION_IO_ERROR;
    }

    free(通知);
    free(加盟店);
    return 再送あり ? MIPAY_DECISION_RETRY : MIPAY_DECISION_OK;
}
