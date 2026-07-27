/*
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    20240520    精算連携    初版作成
 * 1.01    20240918    精算連携    既存繰越行の統合処理を追加
 * 1.02    20250122    精算連携    CSV境界検査と金額桁あふれ検査を追加
 */

#include "mipay_trace.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MIPAY_MIN_PAYMENT_AMOUNT
#define MIPAY_MIN_PAYMENT_AMOUNT 10000LL
#endif

#ifndef MIPAY_DECISION_CARRY_FORWARD
#define MIPAY_DECISION_CARRY_FORWARD 0
#endif

#ifndef MIPAY_DECISION_IO_ERROR
#define MIPAY_DECISION_IO_ERROR 12
#endif

#ifndef MIPAY_DECISION_PARSE_ERROR
#define MIPAY_DECISION_PARSE_ERROR 16
#endif

#define 入力_PCSUMF "pcsumf.csv"
#define 入力_PCCARF "pccarf.csv"
#define 入力_PJMSTF  "pjmstf.csv"
#define 出力_PCSUMF "pcsumf.out.csv"
#define 出力_PCCARF "pccarf.out.csv"

#define 行最大 1024
#define 件数最大 4096
#define 理由長 64
#define 日付長 9
#define 店舗長 32
#define 区分長 8
#define 名称長 96
#define 銀行長 16
#define 口座長 32
#define 旗長 4
#define ランク長 4
#define 番号長 48

typedef struct {
    char 加盟店コード[店舗長];
    char 精算日[日付長];
    char 精算区分[区分長];
    long long 取引件数;
    long long 合計額;
    long long 繰越額;
} 精算行;

typedef struct {
    char 繰越ID[番号長];
    char 加盟店コード[店舗長];
    char 精算区分[区分長];
    long long 繰越額;
    char 理由[理由長];
    char 次回精算日[日付長];
} 繰越行;

typedef struct {
    char 加盟店コード[店舗長];
    char 加盟店名[名称長];
    char 銀行コード[銀行長];
    char 口座番号[口座長];
    char 有効フラグ[旗長];
    char リスクランク[ランク長];
} 加盟店行;

static int 文字列写し(char *宛先, size_t 宛先長, const char *元)
{
    size_t 長さ;

    if (宛先長 == 0U || 元 == NULL) {
        return -1;
    }

    長さ = strlen(元);
    if (長さ >= 宛先長) {
        return -1;
    }

    memcpy(宛先, 元, 長さ + 1U);
    return 0;
}

static void 改行除去(char *行)
{
    size_t 長さ = strlen(行);

    while (長さ > 0U && (行[長さ - 1U] == '\n' || 行[長さ - 1U] == '\r')) {
        行[--長さ] = '\0';
    }
}

static int 次項目(char **位置, char *宛先, size_t 宛先長)
{
    char *開始;
    char *区切り;
    size_t 長さ;

    if (位置 == NULL || *位置 == NULL || 宛先 == NULL || 宛先長 == 0U) {
        return -1;
    }

    開始 = *位置;
    区切り = strchr(開始, ',');
    if (区切り == NULL) {
        長さ = strlen(開始);
        *位置 = NULL;
    } else {
        長さ = (size_t)(区切り - 開始);
        *位置 = 区切り + 1;
    }

    if (長さ >= 宛先長) {
        return -1;
    }

    memcpy(宛先, 開始, 長さ);
    宛先[長さ] = '\0';
    return 0;
}

static int 金額読取(const char *文字列, long long *値)
{
    char *終端;
    long long 読取値;

    if (文字列 == NULL || *文字列 == '\0' || 値 == NULL) {
        return -1;
    }

    errno = 0;
    読取値 = strtoll(文字列, &終端, 10);
    if (errno == ERANGE || *終端 != '\0' || 読取値 < 0) {
        return -1;
    }

    *値 = 読取値;
    return 0;
}

static int 加算検査(long long 左, long long 右, long long *結果)
{
    if (結果 == NULL || 左 < 0 || 右 < 0 || 左 > LLONG_MAX - 右) {
        return -1;
    }

    *結果 = 左 + 右;
    return 0;
}

static int 精算行読取(const char *行, 精算行 *出力)
{
    char 作業[行最大];
    char *位置;
    char 件数[32];
    char 合計[32];
    char 繰越[32];

    if (文字列写し(作業, sizeof(作業), 行) != 0) {
        return -1;
    }

    位置 = 作業;
    if (次項目(&位置, 出力->加盟店コード, sizeof(出力->加盟店コード)) != 0 ||
        次項目(&位置, 出力->精算日, sizeof(出力->精算日)) != 0 ||
        次項目(&位置, 出力->精算区分, sizeof(出力->精算区分)) != 0 ||
        次項目(&位置, 件数, sizeof(件数)) != 0 ||
        次項目(&位置, 合計, sizeof(合計)) != 0 ||
        次項目(&位置, 繰越, sizeof(繰越)) != 0 ||
        位置 != NULL) {
        return -1;
    }

    if (金額読取(件数, &出力->取引件数) != 0 ||
        金額読取(合計, &出力->合計額) != 0 ||
        金額読取(繰越, &出力->繰越額) != 0) {
        return -1;
    }

    return 0;
}

static int 繰越行読取(const char *行, 繰越行 *出力)
{
    char 作業[行最大];
    char *位置;
    char 金額[32];

    if (文字列写し(作業, sizeof(作業), 行) != 0) {
        return -1;
    }

    位置 = 作業;
    if (次項目(&位置, 出力->繰越ID, sizeof(出力->繰越ID)) != 0 ||
        次項目(&位置, 出力->加盟店コード, sizeof(出力->加盟店コード)) != 0 ||
        次項目(&位置, 出力->精算区分, sizeof(出力->精算区分)) != 0 ||
        次項目(&位置, 金額, sizeof(金額)) != 0 ||
        次項目(&位置, 出力->理由, sizeof(出力->理由)) != 0 ||
        次項目(&位置, 出力->次回精算日, sizeof(出力->次回精算日)) != 0 ||
        位置 != NULL) {
        return -1;
    }

    return 金額読取(金額, &出力->繰越額);
}

static int 加盟店行読取(const char *行, 加盟店行 *出力)
{
    char 作業[行最大];
    char *位置;

    if (文字列写し(作業, sizeof(作業), 行) != 0) {
        return -1;
    }

    位置 = 作業;
    if (次項目(&位置, 出力->加盟店コード, sizeof(出力->加盟店コード)) != 0 ||
        次項目(&位置, 出力->加盟店名, sizeof(出力->加盟店名)) != 0 ||
        次項目(&位置, 出力->銀行コード, sizeof(出力->銀行コード)) != 0 ||
        次項目(&位置, 出力->口座番号, sizeof(出力->口座番号)) != 0 ||
        次項目(&位置, 出力->有効フラグ, sizeof(出力->有効フラグ)) != 0 ||
        次項目(&位置, 出力->リスクランク, sizeof(出力->リスクランク)) != 0 ||
        位置 != NULL) {
        return -1;
    }

    return 0;
}

static int 精算一覧読込(精算行 *一覧, size_t *件数)
{
    FILE *fp;
    char 行[行最大];
    size_t n = 0U;

    fp = fopen(入力_PCSUMF, "r");
    if (fp == NULL) {
        return -2;
    }

    while (fgets(行, sizeof(行), fp) != NULL) {
        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }
        if (n >= 件数最大 || 精算行読取(行, &一覧[n]) != 0) {
            fclose(fp);
            return -1;
        }
        ++n;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -2;
    }

    fclose(fp);
    *件数 = n;
    return 0;
}

static int 繰越一覧読込(繰越行 *一覧, size_t *件数)
{
    FILE *fp;
    char 行[行最大];
    size_t n = 0U;

    fp = fopen(入力_PCCARF, "r");
    if (fp == NULL) {
        *件数 = 0U;
        return 0;
    }

    while (fgets(行, sizeof(行), fp) != NULL) {
        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }
        if (n >= 件数最大 || 繰越行読取(行, &一覧[n]) != 0) {
            fclose(fp);
            return -1;
        }
        ++n;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -2;
    }

    fclose(fp);
    *件数 = n;
    return 0;
}

static int 加盟店一覧読込(加盟店行 *一覧, size_t *件数)
{
    FILE *fp;
    char 行[行最大];
    size_t n = 0U;

    fp = fopen(入力_PJMSTF, "r");
    if (fp == NULL) {
        return -2;
    }

    while (fgets(行, sizeof(行), fp) != NULL) {
        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }
        if (n >= 件数最大 || 加盟店行読取(行, &一覧[n]) != 0) {
            fclose(fp);
            return -1;
        }
        ++n;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -2;
    }

    fclose(fp);
    *件数 = n;
    return 0;
}

static const 加盟店行 *加盟店検索(const 加盟店行 *一覧, size_t 件数, const char *加盟店コード)
{
    size_t i;

    for (i = 0U; i < 件数; ++i) {
        if (strcmp(一覧[i].加盟店コード, 加盟店コード) == 0) {
            return &一覧[i];
        }
    }

    return NULL;
}

static 繰越行 *繰越検索(繰越行 *一覧, size_t 件数, const char *加盟店コード, const char *精算区分)
{
    size_t i;

    for (i = 0U; i < 件数; ++i) {
        if (strcmp(一覧[i].加盟店コード, 加盟店コード) == 0 &&
            strcmp(一覧[i].精算区分, 精算区分) == 0) {
            return &一覧[i];
        }
    }

    return NULL;
}

static int 口座無効(const 加盟店行 *加盟店)
{
    if (加盟店 == NULL) {
        return 1;
    }

    if (strcmp(加盟店->有効フラグ, "1") != 0) {
        return 1;
    }

    if (加盟店->銀行コード[0] == '\0' || 加盟店->口座番号[0] == '\0') {
        return 1;
    }

    return 0;
}

static int 理由作成(const 精算行 *精算, const 加盟店行 *加盟店, char *理由, size_t 理由容量)
{
    const char *判定;

    if (口座無効(加盟店)) {
        判定 = "BANK_NG";
    } else if (精算->合計額 + 精算->繰越額 < MIPAY_MIN_PAYMENT_AMOUNT) {
        判定 = "MIN_PAY";
    } else {
        判定 = "NONE";
    }

    return 文字列写し(理由, 理由容量, 判定);
}

static int 理由統合(char *既存, size_t 容量, const char *追加)
{
    size_t 既存長;
    size_t 追加長;

    if (strcmp(既存, 追加) == 0 || strcmp(追加, "NONE") == 0) {
        return 0;
    }

    if (strcmp(既存, "NONE") == 0 || 既存[0] == '\0') {
        return 文字列写し(既存, 容量, 追加);
    }

    既存長 = strlen(既存);
    追加長 = strlen(追加);
    if (既存長 + 1U + 追加長 >= 容量) {
        return -1;
    }

    既存[既存長] = '+';
    memcpy(既存 + 既存長 + 1U, 追加, 追加長 + 1U);
    return 0;
}

static int 次回精算日(const char *精算日, char *出力, size_t 出力長)
{
    char 年[5];
    char 月[3];
    char 日[3];
    int y;
    int m;
    int d;

    if (strlen(精算日) != 8U || 出力長 < 日付長) {
        return -1;
    }

    memcpy(年, 精算日, 4U);
    年[4] = '\0';
    memcpy(月, 精算日 + 4, 2U);
    月[2] = '\0';
    memcpy(日, 精算日 + 6, 2U);
    日[2] = '\0';

    y = atoi(年);
    m = atoi(月);
    d = atoi(日);
    if (y < 2000 || m < 1 || m > 12 || d < 1 || d > 31) {
        return -1;
    }

    if (d <= 15) {
        d = 25;
    } else {
        d = 10;
        ++m;
        if (m > 12) {
            m = 1;
            ++y;
        }
    }

    if (snprintf(出力, 出力長, "%04d%02d%02d", y, m, d) >= (int)出力長) {
        return -1;
    }

    return 0;
}

static int 繰越ID作成(char *出力, size_t 出力長, const char *加盟店コード, const char *精算区分, const char *次回日)
{
    if (snprintf(出力, 出力長, "CF%s%s%s", 加盟店コード, 精算区分, 次回日) >= (int)出力長) {
        return -1;
    }

    return 0;
}

static int 繰越反映(精算行 *精算一覧, size_t 精算件数,
                    繰越行 *繰越一覧, size_t *繰越件数,
                    const 加盟店行 *加盟店一覧, size_t 加盟店件数)
{
    size_t i;

    for (i = 0U; i < 精算件数; ++i) {
        精算行 *精算 = &精算一覧[i];
        const 加盟店行 *加盟店 = 加盟店検索(加盟店一覧, 加盟店件数, 精算->加盟店コード);
        long long 判定額;
        char 理由[理由長];
        char 次回日[日付長];

        if (加算検査(精算->合計額, 精算->繰越額, &判定額) != 0 ||
            理由作成(精算, 加盟店, 理由, sizeof(理由)) != 0 ||
            次回精算日(精算->精算日, 次回日, sizeof(次回日)) != 0) {
            return -1;
        }

        if (判定額 >= MIPAY_MIN_PAYMENT_AMOUNT && !口座無効(加盟店)) {
            continue;
        }

        {
            繰越行 *既存 = 繰越検索(繰越一覧, *繰越件数, 精算->加盟店コード, 精算->精算区分);
            if (既存 != NULL) {
                if (加算検査(既存->繰越額, 判定額, &既存->繰越額) != 0 ||
                    理由統合(既存->理由, sizeof(既存->理由), 理由) != 0 ||
                    文字列写し(既存->次回精算日, sizeof(既存->次回精算日), 次回日) != 0) {
                    return -1;
                }
            } else {
                if (*繰越件数 >= 件数最大) {
                    return -1;
                }
                既存 = &繰越一覧[*繰越件数];
                if (繰越ID作成(既存->繰越ID, sizeof(既存->繰越ID), 精算->加盟店コード, 精算->精算区分, 次回日) != 0 ||
                    文字列写し(既存->加盟店コード, sizeof(既存->加盟店コード), 精算->加盟店コード) != 0 ||
                    文字列写し(既存->精算区分, sizeof(既存->精算区分), 精算->精算区分) != 0 ||
                    文字列写し(既存->理由, sizeof(既存->理由), 理由) != 0 ||
                    文字列写し(既存->次回精算日, sizeof(既存->次回精算日), 次回日) != 0) {
                    return -1;
                }
                既存->繰越額 = 判定額;
                ++(*繰越件数);
            }
        }

        精算->繰越額 = 判定額;
    }

    return 0;
}

static int 繰越一覧出力(const 繰越行 *一覧, size_t 件数)
{
    FILE *fp;
    size_t i;

    fp = fopen(出力_PCCARF, "w");
    if (fp == NULL) {
        return -1;
    }

    for (i = 0U; i < 件数; ++i) {
        if (fprintf(fp, "%s,%s,%s,%lld,%s,%s\n",
                    一覧[i].繰越ID,
                    一覧[i].加盟店コード,
                    一覧[i].精算区分,
                    一覧[i].繰越額,
                    一覧[i].理由,
                    一覧[i].次回精算日) < 0) {
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        return -1;
    }

    return 0;
}

static int 精算一覧出力(const 精算行 *一覧, size_t 件数)
{
    FILE *fp;
    size_t i;

    fp = fopen(出力_PCSUMF, "w");
    if (fp == NULL) {
        return -1;
    }

    for (i = 0U; i < 件数; ++i) {
        if (fprintf(fp, "%s,%s,%s,%lld,%lld,%lld\n",
                    一覧[i].加盟店コード,
                    一覧[i].精算日,
                    一覧[i].精算区分,
                    一覧[i].取引件数,
                    一覧[i].合計額,
                    一覧[i].繰越額) < 0) {
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        return -1;
    }

    return 0;
}

int main(void)
{
    static 精算行 精算一覧[件数最大];
    static 繰越行 繰越一覧[件数最大];
    static 加盟店行 加盟店一覧[件数最大];
    size_t 精算件数 = 0U;
    size_t 繰越件数 = 0U;
    size_t 加盟店件数 = 0U;
    int rc;

    rc = 精算一覧読込(精算一覧, &精算件数);
    if (rc == -2) {
        fprintf(stderr, "E1001:PCSUMF入出力異常\n");
        return MIPAY_DECISION_IO_ERROR;
    }
    if (rc != 0) {
        fprintf(stderr, "E1002:PCSUMF形式異常\n");
        return MIPAY_DECISION_PARSE_ERROR;
    }

    rc = 繰越一覧読込(繰越一覧, &繰越件数);
    if (rc == -2) {
        fprintf(stderr, "E1101:PCCARF入出力異常\n");
        return MIPAY_DECISION_IO_ERROR;
    }
    if (rc != 0) {
        fprintf(stderr, "E1102:PCCARF形式異常\n");
        return MIPAY_DECISION_PARSE_ERROR;
    }

    rc = 加盟店一覧読込(加盟店一覧, &加盟店件数);
    if (rc == -2) {
        fprintf(stderr, "E1201:PJMSTF入出力異常\n");
        return MIPAY_DECISION_IO_ERROR;
    }
    if (rc != 0) {
        fprintf(stderr, "E1202:PJMSTF形式異常\n");
        return MIPAY_DECISION_PARSE_ERROR;
    }

    if (繰越反映(精算一覧, 精算件数, 繰越一覧, &繰越件数, 加盟店一覧, 加盟店件数) != 0) {
        fprintf(stderr, "E1301:繰越判定異常\n");
        return MIPAY_DECISION_PARSE_ERROR;
    }

    if (繰越一覧出力(繰越一覧, 繰越件数) != 0) {
        fprintf(stderr, "E1401:PCCARF出力異常\n");
        return MIPAY_DECISION_IO_ERROR;
    }

    if (精算一覧出力(精算一覧, 精算件数) != 0) {
        fprintf(stderr, "E1501:PCSUMF出力異常\n");
        return MIPAY_DECISION_IO_ERROR;
    }

    return MIPAY_DECISION_CARRY_FORWARD;
}
