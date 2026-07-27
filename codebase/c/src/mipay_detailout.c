/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240520  精算基盤  初版作成
 * 1.01  20241014  精算基盤  調整行展開と差異検知を追加
 * 1.02  20250224  精算基盤  CSV検証と桁あふれ検知を強化
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <ctype.h>

#include "mipay_settle.h"

#ifndef MIPAY_DECISION_NORMAL
#define MIPAY_DECISION_NORMAL 0
#endif

#ifndef MIPAY_DECISION_WARN
#define MIPAY_DECISION_WARN 4
#endif

#ifndef MIPAY_DECISION_ERROR
#define MIPAY_DECISION_ERROR 8
#endif

#define 入力_PSSETF "PSSETF.csv"
#define 入力_PSTXNF "PSTXNF.csv"
#define 入力_PSMERF "PSMERF.csv"
#define 入力_PSADJF "PSADJF.csv"
#define 出力_PSDTLF "PSDTLF.csv"

#define 最大行長 1024
#define 最大精算件数 4096
#define 最大取引件数 32768
#define 最大加盟店件数 4096
#define 最大調整件数 8192

#define 精算対象状態 "01"
#define 手数料_BP 30L

typedef struct {
    char 精算ID[32];
    char 加盟店コード[32];
    long long 純額;
    long long 手数料額;
    long long 支払額;
    char 精算日[16];
} 精算行;

typedef struct {
    char 取引ID[32];
    char 加盟店コード[32];
    char 取引区分;
    long long 取引額;
    char 取引日[16];
    int 使用済み;
} 取引行;

typedef struct {
    char 加盟店コード[32];
    char 加盟店名[128];
    char 状態[8];
    char 口座番号[64];
} 加盟店行;

typedef struct {
    char 調整ID[32];
    char 加盟店コード[32];
    char 調整区分[8];
    long long 調整額;
    char 理由コード[16];
    char 適用日[16];
    char 承認状態[8];
    int 使用済み;
} 調整行;

static void 改行除去(char *文字列)
{
    size_t 長さ = strlen(文字列);
    while (長さ > 0 && (文字列[長さ - 1] == '\n' || 文字列[長さ - 1] == '\r')) {
        文字列[--長さ] = '\0';
    }
}

static char *前後空白除去(char *文字列)
{
    unsigned char *先頭 = (unsigned char *)文字列;
    char *末尾;

    while (*先頭 != '\0' && isspace(*先頭)) {
        ++先頭;
    }

    末尾 = (char *)先頭 + strlen((char *)先頭);
    while (末尾 > (char *)先頭 && isspace((unsigned char)末尾[-1])) {
        *--末尾 = '\0';
    }

    return (char *)先頭;
}

static int CSV分割(char *行, char *項目[], size_t 最大項目)
{
    size_t 件数 = 0;
    char *読位置 = 行;

    while (*読位置 != '\0' && 件数 < 最大項目) {
        char *書位置 = 読位置;
        int 引用中 = 0;

        if (*読位置 == '"') {
            引用中 = 1;
            ++読位置;
            書位置 = 読位置;
            項目[件数++] = 書位置;

            while (*読位置 != '\0') {
                if (*読位置 == '"' && 読位置[1] == '"') {
                    *書位置++ = '"';
                    読位置 += 2;
                } else if (*読位置 == '"') {
                    ++読位置;
                    引用中 = 0;
                    break;
                } else {
                    *書位置++ = *読位置++;
                }
            }
            *書位置 = '\0';

            if (引用中) {
                return -1;
            }
            if (*読位置 == ',') {
                ++読位置;
            } else if (*読位置 != '\0') {
                return -1;
            }
        } else {
            項目[件数++] = 読位置;
            while (*読位置 != '\0' && *読位置 != ',') {
                ++読位置;
            }
            if (*読位置 == ',') {
                *読位置++ = '\0';
            }
        }
    }

    if (*読位置 != '\0') {
        return -1;
    }

    return (int)件数;
}

static int 文字列設定(char *宛先, size_t 宛先長, const char *値)
{
    char 作業[256];
    char *整形後;

    if (strlen(値) >= sizeof 作業) {
        return -1;
    }
    memcpy(作業, 値, strlen(値) + 1);
    整形後 = 前後空白除去(作業);
    if (*整形後 == '\0' || strlen(整形後) >= 宛先長) {
        return -1;
    }
    memcpy(宛先, 整形後, strlen(整形後) + 1);
    return 0;
}

static int 金額変換(const char *文字列, long long *金額)
{
    char 作業[64];
    char *整形後;
    char *終端 = NULL;
    long long 値;

    if (strlen(文字列) >= sizeof 作業) {
        return -1;
    }
    memcpy(作業, 文字列, strlen(文字列) + 1);
    整形後 = 前後空白除去(作業);
    if (*整形後 == '\0') {
        return -1;
    }

    errno = 0;
    値 = strtoll(整形後, &終端, 10);
    if (errno == ERANGE || 終端 == 整形後 || *前後空白除去(終端) != '\0') {
        return -1;
    }

    *金額 = 値;
    return 0;
}

static int 日付検査(const char *日付)
{
    size_t i;

    if (strlen(日付) != 8) {
        return -1;
    }
    for (i = 0; i < 8; ++i) {
        if (!isdigit((unsigned char)日付[i])) {
            return -1;
        }
    }
    return 0;
}

static int 加算検査(long long 左, long long 右, long long *結果)
{
    if ((右 > 0 && 左 > LLONG_MAX - 右) || (右 < 0 && 左 < LLONG_MIN - 右)) {
        return -1;
    }
    *結果 = 左 + 右;
    return 0;
}

static long long 手数料算出(long long 金額)
{
    long long 基準額 = 金額 < 0 ? -金額 : 金額;
    return (基準額 * 手数料_BP + 9999L) / 10000L;
}

static int 精算読込(精算行 配列[], size_t *件数)
{
    FILE *fp = fopen(入力_PSSETF, "r");
    char 行[最大行長];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "重大: PSSETFを開けません\n");
        return -1;
    }

    while (fgets(行, sizeof 行, fp) != NULL) {
        char *項目[6];
        int 項目数;

        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }
        if (n >= 最大精算件数) {
            fclose(fp);
            fprintf(stderr, "重大: PSSETF件数上限超過\n");
            return -1;
        }

        項目数 = CSV分割(行, 項目, 6);
        if (項目数 != 6) {
            fclose(fp);
            fprintf(stderr, "重大: PSSETF項目数不正\n");
            return -1;
        }

        if (文字列設定(配列[n].精算ID, sizeof 配列[n].精算ID, 項目[0]) != 0 ||
            文字列設定(配列[n].加盟店コード, sizeof 配列[n].加盟店コード, 項目[1]) != 0 ||
            金額変換(項目[2], &配列[n].純額) != 0 ||
            金額変換(項目[3], &配列[n].手数料額) != 0 ||
            金額変換(項目[4], &配列[n].支払額) != 0 ||
            文字列設定(配列[n].精算日, sizeof 配列[n].精算日, 項目[5]) != 0 ||
            日付検査(配列[n].精算日) != 0) {
            fclose(fp);
            fprintf(stderr, "重大: PSSETF内容不正\n");
            return -1;
        }
        ++n;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "重大: PSSETF読込失敗\n");
        return -1;
    }

    fclose(fp);
    *件数 = n;
    return 0;
}

static int 取引読込(取引行 配列[], size_t *件数)
{
    FILE *fp = fopen(入力_PSTXNF, "r");
    char 行[最大行長];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "重大: PSTXNFを開けません\n");
        return -1;
    }

    while (fgets(行, sizeof 行, fp) != NULL) {
        char *項目[5];
        char 区分[8];
        int 項目数;

        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }
        if (n >= 最大取引件数) {
            fclose(fp);
            fprintf(stderr, "重大: PSTXNF件数上限超過\n");
            return -1;
        }

        項目数 = CSV分割(行, 項目, 5);
        if (項目数 != 5) {
            fclose(fp);
            fprintf(stderr, "重大: PSTXNF項目数不正\n");
            return -1;
        }

        if (文字列設定(配列[n].取引ID, sizeof 配列[n].取引ID, 項目[0]) != 0 ||
            文字列設定(配列[n].加盟店コード, sizeof 配列[n].加盟店コード, 項目[1]) != 0 ||
            文字列設定(区分, sizeof 区分, 項目[2]) != 0 ||
            金額変換(項目[3], &配列[n].取引額) != 0 ||
            文字列設定(配列[n].取引日, sizeof 配列[n].取引日, 項目[4]) != 0 ||
            日付検査(配列[n].取引日) != 0) {
            fclose(fp);
            fprintf(stderr, "重大: PSTXNF内容不正\n");
            return -1;
        }

        if ((区分[0] != 'C' && 区分[0] != 'R') || 区分[1] != '\0' || 配列[n].取引額 < 0) {
            fclose(fp);
            fprintf(stderr, "重大: PSTXNF区分または金額不正\n");
            return -1;
        }

        配列[n].取引区分 = 区分[0];
        配列[n].使用済み = 0;
        ++n;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "重大: PSTXNF読込失敗\n");
        return -1;
    }

    fclose(fp);
    *件数 = n;
    return 0;
}

static int 加盟店読込(加盟店行 配列[], size_t *件数)
{
    FILE *fp = fopen(入力_PSMERF, "r");
    char 行[最大行長];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "重大: PSMERFを開けません\n");
        return -1;
    }

    while (fgets(行, sizeof 行, fp) != NULL) {
        char *項目[4];
        int 項目数;

        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }
        if (n >= 最大加盟店件数) {
            fclose(fp);
            fprintf(stderr, "重大: PSMERF件数上限超過\n");
            return -1;
        }

        項目数 = CSV分割(行, 項目, 4);
        if (項目数 != 4) {
            fclose(fp);
            fprintf(stderr, "重大: PSMERF項目数不正\n");
            return -1;
        }

        if (文字列設定(配列[n].加盟店コード, sizeof 配列[n].加盟店コード, 項目[0]) != 0 ||
            文字列設定(配列[n].加盟店名, sizeof 配列[n].加盟店名, 項目[1]) != 0 ||
            文字列設定(配列[n].状態, sizeof 配列[n].状態, 項目[2]) != 0 ||
            文字列設定(配列[n].口座番号, sizeof 配列[n].口座番号, 項目[3]) != 0) {
            fclose(fp);
            fprintf(stderr, "重大: PSMERF内容不正\n");
            return -1;
        }

        if (strcmp(配列[n].状態, "01") != 0 &&
            strcmp(配列[n].状態, "02") != 0 &&
            strcmp(配列[n].状態, "09") != 0) {
            fclose(fp);
            fprintf(stderr, "重大: PSMERF状態不正\n");
            return -1;
        }
        ++n;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "重大: PSMERF読込失敗\n");
        return -1;
    }

    fclose(fp);
    *件数 = n;
    return 0;
}

static int 調整読込(調整行 配列[], size_t *件数)
{
    FILE *fp = fopen(入力_PSADJF, "r");
    char 行[最大行長];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "重大: PSADJFを開けません\n");
        return -1;
    }

    while (fgets(行, sizeof 行, fp) != NULL) {
        char *項目[7];
        int 項目数;

        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }
        if (n >= 最大調整件数) {
            fclose(fp);
            fprintf(stderr, "重大: PSADJF件数上限超過\n");
            return -1;
        }

        項目数 = CSV分割(行, 項目, 7);
        if (項目数 != 7) {
            fclose(fp);
            fprintf(stderr, "重大: PSADJF項目数不正\n");
            return -1;
        }

        if (文字列設定(配列[n].調整ID, sizeof 配列[n].調整ID, 項目[0]) != 0 ||
            文字列設定(配列[n].加盟店コード, sizeof 配列[n].加盟店コード, 項目[1]) != 0 ||
            文字列設定(配列[n].調整区分, sizeof 配列[n].調整区分, 項目[2]) != 0 ||
            金額変換(項目[3], &配列[n].調整額) != 0 ||
            文字列設定(配列[n].理由コード, sizeof 配列[n].理由コード, 項目[4]) != 0 ||
            文字列設定(配列[n].適用日, sizeof 配列[n].適用日, 項目[5]) != 0 ||
            文字列設定(配列[n].承認状態, sizeof 配列[n].承認状態, 項目[6]) != 0 ||
            日付検査(配列[n].適用日) != 0) {
            fclose(fp);
            fprintf(stderr, "重大: PSADJF内容不正\n");
            return -1;
        }

        配列[n].使用済み = 0;
        ++n;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "重大: PSADJF読込失敗\n");
        return -1;
    }

    fclose(fp);
    *件数 = n;
    return 0;
}

static const 加盟店行 *加盟店検索(const 加盟店行 配列[], size_t 件数, const char *加盟店コード)
{
    size_t i;

    for (i = 0; i < 件数; ++i) {
        if (strcmp(配列[i].加盟店コード, 加盟店コード) == 0) {
            return &配列[i];
        }
    }
    return NULL;
}

static int 明細出力(FILE *fp, long long *連番, const char *精算ID, const char *加盟店コード,
                    const char *原票ID, long long 取引額, long long 手数料額, const char *行区分)
{
    if (*連番 == LLONG_MAX) {
        fprintf(stderr, "重大: PSDTLF明細番号上限超過\n");
        return -1;
    }
    ++*連番;

    if (fprintf(fp, "D%012lld,%s,%s,%s,%lld,%lld,%s\n",
                *連番, 精算ID, 加盟店コード, 原票ID, 取引額, 手数料額, 行区分) < 0) {
        fprintf(stderr, "重大: PSDTLF書込失敗\n");
        return -1;
    }
    return 0;
}

int main(void)
{
    static 精算行 精算[最大精算件数];
    static 取引行 取引[最大取引件数];
    static 加盟店行 加盟店[最大加盟店件数];
    static 調整行 調整[最大調整件数];

    size_t 精算件数 = 0;
    size_t 取引件数 = 0;
    size_t 加盟店件数 = 0;
    size_t 調整件数 = 0;
    size_t i;
    int 差異あり = 0;
    long long 明細連番 = 0;
    FILE *出力;

    if (精算読込(精算, &精算件数) != 0 ||
        取引読込(取引, &取引件数) != 0 ||
        加盟店読込(加盟店, &加盟店件数) != 0 ||
        調整読込(調整, &調整件数) != 0) {
        return MIPAY_DECISION_ERROR;
    }

    出力 = fopen(出力_PSDTLF, "w");
    if (出力 == NULL) {
        fprintf(stderr, "重大: PSDTLFを作成できません\n");
        return MIPAY_DECISION_ERROR;
    }

    for (i = 0; i < 精算件数; ++i) {
        const 加盟店行 *加盟店情報 = 加盟店検索(加盟店, 加盟店件数, 精算[i].加盟店コード);
        long long 売上合計 = 0;
        long long 返金合計 = 0;
        long long 調整合計 = 0;
        long long 手数料合計 = 0;
        long long 計算純額 = 0;
        long long 計算支払額 = 0;
        size_t j;

        if (加盟店情報 == NULL) {
            fprintf(stderr, "警告: 加盟店未登録 精算ID=%s\n", 精算[i].精算ID);
            差異あり = 1;
            continue;
        }

        if (strcmp(加盟店情報->状態, 精算対象状態) != 0) {
            fprintf(stderr, "警告: 精算対象外 精算ID=%s 加盟店=%s\n", 精算[i].精算ID, 精算[i].加盟店コード);
            差異あり = 1;
            continue;
        }

        for (j = 0; j < 取引件数; ++j) {
            long long 行手数料;

            if (取引[j].使用済み || strcmp(取引[j].加盟店コード, 精算[i].加盟店コード) != 0) {
                continue;
            }
            if (strcmp(取引[j].取引日, 精算[i].精算日) > 0) {
                continue;
            }

            行手数料 = 手数料算出(取引[j].取引額);
            if (取引[j].取引区分 == 'C') {
                if (加算検査(売上合計, 取引[j].取引額, &売上合計) != 0 ||
                    加算検査(手数料合計, 行手数料, &手数料合計) != 0) {
                    fclose(出力);
                    fprintf(stderr, "重大: 売上集計あふれ\n");
                    return MIPAY_DECISION_ERROR;
                }
                if (明細出力(出力, &明細連番, 精算[i].精算ID, 精算[i].加盟店コード,
                             取引[j].取引ID, 取引[j].取引額, 行手数料, "C") != 0) {
                    fclose(出力);
                    return MIPAY_DECISION_ERROR;
                }
            } else {
                if (加算検査(返金合計, 取引[j].取引額, &返金合計) != 0 ||
                    加算検査(手数料合計, 行手数料, &手数料合計) != 0) {
                    fclose(出力);
                    fprintf(stderr, "重大: 返金集計あふれ\n");
                    return MIPAY_DECISION_ERROR;
                }
                if (明細出力(出力, &明細連番, 精算[i].精算ID, 精算[i].加盟店コード,
                             取引[j].取引ID, -取引[j].取引額, 行手数料, "R") != 0) {
                    fclose(出力);
                    return MIPAY_DECISION_ERROR;
                }
            }
            取引[j].使用済み = 1;
        }

        for (j = 0; j < 調整件数; ++j) {
            long long 符号付き調整;

            if (調整[j].使用済み || strcmp(調整[j].加盟店コード, 精算[i].加盟店コード) != 0) {
                continue;
            }
            if (strcmp(調整[j].承認状態, "01") != 0 || strcmp(調整[j].適用日, 精算[i].精算日) > 0) {
                continue;
            }

            符号付き調整 = 調整[j].調整額;
            if (strcmp(調整[j].調整区分, "D") == 0) {
                符号付き調整 = -符号付き調整;
            } else if (strcmp(調整[j].調整区分, "C") != 0) {
                fprintf(stderr, "警告: 調整区分不明 調整ID=%s\n", 調整[j].調整ID);
                差異あり = 1;
                continue;
            }

            if (加算検査(調整合計, 符号付き調整, &調整合計) != 0) {
                fclose(出力);
                fprintf(stderr, "重大: 調整集計あふれ\n");
                return MIPAY_DECISION_ERROR;
            }
            if (明細出力(出力, &明細連番, 精算[i].精算ID, 精算[i].加盟店コード,
                         調整[j].調整ID, 符号付き調整, 0, "A") != 0) {
                fclose(出力);
                return MIPAY_DECISION_ERROR;
            }
            調整[j].使用済み = 1;
        }

        if (加算検査(売上合計, -返金合計, &計算純額) != 0 ||
            加算検査(計算純額, 調整合計, &計算純額) != 0 ||
            加算検査(計算純額, -手数料合計, &計算支払額) != 0) {
            fclose(出力);
            fprintf(stderr, "重大: 精算検算あふれ\n");
            return MIPAY_DECISION_ERROR;
        }

        if (計算純額 != 精算[i].純額 ||
            手数料合計 != 精算[i].手数料額 ||
            計算支払額 != 精算[i].支払額) {
            fprintf(stderr, "警告: 精算差異 精算ID=%s 計算純額=%lld 登録純額=%lld 計算手数料=%lld 登録手数料=%lld 計算支払=%lld 登録支払=%lld\n",
                    精算[i].精算ID, 計算純額, 精算[i].純額,
                    手数料合計, 精算[i].手数料額, 計算支払額, 精算[i].支払額);
            差異あり = 1;
        }
    }

    if (fclose(出力) != 0) {
        fprintf(stderr, "重大: PSDTLF終了処理失敗\n");
        return MIPAY_DECISION_ERROR;
    }

    return 差異あり ? MIPAY_DECISION_WARN : MIPAY_DECISION_NORMAL;
}
