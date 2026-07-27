/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    20210715    市場基盤部  初版作成
 * 1.01    20211215    市場基盤部  成行注文のMID参照と桁あふれ検査を追加
 * 1.02    20220515    市場基盤部  呼値単位検査と拒否候補判定を整理
 */

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mihft_types.h"

#define MIHFT_IO_ERROR  64
#define MIHFT_FMT_ERROR 65

enum {
    MIHFT_DEC_ACCEPT = 0,
    MIHFT_DEC_REJECT_MARGIN = 4,
    MIHFT_DEC_REJECT_NOTIONAL = 8,
    MIHFT_DEC_REJECT_TICK = 12
};

struct 注文行 {
    char 注文ID[32];
    char 顧客番号[32];
    char 銘柄コード[32];
    char 売買区分;
    char 注文種別;
    char 有効期限[8];
    int64_t 注文数量;
    int64_t 指値金額100;
    int 銘柄階層;
};

struct 気配行 {
    char 銘柄コード[32];
    int64_t 買気配100;
    int64_t 売気配100;
    int64_t 中値100;
    int64_t スプレッド100;
    char 気配時刻[32];
};

static void 改行除去(char *行)
{
    size_t 長さ = strlen(行);
    while (長さ > 0 && (行[長さ - 1] == '\n' || 行[長さ - 1] == '\r')) {
        行[--長さ] = '\0';
    }
}

static char *前後空白除去(char *値)
{
    char *末尾;

    while (isspace((unsigned char)*値)) {
        値++;
    }

    末尾 = 値 + strlen(値);
    while (末尾 > 値 && isspace((unsigned char)末尾[-1])) {
        *--末尾 = '\0';
    }

    return 値;
}

static int 次項目(char **現在, char **項目)
{
    char *開始;
    char *区切り;

    if (*現在 == NULL) {
        return 0;
    }

    開始 = *現在;
    区切り = strchr(開始, ',');
    if (区切り != NULL) {
        *区切り = '\0';
        *現在 = 区切り + 1;
    } else {
        *現在 = NULL;
    }

    *項目 = 前後空白除去(開始);
    return 1;
}

static int 文字列複写(char *宛先, size_t 宛先長, const char *元)
{
    size_t 長さ = strlen(元);

    if (長さ == 0 || 長さ >= 宛先長) {
        return -1;
    }

    memcpy(宛先, 元, 長さ + 1);
    return 0;
}

static int 整数読込(const char *値, int64_t *結果)
{
    char *終端;
    long long 一時値;

    if (*値 == '\0') {
        return -1;
    }

    errno = 0;
    一時値 = strtoll(値, &終端, 10);
    if (errno != 0 || *前後空白除去(終端) != '\0' || 一時値 < 0) {
        return -1;
    }

    *結果 = (int64_t)一時値;
    return 0;
}

static int 金額100読込(const char *値, int64_t *結果)
{
    const unsigned char *走査 = (const unsigned char *)値;
    int64_t 整数部 = 0;
    int64_t 小数部 = 0;
    int 小数桁 = 0;
    int 点検出 = 0;
    int 数字数 = 0;

    if (*値 == '\0') {
        *結果 = 0;
        return 0;
    }

    while (isspace(*走査)) {
        走査++;
    }

    if (*走査 == '-') {
        return -1;
    }

    for (; *走査 != '\0'; 走査++) {
        if (isdigit(*走査)) {
            int 桁 = *走査 - '0';
            数字数++;
            if (!点検出) {
                if (整数部 > (INT64_MAX - 桁) / 10) {
                    return -1;
                }
                整数部 = 整数部 * 10 + 桁;
            } else if (小数桁 < 2) {
                小数部 = 小数部 * 10 + 桁;
                小数桁++;
            } else {
                return -1;
            }
        } else if (*走査 == '.' && !点検出) {
            点検出 = 1;
        } else if (isspace(*走査)) {
            while (isspace(*走査)) {
                走査++;
            }
            if (*走査 != '\0') {
                return -1;
            }
            break;
        } else {
            return -1;
        }
    }

    if (数字数 == 0 || 整数部 > (INT64_MAX - 小数部) / 100) {
        return -1;
    }

    while (小数桁 < 2) {
        小数部 *= 10;
        小数桁++;
    }

    *結果 = 整数部 * 100 + 小数部;
    return 0;
}

static int 階層刻み(int 階層, int64_t *刻み100)
{
    if (階層 == 1) {
        *刻み100 = 100;
        return 0;
    }
    if (階層 == 2) {
        *刻み100 = 500;
        return 0;
    }
    if (階層 == 3) {
        *刻み100 = 1000;
        return 0;
    }
    return -1;
}

static int 階層証拠金率bp(int 階層, int64_t *率bp)
{
    if (階層 == 1) {
        *率bp = 1000;
        return 0;
    }
    if (階層 == 2) {
        *率bp = 2000;
        return 0;
    }
    if (階層 == 3) {
        *率bp = 4000;
        return 0;
    }
    return -1;
}

static int 注文解析(char *行, struct 注文行 *注文)
{
    char *現在 = 行;
    char *項目;
    int64_t 一時整数;

    if (!次項目(&現在, &項目) || 文字列複写(注文->注文ID, sizeof(注文->注文ID), 項目) != 0) {
        return -1;
    }
    if (!次項目(&現在, &項目) || 文字列複写(注文->顧客番号, sizeof(注文->顧客番号), 項目) != 0) {
        return -1;
    }
    if (!次項目(&現在, &項目) || 文字列複写(注文->銘柄コード, sizeof(注文->銘柄コード), 項目) != 0) {
        return -1;
    }
    if (!次項目(&現在, &項目) || strlen(項目) != 1 || (項目[0] != 'B' && 項目[0] != 'S')) {
        return -1;
    }
    注文->売買区分 = 項目[0];

    if (!次項目(&現在, &項目) || strlen(項目) != 1 || (項目[0] != 'L' && 項目[0] != 'M')) {
        return -1;
    }
    注文->注文種別 = 項目[0];

    if (!次項目(&現在, &項目) ||
        (strcmp(項目, "DAY") != 0 && strcmp(項目, "IOC") != 0 && strcmp(項目, "FOK") != 0) ||
        文字列複写(注文->有効期限, sizeof(注文->有効期限), 項目) != 0) {
        return -1;
    }

    if (!次項目(&現在, &項目) || 整数読込(項目, &注文->注文数量) != 0 || 注文->注文数量 == 0) {
        return -1;
    }
    if (!次項目(&現在, &項目) || 金額100読込(項目, &注文->指値金額100) != 0) {
        return -1;
    }
    if (!次項目(&現在, &項目) || 整数読込(項目, &一時整数) != 0 || 一時整数 > 3) {
        return -1;
    }
    注文->銘柄階層 = (int)一時整数;

    return 現在 == NULL ? 0 : -1;
}

static int 気配解析(char *行, struct 気配行 *気配)
{
    char *現在 = 行;
    char *項目;

    if (!次項目(&現在, &項目) || 文字列複写(気配->銘柄コード, sizeof(気配->銘柄コード), 項目) != 0) {
        return -1;
    }
    if (!次項目(&現在, &項目) || 金額100読込(項目, &気配->買気配100) != 0) {
        return -1;
    }
    if (!次項目(&現在, &項目) || 金額100読込(項目, &気配->売気配100) != 0) {
        return -1;
    }
    if (!次項目(&現在, &項目) || 金額100読込(項目, &気配->中値100) != 0) {
        return -1;
    }
    if (!次項目(&現在, &項目) || 金額100読込(項目, &気配->スプレッド100) != 0) {
        return -1;
    }
    if (!次項目(&現在, &項目) || 文字列複写(気配->気配時刻, sizeof(気配->気配時刻), 項目) != 0) {
        return -1;
    }

    return 現在 == NULL ? 0 : -1;
}

static int 気配検索(const char *銘柄コード, struct 気配行 *一致)
{
    FILE *fp = fopen("HFQUOTF.csv", "r");
    char 行[512];

    if (fp == NULL) {
        fprintf(stderr, "HFQUOTF読込不可\n");
        return MIHFT_IO_ERROR;
    }

    while (fgets(行, sizeof(行), fp) != NULL) {
        struct 気配行 候補;
        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }
        if (気配解析(行, &候補) != 0) {
            fclose(fp);
            fprintf(stderr, "HFQUOTF形式不正\n");
            return MIHFT_FMT_ERROR;
        }
        if (strcmp(候補.銘柄コード, 銘柄コード) == 0) {
            *一致 = 候補;
            fclose(fp);
            return MIHFT_DEC_ACCEPT;
        }
    }

    fclose(fp);
    return MIHFT_DEC_REJECT_NOTIONAL;
}

static int64_t 切上除算(int64_t 分子, int64_t 分母)
{
    return 分子 / 分母 + (分子 % 分母 != 0);
}

static int 想定元本計算(const struct 注文行 *注文, int64_t *想定元本100, int64_t *証拠金見積100)
{
    struct 気配行 気配;
    int64_t 採用価格100;
    int64_t 刻み100;
    int64_t 証拠金率bp;
    int 気配結果;

    if (階層刻み(注文->銘柄階層, &刻み100) != 0 || 階層証拠金率bp(注文->銘柄階層, &証拠金率bp) != 0) {
        return MIHFT_FMT_ERROR;
    }

    if (注文->注文種別 == 'L') {
        if (注文->指値金額100 <= 0) {
            return MIHFT_DEC_REJECT_NOTIONAL;
        }
        if (注文->指値金額100 % 刻み100 != 0) {
            return MIHFT_DEC_REJECT_TICK;
        }
        採用価格100 = 注文->指値金額100;
    } else {
        気配結果 = 気配検索(注文->銘柄コード, &気配);
        if (気配結果 != MIHFT_DEC_ACCEPT) {
            return 気配結果;
        }
        if (気配.中値100 <= 0) {
            return MIHFT_DEC_REJECT_NOTIONAL;
        }
        採用価格100 = 気配.中値100;
    }

    if (注文->注文数量 > INT64_MAX / 採用価格100) {
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    *想定元本100 = 注文->注文数量 * 採用価格100;
    if (*想定元本100 > MIHFT_MAX_NOTIONAL) {
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    if (*想定元本100 > (INT64_MAX / 証拠金率bp)) {
        return MIHFT_DEC_REJECT_MARGIN;
    }
    *証拠金見積100 = 切上除算(*想定元本100 * 証拠金率bp, 10000);

    return MIHFT_DEC_ACCEPT;
}

int main(void)
{
    FILE *fp = fopen("SCORDF.csv", "r");
    char 行[512];
    int 最終判定 = MIHFT_DEC_ACCEPT;

    if (fp == NULL) {
        fprintf(stderr, "SCORDF読込不可\n");
        return MIHFT_IO_ERROR;
    }

    while (fgets(行, sizeof(行), fp) != NULL) {
        struct 注文行 注文;
        int64_t 想定元本100 = 0;
        int64_t 証拠金見積100 = 0;
        int 判定;

        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }

        if (注文解析(行, &注文) != 0) {
            fclose(fp);
            fprintf(stderr, "SCORDF形式不正\n");
            return MIHFT_FMT_ERROR;
        }

        判定 = 想定元本計算(&注文, &想定元本100, &証拠金見積100);
        if (判定 == MIHFT_IO_ERROR || 判定 == MIHFT_FMT_ERROR) {
            fclose(fp);
            return 判定;
        }

        printf("%s,%s,%" PRId64 ",%" PRId64 ",%d\n",
               注文.注文ID,
               注文.銘柄コード,
               想定元本100,
               証拠金見積100,
               判定);

        if (判定 != MIHFT_DEC_ACCEPT && 最終判定 == MIHFT_DEC_ACCEPT) {
            最終判定 = 判定;
        }
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCORDF読込中断\n");
        return MIHFT_IO_ERROR;
    }

    fclose(fp);
    return 最終判定;
}
