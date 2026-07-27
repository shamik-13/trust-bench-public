/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240709  中川 美和 (E-283)  初版作成
 * 1.01  20241209  篠原 健 (E-203)  銘柄階層別ティック検査を追加
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
    終了_引数異常 = 64,
    終了_入出力異常 = 74,
    終了_形式異常 = 65,
    判定_許可 = 0,
    判定_ティック拒否 = 12
};

typedef struct {
    char 銘柄コード[32];
    char 銘柄名[96];
    int 階層;
    int64_t ティック額;
    int64_t 売買単位;
    char ボードコード[8];
} 銘柄レコード;

static int 文字列同一(const char *左, const char *右)
{
    return strcmp(左, 右) == 0;
}

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

    while (*値 != '\0' && isspace((unsigned char)*値)) {
        値++;
    }

    末尾 = 値 + strlen(値);
    while (末尾 > 値 && isspace((unsigned char)末尾[-1])) {
        *--末尾 = '\0';
    }

    return 値;
}

static int CSV分割(char *行, char *列[], size_t 最大列数)
{
    size_t 列数 = 0;
    char *現在 = 行;

    while (列数 < 最大列数) {
        char *区切り = strchr(現在, ',');

        if (区切り != NULL) {
            *区切り = '\0';
        }

        列[列数++] = 前後空白除去(現在);

        if (区切り == NULL) {
            break;
        }
        現在 = 区切り + 1;
    }

    return (int)列数;
}

static int int64読取(const char *文字列, int64_t *値)
{
    char *終端 = NULL;
    long long 読取値;

    errno = 0;
    読取値 = strtoll(文字列, &終端, 10);
    if (errno != 0 || 終端 == 文字列 || *前後空白除去(終端) != '\0') {
        return 0;
    }

    *値 = (int64_t)読取値;
    return 1;
}

static int int読取(const char *文字列, int *値)
{
    int64_t 一時値;

    if (!int64読取(文字列, &一時値) || 一時値 < INT32_MIN || 一時値 > INT32_MAX) {
        return 0;
    }

    *値 = (int)一時値;
    return 1;
}

static int 階層ティック取得(int 階層, int64_t *ティック額)
{
    if (階層 == 1) {
        *ティック額 = 100;
        return 1;
    }
    if (階層 == 2) {
        *ティック額 = 500;
        return 1;
    }
    if (階層 == 3) {
        *ティック額 = 1000;
        return 1;
    }
    return 0;
}

static int ボード妥当(const char *ボードコード)
{
    return 文字列同一(ボードコード, "T1") ||
           文字列同一(ボードコード, "ST") ||
           文字列同一(ボードコード, "ETF");
}

static int 銘柄レコード読取(char *行, 銘柄レコード *銘柄)
{
    char *列[6];
    int 列数;
    int64_t ティック額;
    int64_t 売買単位;
    int 階層;

    列数 = CSV分割(行, 列, 6);
    if (列数 != 6) {
        return 0;
    }

    if (strlen(列[0]) >= sizeof(銘柄->銘柄コード) ||
        strlen(列[1]) >= sizeof(銘柄->銘柄名) ||
        strlen(列[5]) >= sizeof(銘柄->ボードコード)) {
        return 0;
    }

    if (!int読取(列[2], &階層) ||
        !int64読取(列[3], &ティック額) ||
        !int64読取(列[4], &売買単位)) {
        return 0;
    }

    strcpy(銘柄->銘柄コード, 列[0]);
    strcpy(銘柄->銘柄名, 列[1]);
    銘柄->階層 = 階層;
    銘柄->ティック額 = ティック額;
    銘柄->売買単位 = 売買単位;
    strcpy(銘柄->ボードコード, 列[5]);

    return 1;
}

static int 銘柄検索(const char *パス, const char *銘柄コード, 銘柄レコード *銘柄)
{
    FILE *入力 = fopen(パス, "r");
    char 行[512];
    int 行番号 = 0;

    if (入力 == NULL) {
        fprintf(stderr, "SCINSTFを開けません\n");
        return -1;
    }

    while (fgets(行, sizeof 行, 入力) != NULL) {
        銘柄レコード 候補;

        行番号++;
        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }
        if (行番号 == 1 && strstr(行, "INSTR-CODE") != NULL) {
            continue;
        }

        if (!銘柄レコード読取(行, &候補)) {
            fclose(入力);
            fprintf(stderr, "SCINSTFの形式が不正です\n");
            return -2;
        }

        if (文字列同一(候補.銘柄コード, 銘柄コード)) {
            *銘柄 = 候補;
            fclose(入力);
            return 1;
        }
    }

    if (ferror(入力)) {
        fclose(入力);
        fprintf(stderr, "SCINSTFの読取に失敗しました\n");
        return -1;
    }

    fclose(入力);
    return 0;
}

static int ティック検査(const 銘柄レコード *銘柄, const char *注文種別, int64_t 価格額)
{
    int64_t 規定ティック額;

    if (!階層ティック取得(銘柄->階層, &規定ティック額) || !ボード妥当(銘柄->ボードコード)) {
        return 判定_ティック拒否;
    }

    if (銘柄->ティック額 != 規定ティック額 || 銘柄->ティック額 <= 0 || 銘柄->売買単位 <= 0) {
        return 判定_ティック拒否;
    }

    if (文字列同一(注文種別, "M")) {
        return 判定_許可;
    }

    if (!文字列同一(注文種別, "L") || 価格額 <= 0) {
        return 判定_ティック拒否;
    }

    return (価格額 % 銘柄->ティック額 == 0) ? 判定_許可 : 判定_ティック拒否;
}

int main(int argc, char **argv)
{
    銘柄レコード 銘柄;
    int64_t 価格額 = 0;
    int 検索結果;

    if (argc != 5) {
        fprintf(stderr, "使用法: mihft_tickcheck SCINSTF 銘柄コード 注文種別 価格額\n");
        return 終了_引数異常;
    }

    if (!文字列同一(argv[3], "M") && !int64読取(argv[4], &価格額)) {
        fprintf(stderr, "価格額の形式が不正です\n");
        return 終了_形式異常;
    }

    検索結果 = 銘柄検索(argv[1], argv[2], &銘柄);
    if (検索結果 < 0) {
        return (検索結果 == -1) ? 終了_入出力異常 : 終了_形式異常;
    }
    if (検索結果 == 0) {
        fprintf(stderr, "銘柄がSCINSTFに存在しません\n");
        return 判定_ティック拒否;
    }

    return ティック検査(&銘柄, argv[3], 価格額);
}
