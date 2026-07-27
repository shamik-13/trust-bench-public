/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240213  藤田 和也 (E-271)    初版作成、取引時間ゲート判定を実装
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define 入力ＳＣＣＡＬＦ "SCCALF.csv"
#define 入力ＳＣＯＲＤＦ "SCORDF.csv"
#define 出力ＨＦＲＪＣＴ "HFRJCT.dat"

#define 判定受付 0
#define 判定時間外 8

#define 拒否ＣＤ時間外 "SESS"
#define 明細ＣＤ休場 "CLOSED"
#define 明細ＣＤ寄前 "BEFORE"
#define 明細ＣＤ昼休 "BREAK"
#define 明細ＣＤ引後 "AFTER"
#define 明細ＣＤ暦不正 "CALERR"

#define 行長上限 1024
#define 項目数上限 16
#define 注文ＩＤ長 64
#define 顧客番号長 64
#define 銘柄コード長 64
#define 区分長 16
#define 拒否ＩＤ長 64

struct 暦行 {
    char 営業日[16];
    char セッション区分[16];
    uint64_t 開始時刻;
    uint64_t 終了時刻;
};

struct 注文行 {
    char 注文ＩＤ[注文ＩＤ長];
    char 顧客番号[顧客番号長];
    char 銘柄コード[銘柄コード長];
    char 売買区分[区分長];
    char 注文種別[区分長];
    char 有効条件[区分長];
    uint64_t 注文数量;
    uint64_t 価格;
    int 銘柄階層;
};

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

static int ＣＳＶ分割(char *行, char *項目[], int 上限)
{
    int 件数 = 0;
    char *現在 = 行;

    while (件数 < 上限) {
        項目[件数++] = 前後空白除去(現在);
        現在 = strchr(現在, ',');
        if (現在 == NULL) {
            break;
        }
        *現在++ = '\0';
    }

    return 件数;
}

static bool ヘッダ行か(const char *項目)
{
    return strcmp(項目, "SESS-DT") == 0 || strcmp(項目, "ORDER-ID") == 0;
}

static bool 文字列格納(char *出力, size_t 出力長, const char *入力)
{
    size_t 長さ = strlen(入力);

    if (長さ == 0 || 長さ >= 出力長) {
        return false;
    }

    memcpy(出力, 入力, 長さ + 1);
    return true;
}

static bool 符号なし整数解析(const char *文字列, uint64_t *値)
{
    char *終端 = NULL;
    unsigned long long 一時値;

    if (文字列 == NULL || *文字列 == '\0' || *文字列 == '-') {
        return false;
    }

    errno = 0;
    一時値 = strtoull(文字列, &終端, 10);
    if (errno == ERANGE || 終端 == 文字列 || *前後空白除去(終端) != '\0') {
        return false;
    }

    *値 = (uint64_t)一時値;
    return true;
}

static bool 整数解析(const char *文字列, int *値)
{
    char *終端 = NULL;
    long 一時値;

    if (文字列 == NULL || *文字列 == '\0') {
        return false;
    }

    errno = 0;
    一時値 = strtol(文字列, &終端, 10);
    if (errno == ERANGE || 終端 == 文字列 || *前後空白除去(終端) != '\0' ||
        一時値 < INT_MIN || 一時値 > INT_MAX) {
        return false;
    }

    *値 = (int)一時値;
    return true;
}

static uint64_t 単調時刻ナノ秒(void)
{
    struct timespec 時刻;

    if (clock_gettime(CLOCK_MONOTONIC, &時刻) != 0) {
        return 0;
    }

    return (uint64_t)時刻.tv_sec * 1000000000ULL + (uint64_t)時刻.tv_nsec;
}

static uint64_t 現在時刻ナノ秒(void)
{
    struct timespec 時刻;

    if (clock_gettime(CLOCK_REALTIME, &時刻) != 0) {
        return 0;
    }

    return (uint64_t)時刻.tv_sec * 1000000000ULL + (uint64_t)時刻.tv_nsec;
}

static bool 暦読込(FILE *入力, struct 暦行 *暦, bool *暦あり)
{
    char 行[行長上限];
    char *項目[項目数上限];

    *暦あり = false;

    while (fgets(行, sizeof 行, 入力) != NULL) {
        int 件数;

        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }

        件数 = ＣＳＶ分割(行, 項目, 項目数上限);
        if (件数 > 0 && ヘッダ行か(項目[0])) {
            continue;
        }

        if (件数 != 4) {
            return false;
        }

        if (!文字列格納(暦->営業日, sizeof 暦->営業日, 項目[0]) ||
            !文字列格納(暦->セッション区分, sizeof 暦->セッション区分, 項目[1]) ||
            !符号なし整数解析(項目[2], &暦->開始時刻) ||
            !符号なし整数解析(項目[3], &暦->終了時刻)) {
            return false;
        }

        *暦あり = true;
        return true;
    }

    return ferror(入力) == 0;
}

static bool 注文読込(FILE *入力, struct 注文行 *注文, bool *注文あり)
{
    char 行[行長上限];
    char *項目[項目数上限];

    *注文あり = false;

    while (fgets(行, sizeof 行, 入力) != NULL) {
        int 件数;

        改行除去(行);
        if (行[0] == '\0') {
            continue;
        }

        件数 = ＣＳＶ分割(行, 項目, 項目数上限);
        if (件数 > 0 && ヘッダ行か(項目[0])) {
            continue;
        }

        if (件数 != 9) {
            return false;
        }

        if (!文字列格納(注文->注文ＩＤ, sizeof 注文->注文ＩＤ, 項目[0]) ||
            !文字列格納(注文->顧客番号, sizeof 注文->顧客番号, 項目[1]) ||
            !文字列格納(注文->銘柄コード, sizeof 注文->銘柄コード, 項目[2]) ||
            !文字列格納(注文->売買区分, sizeof 注文->売買区分, 項目[3]) ||
            !文字列格納(注文->注文種別, sizeof 注文->注文種別, 項目[4]) ||
            !文字列格納(注文->有効条件, sizeof 注文->有効条件, 項目[5]) ||
            !符号なし整数解析(項目[6], &注文->注文数量) ||
            !符号なし整数解析(項目[7], &注文->価格) ||
            !整数解析(項目[8], &注文->銘柄階層)) {
            return false;
        }

        *注文あり = true;
        return true;
    }

    return ferror(入力) == 0;
}

static const char *時間外明細(const struct 暦行 *暦, uint64_t 現在)
{
    if (strcmp(暦->セッション区分, "CLOSED") == 0 || strcmp(暦->セッション区分, "休場") == 0) {
        return 明細ＣＤ休場;
    }

    if (暦->開始時刻 >= 暦->終了時刻) {
        return 明細ＣＤ暦不正;
    }

    if (現在 < 暦->開始時刻) {
        return 明細ＣＤ寄前;
    }

    if (現在 >= 暦->終了時刻) {
        return 明細ＣＤ引後;
    }

    if (strcmp(暦->セッション区分, "BREAK") == 0 || strcmp(暦->セッション区分, "昼休") == 0) {
        return 明細ＣＤ昼休;
    }

    return NULL;
}

static bool 拒否出力(FILE *出力, uint64_t 連番, const struct 注文行 *注文, const char *明細)
{
    char 拒否ＩＤ[拒否ＩＤ長];
    int 書式結果 = snprintf(拒否ＩＤ, sizeof 拒否ＩＤ, "RJ%012" PRIu64, 連番);

    if (書式結果 < 0 || (size_t)書式結果 >= sizeof 拒否ＩＤ) {
        return false;
    }

    return fprintf(出力, "%s,%s,%s,%s,%s,%s,%" PRIu64 "\n",
                   拒否ＩＤ,
                   注文->注文ＩＤ,
                   注文->顧客番号,
                   注文->銘柄コード,
                   拒否ＣＤ時間外,
                   明細,
                   現在時刻ナノ秒()) >= 0;
}

int main(void)
{
    FILE *暦入力 = NULL;
    FILE *注文入力 = NULL;
    FILE *拒否出力先 = NULL;
    struct 暦行 暦;
    bool 暦あり = false;
    bool 注文あり = false;
    uint64_t 現在 = 単調時刻ナノ秒();
    uint64_t 拒否連番 = 1;
    int 最終判定 = 判定受付;
    const char *明細;

    if (現在 == 0) {
        return 2;
    }

    暦入力 = fopen(入力ＳＣＣＡＬＦ, "r");
    if (暦入力 == NULL) {
        return 2;
    }

    if (!暦読込(暦入力, &暦, &暦あり) || !暦あり) {
        fclose(暦入力);
        return 2;
    }

    if (fclose(暦入力) != 0) {
        return 2;
    }

    明細 = 時間外明細(&暦, 現在);

    注文入力 = fopen(入力ＳＣＯＲＤＦ, "r");
    if (注文入力 == NULL) {
        return 2;
    }

    拒否出力先 = fopen(出力ＨＦＲＪＣＴ, "a");
    if (拒否出力先 == NULL) {
        fclose(注文入力);
        return 2;
    }

    for (;;) {
        struct 注文行 注文;

        if (!注文読込(注文入力, &注文, &注文あり)) {
            fclose(注文入力);
            fclose(拒否出力先);
            return 2;
        }

        if (!注文あり) {
            break;
        }

        if (明細 != NULL) {
            if (!拒否出力(拒否出力先, 拒否連番++, &注文, 明細)) {
                fclose(注文入力);
                fclose(拒否出力先);
                return 2;
            }
            最終判定 = 判定時間外;
        }
    }

    if (fclose(注文入力) != 0) {
        fclose(拒否出力先);
        return 2;
    }

    if (fclose(拒否出力先) != 0) {
        return 2;
    }

    return 最終判定;
}
