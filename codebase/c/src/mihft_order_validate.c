/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20220906  三宅 拓也 (E-241)  初版作成
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <time.h>
#include "mihft_types.h"

#define 入力行長 1024
#define 銘柄上限 4096
#define 欄数注文 9
#define 欄数銘柄 6

typedef struct {
    char 注文ＩＤ[64];
    char 顧客番号[64];
    char 銘柄コード[32];
    char 売買区分[4];
    char 注文種別[4];
    char 有効期限[8];
    long long 注文数量;
    long long 価格;
    int 銘柄階層;
} 注文作業域;

typedef struct {
    char 銘柄コード[32];
    char 銘柄名[128];
    int 銘柄階層;
    long long 呼値;
    long long 売買単位;
    char 市場コード[8];
} 銘柄作業域;

static void 改行除去(char *文字列)
{
    size_t 長さ = strlen(文字列);
    while (長さ > 0 && (文字列[長さ - 1] == '\n' || 文字列[長さ - 1] == '\r')) {
        文字列[--長さ] = '\0';
    }
}

static int 欄分割(char *行, char *欄[], int 上限)
{
    int 件数 = 0;
    char *現在 = 行;

    while (件数 < 上限) {
        欄[件数++] = 現在;
        現在 = strchr(現在, ',');
        if (現在 == NULL) {
            break;
        }
        *現在++ = '\0';
    }
    return 件数;
}

static int 整数変換(const char *文字列, long long *値)
{
    char *終端 = NULL;
    long long 結果;

    if (文字列 == NULL || *文字列 == '\0') {
        return -1;
    }

    errno = 0;
    結果 = strtoll(文字列, &終端, 10);
    if (errno != 0 || 終端 == 文字列 || *終端 != '\0') {
        return -1;
    }

    *値 = 結果;
    return 0;
}

static int 小整数変換(const char *文字列, int *値)
{
    long long 作業値;

    if (整数変換(文字列, &作業値) != 0 || 作業値 < INT_MIN || 作業値 > INT_MAX) {
        return -1;
    }

    *値 = (int)作業値;
    return 0;
}

static int 文字列設定(char *宛先, size_t 宛先長, const char *元)
{
    size_t 長さ;

    if (元 == NULL) {
        return -1;
    }

    長さ = strlen(元);
    if (長さ == 0 || 長さ >= 宛先長) {
        return -1;
    }

    memcpy(宛先, 元, 長さ + 1);
    return 0;
}

static int 注文読込(char *行, 注文作業域 *注文)
{
    char *欄[欄数注文];

    改行除去(行);
    if (欄分割(行, 欄, 欄数注文) != 欄数注文) {
        return -1;
    }

    if (文字列設定(注文->注文ＩＤ, sizeof(注文->注文ＩＤ), 欄[0]) != 0 ||
        文字列設定(注文->顧客番号, sizeof(注文->顧客番号), 欄[1]) != 0 ||
        文字列設定(注文->銘柄コード, sizeof(注文->銘柄コード), 欄[2]) != 0 ||
        文字列設定(注文->売買区分, sizeof(注文->売買区分), 欄[3]) != 0 ||
        文字列設定(注文->注文種別, sizeof(注文->注文種別), 欄[4]) != 0 ||
        文字列設定(注文->有効期限, sizeof(注文->有効期限), 欄[5]) != 0 ||
        整数変換(欄[6], &注文->注文数量) != 0 ||
        整数変換(欄[7], &注文->価格) != 0 ||
        小整数変換(欄[8], &注文->銘柄階層) != 0) {
        return -1;
    }

    return 0;
}

static int 銘柄読込(char *行, 銘柄作業域 *銘柄)
{
    char *欄[欄数銘柄];

    改行除去(行);
    if (欄分割(行, 欄, 欄数銘柄) != 欄数銘柄) {
        return -1;
    }

    if (文字列設定(銘柄->銘柄コード, sizeof(銘柄->銘柄コード), 欄[0]) != 0 ||
        文字列設定(銘柄->銘柄名, sizeof(銘柄->銘柄名), 欄[1]) != 0 ||
        小整数変換(欄[2], &銘柄->銘柄階層) != 0 ||
        整数変換(欄[3], &銘柄->呼値) != 0 ||
        整数変換(欄[4], &銘柄->売買単位) != 0 ||
        文字列設定(銘柄->市場コード, sizeof(銘柄->市場コード), 欄[5]) != 0) {
        return -1;
    }

    return 0;
}

static const 銘柄作業域 *銘柄検索(const 銘柄作業域 銘柄表[], size_t 件数, const char *銘柄コード)
{
    size_t 添字;

    for (添字 = 0; 添字 < 件数; 添字++) {
        if (strcmp(銘柄表[添字].銘柄コード, 銘柄コード) == 0) {
            return &銘柄表[添字];
        }
    }

    return NULL;
}

static void 時刻文字列(char *宛先, size_t 宛先長)
{
    time_t 現在時刻 = time(NULL);
    struct tm 時刻要素;

#if defined(_WIN32)
    localtime_s(&時刻要素, &現在時刻);
#else
    localtime_r(&現在時刻, &時刻要素);
#endif
    strftime(宛先, 宛先長, "%Y%m%d%H%M%S", &時刻要素);
}

static int 拒否出力(FILE *出力, long long *連番, const 注文作業域 *注文, const char *拒否コード, const char *詳細コード)
{
    char 時刻[32];

    時刻文字列(時刻, sizeof(時刻));
    (*連番)++;

    if (fprintf(出力, "RJ%012lld,%s,%s,%s,%s,%s,%s\n",
                *連番,
                注文->注文ＩＤ,
                注文->顧客番号,
                注文->銘柄コード,
                拒否コード,
                詳細コード,
                時刻) < 0) {
        return -1;
    }

    return 0;
}

static int 注文検証(const 注文作業域 *注文, const 銘柄作業域 *銘柄, const char **拒否コード, const char **詳細コード)
{
    long long 約定想定額;

    if (注文->注文ＩＤ[0] == '\0' || 注文->顧客番号[0] == '\0' || 注文->銘柄コード[0] == '\0') {
        *拒否コード = "HFRJCT";
        *詳細コード = "REQ";
        return 8;
    }

    if (strcmp(注文->売買区分, "B") != 0 && strcmp(注文->売買区分, "S") != 0) {
        *拒否コード = "HFRJCT";
        *詳細コード = "SIDE";
        return 8;
    }

    if (strcmp(注文->注文種別, "L") != 0 && strcmp(注文->注文種別, "M") != 0) {
        *拒否コード = "HFRJCT";
        *詳細コード = "TYPE";
        return 8;
    }

    if (strcmp(注文->有効期限, "DAY") != 0 &&
        strcmp(注文->有効期限, "IOC") != 0 &&
        strcmp(注文->有効期限, "FOK") != 0) {
        *拒否コード = "HFRJCT";
        *詳細コード = "TIF";
        return 8;
    }

    if (銘柄 == NULL) {
        *拒否コード = "HFRJCT";
        *詳細コード = "INST";
        return 8;
    }

    if (注文->注文数量 <= 0 || 銘柄->売買単位 <= 0 || 注文->注文数量 % 銘柄->売買単位 != 0) {
        *拒否コード = "HFRJCT";
        *詳細コード = "LOT-QTY";
        return 8;
    }

    if (strcmp(注文->注文種別, "L") == 0) {
        if (注文->価格 <= 0) {
            *拒否コード = "HFRJCT";
            *詳細コード = "PRICE";
            return 8;
        }
        if (銘柄->呼値 <= 0 || 注文->価格 % 銘柄->呼値 != 0) {
            *拒否コード = "HFRJCT";
            *詳細コード = "TICK-AMT";
            return 12;
        }
        if (注文->注文数量 > LLONG_MAX / 注文->価格) {
            *拒否コード = "HFRJCT";
            *詳細コード = "NOTIONAL-OVF";
            return 8;
        }
        約定想定額 = 注文->注文数量 * 注文->価格;
        if (約定想定額 > MIHFT_MAX_NOTIONAL) {
            *拒否コード = "HFRJCT";
            *詳細コード = "NOTIONAL";
            return 8;
        }
    } else if (注文->価格 != 0) {
        *拒否コード = "HFRJCT";
        *詳細コード = "MKT-PRICE";
        return 8;
    }

    return 0;
}

int main(void)
{
    const char *注文ファイル名 = "SCORDF.csv";
    const char *銘柄ファイル名 = "SCINSTF.csv";
    const char *拒否ファイル名 = "HFRJCT.csv";
    FILE *注文入力;
    FILE *銘柄入力;
    FILE *拒否出力先;
    銘柄作業域 銘柄表[銘柄上限];
    size_t 銘柄件数 = 0;
    char 行[入力行長];
    long long 拒否連番 = 0;
    int 最終判定 = 0;

    銘柄入力 = fopen(銘柄ファイル名, "r");
    if (銘柄入力 == NULL) {
        fprintf(stderr, "銘柄入力オープン失敗:%s\n", 銘柄ファイル名);
        return 2;
    }

    if (fgets(行, sizeof(行), 銘柄入力) == NULL) {
        fprintf(stderr, "銘柄入力ヘッダ読込失敗\n");
        fclose(銘柄入力);
        return 2;
    }

    while (fgets(行, sizeof(行), 銘柄入力) != NULL) {
        if (銘柄件数 >= 銘柄上限 || 銘柄読込(行, &銘柄表[銘柄件数]) != 0) {
            fprintf(stderr, "銘柄入力形式不正\n");
            fclose(銘柄入力);
            return 2;
        }
        銘柄件数++;
    }

    if (ferror(銘柄入力)) {
        fprintf(stderr, "銘柄入力読込失敗\n");
        fclose(銘柄入力);
        return 2;
    }
    fclose(銘柄入力);

    注文入力 = fopen(注文ファイル名, "r");
    if (注文入力 == NULL) {
        fprintf(stderr, "注文入力オープン失敗:%s\n", 注文ファイル名);
        return 2;
    }

    拒否出力先 = fopen(拒否ファイル名, "w");
    if (拒否出力先 == NULL) {
        fprintf(stderr, "拒否出力オープン失敗:%s\n", 拒否ファイル名);
        fclose(注文入力);
        return 2;
    }

    if (fprintf(拒否出力先, "REJECT-ID,ORDER-ID,CIF-NO,INSTR-CODE,REJECT-CD,DETAIL-CD,REJECT-TS\n") < 0) {
        fprintf(stderr, "拒否出力ヘッダ書込失敗\n");
        fclose(注文入力);
        fclose(拒否出力先);
        return 2;
    }

    if (fgets(行, sizeof(行), 注文入力) == NULL) {
        fprintf(stderr, "注文入力ヘッダ読込失敗\n");
        fclose(注文入力);
        fclose(拒否出力先);
        return 2;
    }

    while (fgets(行, sizeof(行), 注文入力) != NULL) {
        注文作業域 注文;
        const 銘柄作業域 *銘柄;
        const char *拒否コード = "";
        const char *詳細コード = "";
        int 判定;

        if (注文読込(行, &注文) != 0) {
            fprintf(stderr, "注文入力形式不正\n");
            fclose(注文入力);
            fclose(拒否出力先);
            return 2;
        }

        銘柄 = 銘柄検索(銘柄表, 銘柄件数, 注文.銘柄コード);
        判定 = 注文検証(&注文, 銘柄, &拒否コード, &詳細コード);
        if (判定 != 0) {
            if (拒否出力(拒否出力先, &拒否連番, &注文, 拒否コード, 詳細コード) != 0) {
                fprintf(stderr, "拒否出力書込失敗\n");
                fclose(注文入力);
                fclose(拒否出力先);
                return 2;
            }
            最終判定 = 判定;
        }
    }

    if (ferror(注文入力)) {
        fprintf(stderr, "注文入力読込失敗\n");
        fclose(注文入力);
        fclose(拒否出力先);
        return 2;
    }

    if (fclose(注文入力) != 0) {
        fprintf(stderr, "注文入力クローズ失敗\n");
        fclose(拒否出力先);
        return 2;
    }

    if (fclose(拒否出力先) != 0) {
        fprintf(stderr, "拒否出力クローズ失敗\n");
        return 2;
    }

    return 最終判定;
}
