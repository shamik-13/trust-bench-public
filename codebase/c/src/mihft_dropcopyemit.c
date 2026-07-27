/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240709  篠原 健 (E-203)  ドロップコピー送出処理の初版作成
 * 1.01  20241209  中川 美和 (E-283)  EXEC-ID重複検出と判定ログ出力を追加
 * 1.02  20250509  藤田 和也 (E-271)  CSV境界検査と固定長送出整形を追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_RC_NORMAL 0
#define MIHFT_RC_ERROR  12

#define 入力ファイル名 "HFDROPQ.csv"
#define 送出ファイル名 "HFDROPMSG.dat"
#define 判定ログ名     "HFDECLOG.csv"

#define 最大行長       512
#define 最大項目数     7
#define 最大EXEC保持数 8192
#define 送出電文長     160

#define 動作送出       "SEND"
#define 動作抑止       "SKIP"
#define 理由正常       "OK"
#define 理由重複       "DUP_EXEC"
#define 理由不正       "BAD_RECORD"

typedef struct {
    char drop_id[32];
    char exec_id[48];
    char order_id[48];
    char instr_code[32];
    int64_t fill_qty;
    int64_t fill_amt;
    char capture_ts[32];
} 読込約定;

typedef struct {
    char exec_id[48];
    uint32_t hash;
} EXEC保持;

static EXEC保持 exec表[最大EXEC保持数];
static size_t exec件数;

static void 改行除去(char *行)
{
    size_t 長さ = strlen(行);
    while (長さ > 0 && (行[長さ - 1] == '\n' || 行[長さ - 1] == '\r')) {
        行[--長さ] = '\0';
    }
}

static int 文字列複写(char *宛先, size_t 宛先長, const char *元)
{
    size_t 長さ;

    if (宛先長 == 0 || 元 == NULL) {
        return -1;
    }

    長さ = strlen(元);
    if (長さ >= 宛先長) {
        return -1;
    }

    memcpy(宛先, 元, 長さ + 1);
    return 0;
}

static int64_t 整数変換(const char *文字列, int *失敗)
{
    char *終端;
    long long 値;

    errno = 0;
    値 = strtoll(文字列, &終端, 10);
    if (errno != 0 || 終端 == 文字列 || *終端 != '\0') {
        *失敗 = 1;
        return 0;
    }

    return (int64_t)値;
}

static uint32_t fnv1a32(const char *文字列)
{
    uint32_t hash = 2166136261u;

    while (*文字列 != '\0') {
        hash ^= (unsigned char)*文字列++;
        hash *= 16777619u;
    }

    return hash;
}

static int csv分割(char *行, char *項目[], size_t 最大)
{
    size_t 件数 = 0;
    char *位置 = 行;

    while (件数 < 最大) {
        項目[件数++] = 位置;
        位置 = strchr(位置, ',');
        if (位置 == NULL) {
            break;
        }
        *位置++ = '\0';
    }

    if (strchr(項目[件数 - 1], ',') != NULL) {
        return -1;
    }

    return (int)件数;
}

static int 約定読込(char *行, 読込約定 *約定)
{
    char *項目[最大項目数];
    int 件数;
    int 失敗 = 0;

    改行除去(行);
    件数 = csv分割(行, 項目, 最大項目数);
    if (件数 != 最大項目数) {
        return -1;
    }

    if (文字列複写(約定->drop_id, sizeof(約定->drop_id), 項目[0]) != 0 ||
        文字列複写(約定->exec_id, sizeof(約定->exec_id), 項目[1]) != 0 ||
        文字列複写(約定->order_id, sizeof(約定->order_id), 項目[2]) != 0 ||
        文字列複写(約定->instr_code, sizeof(約定->instr_code), 項目[3]) != 0 ||
        文字列複写(約定->capture_ts, sizeof(約定->capture_ts), 項目[6]) != 0) {
        return -1;
    }

    約定->fill_qty = 整数変換(項目[4], &失敗);
    約定->fill_amt = 整数変換(項目[5], &失敗);
    if (失敗 || 約定->fill_qty <= 0 || 約定->fill_amt < 0) {
        return -1;
    }

    return 0;
}

static int exec重複判定登録(const char *exec_id)
{
    uint32_t hash = fnv1a32(exec_id);
    size_t i;

    for (i = 0; i < exec件数; i++) {
        if (exec表[i].hash == hash && strcmp(exec表[i].exec_id, exec_id) == 0) {
            return 1;
        }
    }

    if (exec件数 >= 最大EXEC保持数) {
        return -1;
    }

    exec表[exec件数].hash = hash;
    if (文字列複写(exec表[exec件数].exec_id, sizeof(exec表[exec件数].exec_id), exec_id) != 0) {
        return -1;
    }
    exec件数++;

    return 0;
}

static void 現在時刻(char *宛先, size_t 宛先長)
{
    time_t 秒 = time(NULL);
    struct tm 時刻;

#if defined(_WIN32)
    localtime_s(&時刻, &秒);
#else
    localtime_r(&秒, &時刻);
#endif
    strftime(宛先, 宛先長, "%Y%m%d%H%M%S", &時刻);
}

static int 判定ログ出力(FILE *出力, unsigned long long decision_id,
                         const 読込約定 *約定, const char *動作, const char *理由)
{
    char 判定時刻[20];

    現在時刻(判定時刻, sizeof(判定時刻));
    if (fprintf(出力, "%llu,%s,%s,%s,%s,%s\n",
                decision_id,
                約定->order_id,
                約定->instr_code,
                動作,
                理由,
                判定時刻) < 0) {
        return -1;
    }

    return 0;
}

static int 固定長送出(FILE *出力, const 読込約定 *約定)
{
    char 電文[送出電文長 + 1];
    int 長さ;

    memset(電文, ' ', sizeof(電文));
    電文[送出電文長] = '\0';

    長さ = snprintf(電文, sizeof(電文),
                    "%-12.12s%-32.32s%-32.32s%-16.16s%020lld%020lld%-14.14s",
                    約定->drop_id,
                    約定->exec_id,
                    約定->order_id,
                    約定->instr_code,
                    (long long)約定->fill_qty,
                    (long long)約定->fill_amt,
                    約定->capture_ts);
    if (長さ < 0 || 長さ > 送出電文長) {
        return -1;
    }

    while ((size_t)長さ < 送出電文長) {
        電文[長さ++] = ' ';
    }

    if (fwrite(電文, 1, 送出電文長, 出力) != 送出電文長) {
        return -1;
    }
    if (fputc('\n', 出力) == EOF) {
        return -1;
    }

    return 0;
}

int main(void)
{
    FILE *入力;
    FILE *送出;
    FILE *判定ログ;
    char 行[最大行長];
    unsigned long long decision_id = 1;

    入力 = fopen(入力ファイル名, "r");
    if (入力 == NULL) {
        return MIHFT_RC_ERROR;
    }

    送出 = fopen(送出ファイル名, "w");
    if (送出 == NULL) {
        fclose(入力);
        return MIHFT_RC_ERROR;
    }

    判定ログ = fopen(判定ログ名, "w");
    if (判定ログ == NULL) {
        fclose(送出);
        fclose(入力);
        return MIHFT_RC_ERROR;
    }

    while (fgets(行, sizeof(行), 入力) != NULL) {
        読込約定 約定;
        int 重複;

        if (strchr(行, '\n') == NULL && !feof(入力)) {
            fclose(判定ログ);
            fclose(送出);
            fclose(入力);
            return MIHFT_RC_ERROR;
        }

        if (約定読込(行, &約定) != 0) {
            memset(&約定, 0, sizeof(約定));
            文字列複写(約定.order_id, sizeof(約定.order_id), "UNKNOWN");
            文字列複写(約定.instr_code, sizeof(約定.instr_code), "UNKNOWN");
            if (判定ログ出力(判定ログ, decision_id++, &約定, 動作抑止, 理由不正) != 0) {
                fclose(判定ログ);
                fclose(送出);
                fclose(入力);
                return MIHFT_RC_ERROR;
            }
            continue;
        }

        重複 = exec重複判定登録(約定.exec_id);
        if (重複 < 0) {
            fclose(判定ログ);
            fclose(送出);
            fclose(入力);
            return MIHFT_RC_ERROR;
        }

        if (重複) {
            if (判定ログ出力(判定ログ, decision_id++, &約定, 動作抑止, 理由重複) != 0) {
                fclose(判定ログ);
                fclose(送出);
                fclose(入力);
                return MIHFT_RC_ERROR;
            }
            continue;
        }

        if (固定長送出(送出, &約定) != 0 ||
            判定ログ出力(判定ログ, decision_id++, &約定, 動作送出, 理由正常) != 0) {
            fclose(判定ログ);
            fclose(送出);
            fclose(入力);
            return MIHFT_RC_ERROR;
        }
    }

    if (ferror(入力) || fclose(判定ログ) != 0 || fclose(送出) != 0 || fclose(入力) != 0) {
        return MIHFT_RC_ERROR;
    }

    return MIHFT_RC_NORMAL;
}
