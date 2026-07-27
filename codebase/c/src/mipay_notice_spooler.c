/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240415  みらいペイ システム部  初版作成
 * 1.01  20240903  みらいペイ システム部  NOTICE-ID重複検査と制御文字検査を追加
 * 1.02  20250122  みらいペイ システム部  チャネル別固定長スプール整形を追加
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#define MIPAY_RC_OK 0
#define MIPAY_RC_IO 8
#define MIPAY_RC_PARSE 12

#define 入力行_MAX 4096
#define NOTICE_ID_MAX 32
#define WALLET_ID_MAX 32
#define NOTICE_KBN_MAX 8
#define NOTICE_TEXT_MAX 512
#define SEND_STATUS_MAX 16
#define CREATE_TS_MAX 32

#define スプール_NOTICE_ID幅 24
#define スプール_WALLET_ID幅 24
#define スプール_KBN幅 4
#define スプール_TEXT幅 160
#define スプール_STATUS幅 8
#define スプール_TS幅 20

#define 最大通知件数 20000
#define 送信待ち "PENDING"
#define 再送対象 "RETRY"

typedef struct {
    char notice_id[NOTICE_ID_MAX + 1];
    char wallet_id[WALLET_ID_MAX + 1];
    char notice_kbn[NOTICE_KBN_MAX + 1];
    char notice_text[NOTICE_TEXT_MAX + 1];
    char send_status[SEND_STATUS_MAX + 1];
    char create_ts[CREATE_TS_MAX + 1];
} 通知レコード;

typedef struct {
    char notice_id[NOTICE_ID_MAX + 1];
    unsigned long 行番号;
} NOTICE_ID索引;

static int 文字列複写(char *宛先, size_t 宛先長, const char *元, unsigned long 行番号, const char *項目名)
{
    size_t 長さ = strlen(元);

    if (長さ >= 宛先長) {
        fprintf(stderr, "E101 行%lu %s長超過\n", 行番号, 項目名);
        return 0;
    }
    memcpy(宛先, 元, 長さ + 1);
    return 1;
}

static int 禁止制御文字あり(const char *文字列)
{
    const unsigned char *p = (const unsigned char *)文字列;

    while (*p != '\0') {
        if (*p < 0x20U && *p != '\t') {
            return 1;
        }
        p++;
    }
    return 0;
}

static size_t 表示文字数(const char *文字列)
{
    size_t 件数 = 0;
    const unsigned char *p = (const unsigned char *)文字列;

    while (*p != '\0') {
        if ((*p & 0xC0U) != 0x80U) {
            件数++;
        }
        p++;
    }
    return 件数;
}

static int 固定長出力(FILE *出力, const char *値, size_t 幅)
{
    size_t 長さ = strlen(値);

    if (長さ > 幅) {
        長さ = 幅;
    }
    if (fwrite(値, 1, 長さ, 出力) != 長さ) {
        return 0;
    }
    while (長さ < 幅) {
        if (fputc(' ', 出力) == EOF) {
            return 0;
        }
        長さ++;
    }
    return 1;
}

static int CSV項目取得(char **位置, char *宛先, size_t 宛先長)
{
    char *p = *位置;
    size_t n = 0;
    int 引用中 = 0;

    if (*p == '"') {
        引用中 = 1;
        p++;
        while (*p != '\0') {
            if (*p == '"') {
                if (p[1] == '"') {
                    if (n + 1 >= 宛先長) {
                        return 0;
                    }
                    宛先[n++] = '"';
                    p += 2;
                    continue;
                }
                p++;
                break;
            }
            if (n + 1 >= 宛先長) {
                return 0;
            }
            宛先[n++] = *p++;
        }
        if (引用中 && *p != ',' && *p != '\0' && *p != '\n' && *p != '\r') {
            return 0;
        }
    } else {
        while (*p != ',' && *p != '\0' && *p != '\n' && *p != '\r') {
            if (n + 1 >= 宛先長) {
                return 0;
            }
            宛先[n++] = *p++;
        }
    }

    宛先[n] = '\0';

    if (*p == ',') {
        p++;
    }
    *位置 = p;
    return 1;
}

static int CSV解析(char *行, 通知レコード *通知, unsigned long 行番号)
{
    char *位置 = 行;
    char 項目[6][NOTICE_TEXT_MAX + 1];
    int i;

    for (i = 0; i < 6; i++) {
        if (!CSV項目取得(&位置, 項目[i], sizeof(項目[i]))) {
            fprintf(stderr, "E201 行%lu CSV項目不正\n", 行番号);
            return 0;
        }
    }

    while (*位置 == ' ' || *位置 == '\t') {
        位置++;
    }
    if (*位置 != '\0' && *位置 != '\n' && *位置 != '\r') {
        fprintf(stderr, "E202 行%lu CSV項目過多\n", 行番号);
        return 0;
    }

    return 文字列複写(通知->notice_id, sizeof(通知->notice_id), 項目[0], 行番号, "NOTICE-ID") &&
           文字列複写(通知->wallet_id, sizeof(通知->wallet_id), 項目[1], 行番号, "WALLET-ID") &&
           文字列複写(通知->notice_kbn, sizeof(通知->notice_kbn), 項目[2], 行番号, "NOTICE-KBN") &&
           文字列複写(通知->notice_text, sizeof(通知->notice_text), 項目[3], 行番号, "NOTICE-TEXT") &&
           文字列複写(通知->send_status, sizeof(通知->send_status), 項目[4], 行番号, "SEND-STATUS") &&
           文字列複写(通知->create_ts, sizeof(通知->create_ts), 項目[5], 行番号, "CREATE-TS");
}

static int NOTICE_ID比較(const void *a, const void *b)
{
    const NOTICE_ID索引 *左 = (const NOTICE_ID索引 *)a;
    const NOTICE_ID索引 *右 = (const NOTICE_ID索引 *)b;

    return strcmp(左->notice_id, 右->notice_id);
}

static int 重複NOTICE_ID(const NOTICE_ID索引 *索引, size_t 件数, const char *notice_id)
{
    NOTICE_ID索引 探索キー;

    memset(&探索キー, 0, sizeof(探索キー));
    strncpy(探索キー.notice_id, notice_id, NOTICE_ID_MAX);
    return bsearch(&探索キー, 索引, 件数, sizeof(索引[0]), NOTICE_ID比較) != NULL;
}

static int 通知検査(const 通知レコード *通知, const NOTICE_ID索引 *索引, size_t 件数, unsigned long 行番号)
{
    if (通知->notice_id[0] == '\0' || 通知->wallet_id[0] == '\0' ||
        通知->notice_kbn[0] == '\0' || 通知->create_ts[0] == '\0') {
        fprintf(stderr, "W301 行%lu 必須項目不足\n", 行番号);
        return 0;
    }

    if (表示文字数(通知->notice_text) > スプール_TEXT幅) {
        fprintf(stderr, "W302 行%lu 通知本文文字数超過\n", 行番号);
        return 0;
    }

    if (禁止制御文字あり(通知->notice_text) || 禁止制御文字あり(通知->notice_id) ||
        禁止制御文字あり(通知->wallet_id)) {
        fprintf(stderr, "W303 行%lu 禁止制御文字検出\n", 行番号);
        return 0;
    }

    if (重複NOTICE_ID(索引, 件数, 通知->notice_id)) {
        fprintf(stderr, "W304 行%lu NOTICE-ID重複\n", 行番号);
        return 0;
    }

    return 1;
}

static int スプール出力(FILE *出力, const 通知レコード *通知, int 再送)
{
    const char *状態 = 再送 ? 再送対象 : 通知->send_status;

    if (!固定長出力(出力, 通知->notice_id, スプール_NOTICE_ID幅) ||
        !固定長出力(出力, 通知->wallet_id, スプール_WALLET_ID幅) ||
        !固定長出力(出力, 通知->notice_kbn, スプール_KBN幅) ||
        !固定長出力(出力, 通知->notice_text, スプール_TEXT幅) ||
        !固定長出力(出力, 状態, スプール_STATUS幅) ||
        !固定長出力(出力, 通知->create_ts, スプール_TS幅) ||
        fputc('\n', 出力) == EOF) {
        return 0;
    }

    return 1;
}

int main(void)
{
    char 行[入力行_MAX];
    通知レコード 通知一覧[最大通知件数];
    NOTICE_ID索引 索引[最大通知件数];
    size_t 件数 = 0;
    unsigned long 行番号 = 0;
    size_t i;

    while (fgets(行, sizeof(行), stdin) != NULL) {
        size_t 行長 = strlen(行);

        行番号++;
        if (行長 == sizeof(行) - 1 && 行[行長 - 1] != '\n') {
            fprintf(stderr, "E001 行%lu 入力行長超過\n", 行番号);
            return MIPAY_RC_PARSE;
        }
        if (件数 >= 最大通知件数) {
            fprintf(stderr, "E002 行%lu 入力件数上限超過\n", 行番号);
            return MIPAY_RC_PARSE;
        }
        if (!CSV解析(行, &通知一覧[件数], 行番号)) {
            return MIPAY_RC_PARSE;
        }
        strncpy(索引[件数].notice_id, 通知一覧[件数].notice_id, NOTICE_ID_MAX);
        索引[件数].notice_id[NOTICE_ID_MAX] = '\0';
        索引[件数].行番号 = 行番号;
        件数++;
    }

    if (ferror(stdin)) {
        fprintf(stderr, "E003 入力読込失敗\n");
        return MIPAY_RC_IO;
    }

    qsort(索引, 件数, sizeof(索引[0]), NOTICE_ID比較);

    for (i = 1; i < 件数; i++) {
        if (strcmp(索引[i - 1].notice_id, 索引[i].notice_id) == 0) {
            fprintf(stderr, "W305 行%lu NOTICE-ID重複先行検出\n", 索引[i].行番号);
        }
    }

    for (i = 0; i < 件数; i++) {
        NOTICE_ID索引 一時索引[最大通知件数];
        size_t j;
        size_t n = 0;
        int 再送;

        for (j = 0; j < 件数; j++) {
            if (j != i) {
                一時索引[n++] = 索引[j];
            }
        }

        再送 = strcmp(通知一覧[i].send_status, 送信待ち) == 0 &&
               !通知検査(&通知一覧[i], 一時索引, n, (unsigned long)(i + 1));

        if (!スプール出力(stdout, &通知一覧[i], 再送)) {
            fprintf(stderr, "E004 出力書込失敗\n");
            return MIPAY_RC_IO;
        }
    }

    if (fflush(stdout) == EOF) {
        fprintf(stderr, "E005 出力確定失敗\n");
        return MIPAY_RC_IO;
    }

    return MIPAY_RC_OK;
}
