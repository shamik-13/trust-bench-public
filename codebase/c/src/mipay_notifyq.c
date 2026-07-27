/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240708  精算基盤  初版作成
 * 1.01  20241209  精算基盤  通知重複判定と停止加盟店除外を追加
 */

#include "mipay_settle.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define 入力精算ファイル "PSSETF.csv"
#define 入力帳票ファイル "PSRPTF.csv"
#define 入力加盟店ファイル "PSMERF.csv"
#define 出力通知ファイル "PSNTFF.csv"

#define 正常終了 0
#define 異常終了 8

#define 最大行長 2048
#define 最大加盟店数 20000
#define 最大通知数 60000
#define 加盟店コード長 32
#define 精算ＩＤ長 32
#define 帳票ＩＤ長 32
#define 日付長 16
#define パス長 512
#define 状態長 8
#define 通知区分長 8

typedef struct {
    char 精算ＩＤ[精算ＩＤ長];
    char 加盟店コード[加盟店コード長];
    long long 純額;
    long long 手数料額;
    long long 振込額;
    char 精算日[日付長];
} 精算行;

typedef struct {
    char 帳票ＩＤ[帳票ＩＤ長];
    char 加盟店コード[加盟店コード長];
    char 帳票区分[通知区分長];
    char 期間自[日付長];
    char 期間至[日付長];
    char 出力パス[パス長];
    char 作成状態[状態長];
} 帳票行;

typedef struct {
    char 加盟店コード[加盟店コード長];
    char 加盟店名[128];
    char 加盟店状態[状態長];
    char 銀行口座[64];
} 加盟店行;

typedef struct {
    char 通知ＩＤ[48];
    char 加盟店コード[加盟店コード長];
    char 通知区分[通知区分長];
    char 精算ＩＤ[精算ＩＤ長];
    char 送信状態[状態長];
    char 送信時刻[日付長];
} 通知行;

static 加盟店行 加盟店表[最大加盟店数];
static size_t 加盟店件数;
static 通知行 通知表[最大通知数];
static size_t 通知件数;

static void 改行除去(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int 文字列写像(char *dst, size_t dstsz, const char *src)
{
    size_t n;

    if (dstsz == 0 || src == NULL) {
        return -1;
    }
    n = strlen(src);
    if (n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int 次項目(char **cur, char *dst, size_t dstsz)
{
    char *p = *cur;
    char *w = dst;
    size_t 残 = dstsz;

    if (p == NULL || dst == NULL || dstsz == 0) {
        return -1;
    }

    if (*p == '"') {
        ++p;
        while (*p != '\0') {
            if (*p == '"' && p[1] == '"') {
                if (残 <= 1) {
                    return -1;
                }
                *w++ = '"';
                --残;
                p += 2;
            } else if (*p == '"') {
                ++p;
                break;
            } else {
                if (残 <= 1) {
                    return -1;
                }
                *w++ = *p++;
                --残;
            }
        }
        if (*p == ',') {
            ++p;
        } else if (*p != '\0') {
            return -1;
        }
    } else {
        while (*p != '\0' && *p != ',') {
            if (残 <= 1) {
                return -1;
            }
            *w++ = *p++;
            --残;
        }
        if (*p == ',') {
            ++p;
        }
    }

    *w = '\0';
    *cur = p;
    return 0;
}

static int 金額変換(const char *s, long long *out)
{
    char *end;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }
    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || *end != '\0') {
        return -1;
    }
    *out = v;
    return 0;
}

static int 日付検査(const char *s)
{
    size_t i;

    if (strlen(s) != 8) {
        return -1;
    }
    for (i = 0; i < 8; ++i) {
        if (s[i] < '0' || s[i] > '9') {
            return -1;
        }
    }
    return 0;
}

static const 加盟店行 *加盟店検索(const char *code)
{
    size_t i;

    for (i = 0; i < 加盟店件数; ++i) {
        if (strcmp(加盟店表[i].加盟店コード, code) == 0) {
            return &加盟店表[i];
        }
    }
    return NULL;
}

static int 通知重複あり(const char *settle_id, const char *notice_kbn)
{
    size_t i;

    for (i = 0; i < 通知件数; ++i) {
        if (strcmp(通知表[i].精算ＩＤ, settle_id) == 0 &&
            strcmp(通知表[i].通知区分, notice_kbn) == 0) {
            return 1;
        }
    }
    return 0;
}

static int 通知追加(const char *merchant, const char *notice_kbn, const char *settle_id, const char *send_at)
{
    通知行 *n;
    int len;

    if (通知件数 >= 最大通知数 || 通知重複あり(settle_id, notice_kbn)) {
        return 通知件数 >= 最大通知数 ? -1 : 0;
    }

    n = &通知表[通知件数];
    len = snprintf(n->通知ＩＤ, sizeof(n->通知ＩＤ), "NT%010lu", (unsigned long)(通知件数 + 1));
    if (len < 0 || (size_t)len >= sizeof(n->通知ＩＤ)) {
        return -1;
    }
    if (文字列写像(n->加盟店コード, sizeof(n->加盟店コード), merchant) != 0 ||
        文字列写像(n->通知区分, sizeof(n->通知区分), notice_kbn) != 0 ||
        文字列写像(n->精算ＩＤ, sizeof(n->精算ＩＤ), settle_id) != 0 ||
        文字列写像(n->送信状態, sizeof(n->送信状態), "00") != 0 ||
        文字列写像(n->送信時刻, sizeof(n->送信時刻), send_at) != 0) {
        return -1;
    }

    ++通知件数;
    return 0;
}

static int 加盟店読込(void)
{
    FILE *fp = fopen(入力加盟店ファイル, "r");
    char line[最大行長];

    if (fp == NULL) {
        fprintf(stderr, "E001:加盟店ファイルを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *p;
        加盟店行 m;

        改行除去(line);
        if (line[0] == '\0' || strncmp(line, "MERCHANT-CODE", 13) == 0) {
            continue;
        }
        if (加盟店件数 >= 最大加盟店数) {
            fprintf(stderr, "E002:加盟店件数が上限を超過しました\n");
            fclose(fp);
            return -1;
        }

        p = line;
        memset(&m, 0, sizeof(m));
        if (次項目(&p, m.加盟店コード, sizeof(m.加盟店コード)) != 0 ||
            次項目(&p, m.加盟店名, sizeof(m.加盟店名)) != 0 ||
            次項目(&p, m.加盟店状態, sizeof(m.加盟店状態)) != 0 ||
            次項目(&p, m.銀行口座, sizeof(m.銀行口座)) != 0) {
            fprintf(stderr, "E003:加盟店ファイルの形式が不正です\n");
            fclose(fp);
            return -1;
        }
        if (m.加盟店コード[0] == '\0' || m.銀行口座[0] == '\0') {
            fprintf(stderr, "E004:加盟店ファイルの必須項目が空です\n");
            fclose(fp);
            return -1;
        }
        if (strcmp(m.加盟店状態, "01") != 0 &&
            strcmp(m.加盟店状態, "02") != 0 &&
            strcmp(m.加盟店状態, "09") != 0) {
            fprintf(stderr, "E005:加盟店状態が不正です\n");
            fclose(fp);
            return -1;
        }
        加盟店表[加盟店件数++] = m;
    }

    if (ferror(fp)) {
        fprintf(stderr, "E006:加盟店ファイルの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }
    fclose(fp);
    return 0;
}

static int 精算処理(void)
{
    FILE *fp = fopen(入力精算ファイル, "r");
    char line[最大行長];

    if (fp == NULL) {
        fprintf(stderr, "E101:精算ファイルを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *p;
        char buf[64];
        精算行 s;
        const 加盟店行 *m;

        改行除去(line);
        if (line[0] == '\0' || strncmp(line, "SETTLE-ID", 9) == 0) {
            continue;
        }

        p = line;
        memset(&s, 0, sizeof(s));
        if (次項目(&p, s.精算ＩＤ, sizeof(s.精算ＩＤ)) != 0 ||
            次項目(&p, s.加盟店コード, sizeof(s.加盟店コード)) != 0 ||
            次項目(&p, buf, sizeof(buf)) != 0 ||
            金額変換(buf, &s.純額) != 0 ||
            次項目(&p, buf, sizeof(buf)) != 0 ||
            金額変換(buf, &s.手数料額) != 0 ||
            次項目(&p, buf, sizeof(buf)) != 0 ||
            金額変換(buf, &s.振込額) != 0 ||
            次項目(&p, s.精算日, sizeof(s.精算日)) != 0 ||
            日付検査(s.精算日) != 0) {
            fprintf(stderr, "E102:精算ファイルの形式が不正です\n");
            fclose(fp);
            return -1;
        }

        if (s.純額 < 0 || s.手数料額 < 0 || s.振込額 < 0 || s.純額 - s.手数料額 != s.振込額) {
            fprintf(stderr, "E103:精算金額の整合性が不正です\n");
            fclose(fp);
            return -1;
        }

        m = 加盟店検索(s.加盟店コード);
        if (m == NULL) {
            fprintf(stderr, "E104:精算対象の加盟店が未登録です\n");
            fclose(fp);
            return -1;
        }
        if (strcmp(m->加盟店状態, "01") == 0) {
            if (通知追加(s.加盟店コード, "01", s.精算ＩＤ, s.精算日) != 0) {
                fprintf(stderr, "E105:精算通知の作成に失敗しました\n");
                fclose(fp);
                return -1;
            }
            if (s.振込額 == 0) {
                if (通知追加(s.加盟店コード, "03", s.精算ＩＤ, s.精算日) != 0) {
                    fprintf(stderr, "E106:振込失敗通知の作成に失敗しました\n");
                    fclose(fp);
                    return -1;
                }
            }
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E107:精算ファイルの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }
    fclose(fp);
    return 0;
}

static int 帳票処理(void)
{
    FILE *fp = fopen(入力帳票ファイル, "r");
    char line[最大行長];

    if (fp == NULL) {
        fprintf(stderr, "E201:帳票ファイルを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *p;
        帳票行 r;
        const 加盟店行 *m;

        改行除去(line);
        if (line[0] == '\0' || strncmp(line, "REPORT-ID", 9) == 0) {
            continue;
        }

        p = line;
        memset(&r, 0, sizeof(r));
        if (次項目(&p, r.帳票ＩＤ, sizeof(r.帳票ＩＤ)) != 0 ||
            次項目(&p, r.加盟店コード, sizeof(r.加盟店コード)) != 0 ||
            次項目(&p, r.帳票区分, sizeof(r.帳票区分)) != 0 ||
            次項目(&p, r.期間自, sizeof(r.期間自)) != 0 ||
            次項目(&p, r.期間至, sizeof(r.期間至)) != 0 ||
            次項目(&p, r.出力パス, sizeof(r.出力パス)) != 0 ||
            次項目(&p, r.作成状態, sizeof(r.作成状態)) != 0 ||
            日付検査(r.期間自) != 0 ||
            日付検査(r.期間至) != 0) {
            fprintf(stderr, "E202:帳票ファイルの形式が不正です\n");
            fclose(fp);
            return -1;
        }

        m = 加盟店検索(r.加盟店コード);
        if (m == NULL) {
            fprintf(stderr, "E203:帳票対象の加盟店が未登録です\n");
            fclose(fp);
            return -1;
        }
        if (strcmp(m->加盟店状態, "01") == 0 && strcmp(r.作成状態, "00") == 0) {
            if (通知追加(r.加盟店コード, "02", r.帳票ＩＤ, r.期間至) != 0) {
                fprintf(stderr, "E204:帳票通知の作成に失敗しました\n");
                fclose(fp);
                return -1;
            }
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E205:帳票ファイルの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }
    fclose(fp);
    return 0;
}

static int 通知書出(void)
{
    FILE *fp = fopen(出力通知ファイル, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "E301:通知ファイルを開けません\n");
        return -1;
    }

    if (fprintf(fp, "NOTICE-ID,MERCHANT-CODE,NOTICE-KBN,SETTLE-ID,SEND-STATUS,SEND-AT\n") < 0) {
        fprintf(stderr, "E302:通知ファイルの書込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    for (i = 0; i < 通知件数; ++i) {
        const 通知行 *n = &通知表[i];
        if (fprintf(fp, "%s,%s,%s,%s,%s,%s\n",
                    n->通知ＩＤ,
                    n->加盟店コード,
                    n->通知区分,
                    n->精算ＩＤ,
                    n->送信状態,
                    n->送信時刻) < 0) {
            fprintf(stderr, "E303:通知ファイルの明細書込に失敗しました\n");
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "E304:通知ファイルの終了処理に失敗しました\n");
        return -1;
    }
    return 0;
}

int main(void)
{
    if (加盟店読込() != 0) {
        return 異常終了;
    }
    if (精算処理() != 0) {
        return 異常終了;
    }
    if (帳票処理() != 0) {
        return 異常終了;
    }
    if (通知書出() != 0) {
        return 異常終了;
    }

    return 正常終了;
}
