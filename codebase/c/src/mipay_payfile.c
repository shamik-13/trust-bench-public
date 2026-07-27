/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240715  決済基盤  初版作成
 * 1.01  20241216  決済基盤  二重出力検査と営業日判定を追加
 */
#include "mipay_settle.h"

#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MIPAY_DECISION_OK
#define MIPAY_DECISION_OK 0
#endif

#ifndef MIPAY_DECISION_REJECT
#define MIPAY_DECISION_REJECT 10
#endif

#ifndef MIPAY_DECISION_ERROR
#define MIPAY_DECISION_ERROR 99
#endif

#define 入力行上限 4096
#define 文字列長 128
#define 金額上限 999999999999LL

typedef struct {
    char 精算id[文字列長];
    char 加盟店コード[文字列長];
    long long 純額;
    long long 手数料;
    long long 支払額;
    char 精算日[16];
} 精算行;

typedef struct {
    char 加盟店コード[文字列長];
    char 加盟店名[文字列長];
    char 状態[4];
    char 口座番号[文字列長];
} 加盟店行;

typedef struct {
    char キー[文字列長];
    char 値[文字列長];
    char 適用日[16];
    char 失効日[16];
    char 更新時刻[32];
} 設定行;

static void 改行除去(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static bool 文字列コピー(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);
    if (n >= dstsz) {
        return false;
    }
    memcpy(dst, src, n + 1);
    return true;
}

static bool 金額変換(const char *s, long long *out)
{
    char *end = NULL;
    long long v;

    if (s[0] == '\0') {
        return false;
    }
    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return false;
    }
    if (v < -金額上限 || v > 金額上限) {
        return false;
    }
    *out = v;
    return true;
}

static bool 日付8桁(const char *s)
{
    size_t i;

    if (strlen(s) != 8) {
        return false;
    }
    for (i = 0; i < 8; i++) {
        if (s[i] < '0' || s[i] > '9') {
            return false;
        }
    }
    return true;
}

static int csv分割(char *line, char **field, int max_field)
{
    int n = 0;
    char *p = line;

    while (n < max_field) {
        field[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return n;
}

static bool 精算行読込(FILE *fp, 精算行 *row, bool *eof)
{
    char line[入力行上限];
    char *f[6];

    *eof = false;
    if (fgets(line, sizeof line, fp) == NULL) {
        if (ferror(fp)) {
            return false;
        }
        *eof = true;
        return true;
    }
    改行除去(line);
    if (csv分割(line, f, 6) != 6) {
        return false;
    }
    if (!文字列コピー(row->精算id, sizeof row->精算id, f[0]) ||
        !文字列コピー(row->加盟店コード, sizeof row->加盟店コード, f[1]) ||
        !金額変換(f[2], &row->純額) ||
        !金額変換(f[3], &row->手数料) ||
        !金額変換(f[4], &row->支払額) ||
        !文字列コピー(row->精算日, sizeof row->精算日, f[5]) ||
        !日付8桁(row->精算日)) {
        return false;
    }
    return true;
}

static bool 加盟店検索(const char *加盟店コード, 加盟店行 *out)
{
    FILE *fp = fopen("PSMERF.csv", "r");
    char line[入力行上限];

    if (fp == NULL) {
        return false;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[4];
        改行除去(line);
        if (csv分割(line, f, 4) != 4) {
            fclose(fp);
            return false;
        }
        if (strcmp(f[0], 加盟店コード) == 0) {
            bool ok = 文字列コピー(out->加盟店コード, sizeof out->加盟店コード, f[0]) &&
                      文字列コピー(out->加盟店名, sizeof out->加盟店名, f[1]) &&
                      文字列コピー(out->状態, sizeof out->状態, f[2]) &&
                      文字列コピー(out->口座番号, sizeof out->口座番号, f[3]);
            fclose(fp);
            return ok;
        }
    }
    fclose(fp);
    return false;
}

static bool 設定値取得(const char *key, const char *対象日, char *value, size_t valuesz)
{
    FILE *fp = fopen("PSCONF.csv", "r");
    char line[入力行上限];
    設定行 best;
    bool found = false;

    if (fp == NULL) {
        return false;
    }
    memset(&best, 0, sizeof best);
    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[5];
        設定行 cur;

        改行除去(line);
        if (csv分割(line, f, 5) != 5) {
            fclose(fp);
            return false;
        }
        if (!文字列コピー(cur.キー, sizeof cur.キー, f[0]) ||
            !文字列コピー(cur.値, sizeof cur.値, f[1]) ||
            !文字列コピー(cur.適用日, sizeof cur.適用日, f[2]) ||
            !文字列コピー(cur.失効日, sizeof cur.失効日, f[3]) ||
            !文字列コピー(cur.更新時刻, sizeof cur.更新時刻, f[4]) ||
            !日付8桁(cur.適用日) ||
            !日付8桁(cur.失効日)) {
            fclose(fp);
            return false;
        }
        if (strcmp(cur.キー, key) == 0 &&
            strcmp(cur.適用日, 対象日) <= 0 &&
            strcmp(対象日, cur.失効日) <= 0 &&
            (!found || strcmp(cur.更新時刻, best.更新時刻) > 0)) {
            best = cur;
            found = true;
        }
    }
    fclose(fp);
    return found && 文字列コピー(value, valuesz, best.値);
}

static bool 口座有効(const char *acct)
{
    size_t i, n = strlen(acct);

    if (n < 7 || n > 32) {
        return false;
    }
    for (i = 0; i < n; i++) {
        if (acct[i] < '0' || acct[i] > '9') {
            return false;
        }
    }
    return true;
}

static bool 二重出力済み(const char *精算id)
{
    FILE *fp = fopen("PSPAYF.csv", "r");
    char line[入力行上限];
    bool dup = false;

    if (fp == NULL) {
        return false;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[6];
        改行除去(line);
        if (csv分割(line, f, 6) == 6 && strcmp(f[0], 精算id) == 0) {
            dup = true;
            break;
        }
    }
    fclose(fp);
    return dup;
}

static bool 支払ファイル出力(FILE *out, const 精算行 *s, const 加盟店行 *m)
{
    if (fprintf(out, "%s,%s,%s,%lld,%s,%s\n",
                s->精算id,
                s->加盟店コード,
                m->口座番号,
                s->支払額,
                s->精算日,
                "00") < 0) {
        return false;
    }
    return true;
}

int main(void)
{
    FILE *in = fopen("PSSETF.csv", "r");
    FILE *out;
    int decision = MIPAY_DECISION_OK;
    bool eof = false;

    if (in == NULL) {
        fprintf(stderr, "E001:PSSETF読込失敗\n");
        return MIPAY_DECISION_ERROR;
    }

    out = fopen("PSPAYF.csv", "a");
    if (out == NULL) {
        fclose(in);
        fprintf(stderr, "E002:PSPAYF出力開始失敗\n");
        return MIPAY_DECISION_ERROR;
    }

    while (!eof) {
        精算行 s;
        加盟店行 m;
        char 営業日[文字列長];
        char 停止[文字列長];

        if (!精算行読込(in, &s, &eof)) {
            fclose(out);
            fclose(in);
            fprintf(stderr, "E003:PSSETF形式不正\n");
            return MIPAY_DECISION_ERROR;
        }
        if (eof) {
            break;
        }

        if (s.支払額 <= 0) {
            decision = MIPAY_DECISION_REJECT;
            continue;
        }
        if (!設定値取得("BANK_BUSINESS_DAY", s.精算日, 営業日, sizeof 営業日) ||
            strcmp(営業日, "1") != 0) {
            decision = MIPAY_DECISION_REJECT;
            continue;
        }
        if (!設定値取得("PAYOUT_STOP_FLAG", s.精算日, 停止, sizeof 停止) ||
            strcmp(停止, "1") == 0) {
            decision = MIPAY_DECISION_REJECT;
            continue;
        }
        if (二重出力済み(s.精算id)) {
            decision = MIPAY_DECISION_REJECT;
            continue;
        }
        if (!加盟店検索(s.加盟店コード, &m)) {
            decision = MIPAY_DECISION_REJECT;
            continue;
        }
        if (strcmp(m.状態, "01") != 0 || !口座有効(m.口座番号)) {
            decision = MIPAY_DECISION_REJECT;
            continue;
        }
        if (!支払ファイル出力(out, &s, &m)) {
            fclose(out);
            fclose(in);
            fprintf(stderr, "E004:PSPAYF書込失敗\n");
            return MIPAY_DECISION_ERROR;
        }
    }

    if (fclose(out) != 0) {
        fclose(in);
        fprintf(stderr, "E005:PSPAYF確定失敗\n");
        return MIPAY_DECISION_ERROR;
    }
    if (fclose(in) != 0) {
        fprintf(stderr, "E006:PSSETF終了失敗\n");
        return MIPAY_DECISION_ERROR;
    }
    return decision;
}
