/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240722  決済基盤  初版作成。銀行結果取込と支払・入金候補更新を実装。
 * 1.01  20241223  決済基盤  金額桁あふれ検査と重複振込ID検査を追加。
 */

#include "mipay_settle.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MIPAY_DECISION_OK
#define MIPAY_DECISION_OK 0
#endif

#ifndef MIPAY_DECISION_IO_ERROR
#define MIPAY_DECISION_IO_ERROR 12
#endif

#ifndef MIPAY_DECISION_PARSE_ERROR
#define MIPAY_DECISION_PARSE_ERROR 16
#endif

#define 入力行最大 1024
#define 識別子最大 64
#define 加盟店最大 32
#define 口座最大 48
#define 日付最大 16
#define 状態最大 8

typedef struct {
    char payout_id[識別子最大];
    char merchant_code[加盟店最大];
    char bank_acct_no[口座最大];
    long long payout_amt;
    char payout_dt[日付最大];
    char bank_result_cd[状態最大];
} 支払行;

typedef struct {
    char payout_id[識別子最大];
    char result_cd[状態最大];
    char bank_dt[日付最大];
} 銀行結果行;

typedef struct {
    支払行 *rows;
    size_t used;
    size_t cap;
} 支払表;

typedef struct {
    銀行結果行 *rows;
    size_t used;
    size_t cap;
} 銀行結果表;

static void 改行除去(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static char *前後空白除去(char *s)
{
    char *end;

    while (*s != '\0' && isspace((unsigned char)*s)) {
        ++s;
    }

    end = s + strlen(s);
    while (end > s && isspace((unsigned char)end[-1])) {
        *--end = '\0';
    }

    return s;
}

static int 文字列複写(char *dst, size_t dstsz, const char *src, const char *項目名, size_t 行番号)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dstsz) {
        fprintf(stderr, "%s:%zu:項目長不正\n", 項目名, 行番号);
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static int 金額変換(const char *s, long long *out, size_t 行番号)
{
    char *end = NULL;
    long long v;

    if (*s == '\0' || *s == '-') {
        fprintf(stderr, "金額:%zu:形式不正\n", 行番号);
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno == ERANGE || end == s || *end != '\0' || v <= 0) {
        fprintf(stderr, "金額:%zu:範囲不正\n", 行番号);
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
        if (!isdigit((unsigned char)s[i])) {
            return -1;
        }
    }

    return 0;
}

static int 支払表追加(支払表 *tbl, const 支払行 *row)
{
    支払行 *p;
    size_t next;

    if (tbl->used == tbl->cap) {
        next = tbl->cap == 0 ? 256u : tbl->cap * 2u;
        if (next < tbl->cap || next > SIZE_MAX / sizeof(*tbl->rows)) {
            fprintf(stderr, "支払表:容量超過\n");
            return -1;
        }

        p = (支払行 *)realloc(tbl->rows, next * sizeof(*tbl->rows));
        if (p == NULL) {
            fprintf(stderr, "支払表:記憶域不足\n");
            return -1;
        }

        tbl->rows = p;
        tbl->cap = next;
    }

    tbl->rows[tbl->used++] = *row;
    return 0;
}

static int 銀行結果表追加(銀行結果表 *tbl, const 銀行結果行 *row)
{
    銀行結果行 *p;
    size_t next;

    if (tbl->used == tbl->cap) {
        next = tbl->cap == 0 ? 256u : tbl->cap * 2u;
        if (next < tbl->cap || next > SIZE_MAX / sizeof(*tbl->rows)) {
            fprintf(stderr, "銀行結果表:容量超過\n");
            return -1;
        }

        p = (銀行結果行 *)realloc(tbl->rows, next * sizeof(*tbl->rows));
        if (p == NULL) {
            fprintf(stderr, "銀行結果表:記憶域不足\n");
            return -1;
        }

        tbl->rows = p;
        tbl->cap = next;
    }

    tbl->rows[tbl->used++] = *row;
    return 0;
}

static int 支払行解析(char *line, 支払行 *row, size_t 行番号)
{
    char *tok[6];
    char *cur = line;
    size_t i;

    for (i = 0; i < 6; ++i) {
        tok[i] = strsep(&cur, ",");
        if (tok[i] == NULL) {
            fprintf(stderr, "PSPAYF:%zu:項目不足\n", 行番号);
            return -1;
        }
        tok[i] = 前後空白除去(tok[i]);
    }

    if (cur != NULL) {
        fprintf(stderr, "PSPAYF:%zu:項目過多\n", 行番号);
        return -1;
    }

    if (文字列複写(row->payout_id, sizeof(row->payout_id), tok[0], "PAYOUT-ID", 行番号) != 0 ||
        文字列複写(row->merchant_code, sizeof(row->merchant_code), tok[1], "MERCHANT-CODE", 行番号) != 0 ||
        文字列複写(row->bank_acct_no, sizeof(row->bank_acct_no), tok[2], "BANK-ACCT-NO", 行番号) != 0 ||
        金額変換(tok[3], &row->payout_amt, 行番号) != 0 ||
        文字列複写(row->payout_dt, sizeof(row->payout_dt), tok[4], "PAYOUT-DT", 行番号) != 0 ||
        文字列複写(row->bank_result_cd, sizeof(row->bank_result_cd), tok[5], "BANK-RESULT-CD", 行番号) != 0) {
        return -1;
    }

    if (日付検査(row->payout_dt) != 0) {
        fprintf(stderr, "PAYOUT-DT:%zu:日付不正\n", 行番号);
        return -1;
    }

    return 0;
}

static int 銀行結果行解析(char *line, 銀行結果行 *row, size_t 行番号)
{
    char *tok[3];
    char *cur = line;
    size_t i;

    for (i = 0; i < 3; ++i) {
        tok[i] = strsep(&cur, ",");
        if (tok[i] == NULL) {
            fprintf(stderr, "銀行結果:%zu:項目不足\n", 行番号);
            return -1;
        }
        tok[i] = 前後空白除去(tok[i]);
    }

    if (cur != NULL) {
        fprintf(stderr, "銀行結果:%zu:項目過多\n", 行番号);
        return -1;
    }

    if (strcmp(tok[1], "00") != 0 && strcmp(tok[1], "51") != 0 && strcmp(tok[1], "80") != 0) {
        fprintf(stderr, "銀行結果:%zu:結果コード不正\n", 行番号);
        return -1;
    }

    if (文字列複写(row->payout_id, sizeof(row->payout_id), tok[0], "PAYOUT-ID", 行番号) != 0 ||
        文字列複写(row->result_cd, sizeof(row->result_cd), tok[1], "BANK-RESULT-CD", 行番号) != 0 ||
        文字列複写(row->bank_dt, sizeof(row->bank_dt), tok[2], "RESULT-DT", 行番号) != 0) {
        return -1;
    }

    if (日付検査(row->bank_dt) != 0) {
        fprintf(stderr, "RESULT-DT:%zu:日付不正\n", 行番号);
        return -1;
    }

    return 0;
}

static int 支払読込(const char *path, 支払表 *tbl)
{
    FILE *fp = fopen(path, "r");
    char line[入力行最大];
    size_t 行番号 = 0;

    if (fp == NULL) {
        fprintf(stderr, "PSPAYF:入力オープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        支払行 row;

        ++行番号;
        改行除去(line);
        if (line[0] == '\0') {
            continue;
        }

        if (支払行解析(line, &row, 行番号) != 0 || 支払表追加(tbl, &row) != 0) {
            fclose(fp);
            return -1;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "PSPAYF:入力読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int 銀行結果読込(const char *path, 銀行結果表 *tbl)
{
    FILE *fp = fopen(path, "r");
    char line[入力行最大];
    size_t 行番号 = 0;

    if (fp == NULL) {
        fprintf(stderr, "銀行結果:入力オープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        銀行結果行 row;

        ++行番号;
        改行除去(line);
        if (line[0] == '\0') {
            continue;
        }

        if (銀行結果行解析(line, &row, 行番号) != 0 || 銀行結果表追加(tbl, &row) != 0) {
            fclose(fp);
            return -1;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "銀行結果:入力読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static 支払行 *支払検索(支払表 *tbl, const char *payout_id)
{
    size_t i;
    支払行 *found = NULL;

    for (i = 0; i < tbl->used; ++i) {
        if (strcmp(tbl->rows[i].payout_id, payout_id) == 0) {
            if (found != NULL) {
                fprintf(stderr, "PSPAYF:振込ID重複\n");
                return NULL;
            }
            found = &tbl->rows[i];
        }
    }

    return found;
}

static int 突合更新(支払表 *pay, const 銀行結果表 *ack)
{
    size_t i;

    for (i = 0; i < ack->used; ++i) {
        支払行 *row = 支払検索(pay, ack->rows[i].payout_id);

        if (row == NULL) {
            fprintf(stderr, "銀行結果:振込ID未検出\n");
            return -1;
        }

        if (strcmp(ack->rows[i].result_cd, "00") == 0) {
            strcpy(row->bank_result_cd, "00");
        } else if (strcmp(ack->rows[i].result_cd, "51") == 0) {
            strcpy(row->bank_result_cd, "51");
        } else {
            strcpy(row->bank_result_cd, "80");
        }
    }

    return 0;
}

static int 支払書込(const char *path, const 支払表 *tbl)
{
    FILE *fp = fopen(path, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "PSPAYF:出力オープン失敗\n");
        return -1;
    }

    for (i = 0; i < tbl->used; ++i) {
        const 支払行 *r = &tbl->rows[i];

        if (fprintf(fp, "%s,%s,%s,%lld,%s,%s\n",
                    r->payout_id,
                    r->merchant_code,
                    r->bank_acct_no,
                    r->payout_amt,
                    r->payout_dt,
                    r->bank_result_cd) < 0) {
            fprintf(stderr, "PSPAYF:出力書込失敗\n");
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "PSPAYF:出力クローズ失敗\n");
        return -1;
    }

    return 0;
}

static int 入金候補書込(const char *path, const 支払表 *tbl)
{
    FILE *fp = fopen(path, "w");
    size_t i;
    unsigned long long seq = 1;

    if (fp == NULL) {
        fprintf(stderr, "PSRCVF:出力オープン失敗\n");
        return -1;
    }

    for (i = 0; i < tbl->used; ++i) {
        const 支払行 *r = &tbl->rows[i];

        if (strcmp(r->bank_result_cd, "00") != 0) {
            continue;
        }

        if (fprintf(fp, "RCV%012llu,%s,%lld,%s,M,%s\n",
                    seq++,
                    r->merchant_code,
                    r->payout_amt,
                    r->payout_dt,
                    r->payout_id) < 0) {
            fprintf(stderr, "PSRCVF:出力書込失敗\n");
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "PSRCVF:出力クローズ失敗\n");
        return -1;
    }

    return 0;
}

int main(void)
{
    const char *pspay_in = getenv("MIPAY_PSPAYF_IN");
    const char *bank_in = getenv("MIPAY_BANKACK_IN");
    const char *pspay_out = getenv("MIPAY_PSPAYF_OUT");
    const char *psrcv_out = getenv("MIPAY_PSRCVF_OUT");
    支払表 pay = {0};
    銀行結果表 ack = {0};
    int rc = MIPAY_DECISION_OK;

    if (pspay_in == NULL) {
        pspay_in = "PSPAYF.csv";
    }
    if (bank_in == NULL) {
        bank_in = "BANKACK.csv";
    }
    if (pspay_out == NULL) {
        pspay_out = "PSPAYF.out.csv";
    }
    if (psrcv_out == NULL) {
        psrcv_out = "PSRCVF.csv";
    }

    if (支払読込(pspay_in, &pay) != 0 || 銀行結果読込(bank_in, &ack) != 0) {
        rc = MIPAY_DECISION_PARSE_ERROR;
        goto 終了;
    }

    if (突合更新(&pay, &ack) != 0) {
        rc = MIPAY_DECISION_PARSE_ERROR;
        goto 終了;
    }

    if (支払書込(pspay_out, &pay) != 0 || 入金候補書込(psrcv_out, &pay) != 0) {
        rc = MIPAY_DECISION_IO_ERROR;
        goto 終了;
    }

終了:
    free(pay.rows);
    free(ack.rows);
    return rc;
}
