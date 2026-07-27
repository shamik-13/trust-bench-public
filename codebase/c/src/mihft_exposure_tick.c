/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    20240213    市場基盤部  初版作成。ティック別評価額を顧客単位へ集約。
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define 入力行_MAX 1024
#define CIF_NO_MAX 32
#define INSTR_CODE_MAX 32
#define SESS_DT_MAX 16
#define 集約_MAX 65536
#define 建玉_MAX 262144
#define 判定_ACCEPT 0
#define 異常終了_IO 20
#define 異常終了_PARSE 24
#define 異常終了_OVERFLOW 28

typedef struct {
    char cif_no[CIF_NO_MAX];
    char instr_code[INSTR_CODE_MAX];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
} scposf_rec_t;

typedef struct {
    char cif_no[CIF_NO_MAX];
    char instr_code[INSTR_CODE_MAX];
    char sess_dt[SESS_DT_MAX];
    int64_t net_qty;
    int64_t mark_amt;
    int64_t mark_notional_amt;
    int64_t unrlzd_amt;
} scm2mf_rec_t;

typedef struct {
    char cif_no[CIF_NO_MAX];
    char sess_dt[SESS_DT_MAX];
    int64_t gross_long_amt;
    int64_t gross_short_amt;
    int64_t net_exposure_amt;
    int64_t limit_util_pct;
} scexpf_rec_t;

typedef struct {
    char cif_no[CIF_NO_MAX];
    char instr_code[INSTR_CODE_MAX];
    int present;
} pos_key_t;

static void 改行除去(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int 見出し行判定(const char *s)
{
    return strstr(s, "CIF-NO") != NULL || strstr(s, "CIF_NO") != NULL;
}

static int 文字列複写(char *dst, size_t dst_sz, const char *src)
{
    size_t len = strlen(src);
    if (len == 0 || len >= dst_sz) {
        return -1;
    }
    memcpy(dst, src, len + 1);
    return 0;
}

static int 金額読取(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (*s == '\0') {
        return -1;
    }
    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }
    *out = (int64_t)v;
    return 0;
}

static int 金額加算(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int 金額減算(int64_t a, int64_t b, int64_t *out)
{
    if ((b < 0 && a > INT64_MAX + b) || (b > 0 && a < INT64_MIN + b)) {
        return -1;
    }
    *out = a - b;
    return 0;
}

static int 金額乗算100(int64_t a, int64_t *out)
{
    if (a > INT64_MAX / 100 || a < INT64_MIN / 100) {
        return -1;
    }
    *out = a * 100;
    return 0;
}

static int scposf_parse(char *line, scposf_rec_t *rec)
{
    char *tok[5];
    size_t i;

    for (i = 0; i < 5; i++) {
        tok[i] = strtok(i == 0 ? line : NULL, ",");
        if (tok[i] == NULL) {
            return -1;
        }
    }
    if (strtok(NULL, ",") != NULL) {
        return -1;
    }
    if (文字列複写(rec->cif_no, sizeof(rec->cif_no), tok[0]) != 0) {
        return -1;
    }
    if (文字列複写(rec->instr_code, sizeof(rec->instr_code), tok[1]) != 0) {
        return -1;
    }
    if (金額読取(tok[2], &rec->net_qty) != 0 ||
        金額読取(tok[3], &rec->avg_amt) != 0 ||
        金額読取(tok[4], &rec->rlzd_amt) != 0) {
        return -1;
    }
    return 0;
}

static int scm2mf_parse(char *line, scm2mf_rec_t *rec)
{
    char *tok[7];
    size_t i;

    for (i = 0; i < 7; i++) {
        tok[i] = strtok(i == 0 ? line : NULL, ",");
        if (tok[i] == NULL) {
            return -1;
        }
    }
    if (strtok(NULL, ",") != NULL) {
        return -1;
    }
    if (文字列複写(rec->cif_no, sizeof(rec->cif_no), tok[0]) != 0 ||
        文字列複写(rec->instr_code, sizeof(rec->instr_code), tok[1]) != 0 ||
        文字列複写(rec->sess_dt, sizeof(rec->sess_dt), tok[2]) != 0) {
        return -1;
    }
    if (金額読取(tok[3], &rec->net_qty) != 0 ||
        金額読取(tok[4], &rec->mark_amt) != 0 ||
        金額読取(tok[5], &rec->mark_notional_amt) != 0 ||
        金額読取(tok[6], &rec->unrlzd_amt) != 0) {
        return -1;
    }
    return 0;
}

static int 建玉登録(pos_key_t *keys, size_t *count, const scposf_rec_t *rec)
{
    size_t i;

    if (rec->net_qty == 0) {
        return 0;
    }
    for (i = 0; i < *count; i++) {
        if (strcmp(keys[i].cif_no, rec->cif_no) == 0 &&
            strcmp(keys[i].instr_code, rec->instr_code) == 0) {
            keys[i].present = 1;
            return 0;
        }
    }
    if (*count >= 建玉_MAX) {
        return -1;
    }
    if (文字列複写(keys[*count].cif_no, sizeof(keys[*count].cif_no), rec->cif_no) != 0 ||
        文字列複写(keys[*count].instr_code, sizeof(keys[*count].instr_code), rec->instr_code) != 0) {
        return -1;
    }
    keys[*count].present = 1;
    (*count)++;
    return 0;
}

static int 建玉存在(const pos_key_t *keys, size_t count, const scm2mf_rec_t *rec)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(keys[i].cif_no, rec->cif_no) == 0 &&
            strcmp(keys[i].instr_code, rec->instr_code) == 0) {
            return keys[i].present;
        }
    }
    return 0;
}

static int 集約取得(scexpf_rec_t *rows, size_t *count, const char *cif_no, const char *sess_dt, scexpf_rec_t **out)
{
    size_t i;

    for (i = 0; i < *count; i++) {
        if (strcmp(rows[i].cif_no, cif_no) == 0 && strcmp(rows[i].sess_dt, sess_dt) == 0) {
            *out = &rows[i];
            return 0;
        }
    }
    if (*count >= 集約_MAX) {
        return -1;
    }
    memset(&rows[*count], 0, sizeof(rows[*count]));
    if (文字列複写(rows[*count].cif_no, sizeof(rows[*count].cif_no), cif_no) != 0 ||
        文字列複写(rows[*count].sess_dt, sizeof(rows[*count].sess_dt), sess_dt) != 0) {
        return -1;
    }
    *out = &rows[*count];
    (*count)++;
    return 0;
}

static int 評価額反映(scexpf_rec_t *agg, int64_t mark_notional_amt)
{
    int64_t scaled;
    int64_t next;

    if (金額乗算100(mark_notional_amt, &scaled) != 0) {
        return -1;
    }
    if (scaled >= 0) {
        if (金額加算(agg->gross_long_amt, scaled, &next) != 0) {
            return -1;
        }
        agg->gross_long_amt = next;
    } else {
        if (scaled == INT64_MIN || 金額加算(agg->gross_short_amt, -scaled, &next) != 0) {
            return -1;
        }
        agg->gross_short_amt = next;
    }
    if (金額減算(agg->gross_long_amt, agg->gross_short_amt, &agg->net_exposure_amt) != 0) {
        return -1;
    }
    return 0;
}

static int 利用率算出(scexpf_rec_t *agg)
{
    int64_t gross;
    int64_t limit_scaled;

    if (金額加算(agg->gross_long_amt, agg->gross_short_amt, &gross) != 0 ||
        金額乗算100((int64_t)MIHFT_MAX_NOTIONAL, &limit_scaled) != 0) {
        return -1;
    }
    if (limit_scaled <= 0 || gross > (INT64_MAX / 10000)) {
        return -1;
    }
    agg->limit_util_pct = (gross * 10000 + limit_scaled / 2) / limit_scaled;
    return 0;
}

static int scposf_read(pos_key_t *keys, size_t *key_count)
{
    FILE *fp = fopen("SCPOSF.csv", "r");
    char line[入力行_MAX];
    unsigned long line_no = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCPOSFオープン失敗\n");
        return 異常終了_IO;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        scposf_rec_t rec;

        line_no++;
        改行除去(line);
        if (line[0] == '\0' || 見出し行判定(line)) {
            continue;
        }
        if (scposf_parse(line, &rec) != 0) {
            fprintf(stderr, "SCPOSF形式不正 行=%lu\n", line_no);
            fclose(fp);
            return 異常終了_PARSE;
        }
        if (建玉登録(keys, key_count, &rec) != 0) {
            fprintf(stderr, "SCPOSF建玉登録失敗 行=%lu\n", line_no);
            fclose(fp);
            return 異常終了_OVERFLOW;
        }
    }
    if (ferror(fp)) {
        fprintf(stderr, "SCPOSF読込失敗\n");
        fclose(fp);
        return 異常終了_IO;
    }
    fclose(fp);
    return 判定_ACCEPT;
}

static int scm2mf_read(const pos_key_t *keys, size_t key_count, scexpf_rec_t *aggs, size_t *agg_count)
{
    FILE *fp = fopen("SCM2MF.csv", "r");
    char line[入力行_MAX];
    unsigned long line_no = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCM2MFオープン失敗\n");
        return 異常終了_IO;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        scm2mf_rec_t rec;
        scexpf_rec_t *agg;

        line_no++;
        改行除去(line);
        if (line[0] == '\0' || 見出し行判定(line)) {
            continue;
        }
        if (scm2mf_parse(line, &rec) != 0) {
            fprintf(stderr, "SCM2MF形式不正 行=%lu\n", line_no);
            fclose(fp);
            return 異常終了_PARSE;
        }
        if (!建玉存在(keys, key_count, &rec) && rec.net_qty == 0) {
            continue;
        }
        if (集約取得(aggs, agg_count, rec.cif_no, rec.sess_dt, &agg) != 0) {
            fprintf(stderr, "SCM2MF集約領域不足 行=%lu\n", line_no);
            fclose(fp);
            return 異常終了_OVERFLOW;
        }
        if (評価額反映(agg, rec.mark_notional_amt) != 0) {
            fprintf(stderr, "SCM2MF評価額桁あふれ 行=%lu\n", line_no);
            fclose(fp);
            return 異常終了_OVERFLOW;
        }
    }
    if (ferror(fp)) {
        fprintf(stderr, "SCM2MF読込失敗\n");
        fclose(fp);
        return 異常終了_IO;
    }
    fclose(fp);
    return 判定_ACCEPT;
}

static int scexpf_write(scexpf_rec_t *aggs, size_t agg_count)
{
    FILE *fp = fopen("SCEXPF.csv", "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "SCEXPFオープン失敗\n");
        return 異常終了_IO;
    }
    if (fprintf(fp, "CIF-NO,SESS-DT,GROSS-LONG-AMT,GROSS-SHORT-AMT,NET-EXPOSURE-AMT,LIMIT-UTIL-PCT\n") < 0) {
        fprintf(stderr, "SCEXPF見出し出力失敗\n");
        fclose(fp);
        return 異常終了_IO;
    }
    for (i = 0; i < agg_count; i++) {
        if (利用率算出(&aggs[i]) != 0) {
            fprintf(stderr, "SCEXPF利用率桁あふれ 行=%zu\n", i + 1);
            fclose(fp);
            return 異常終了_OVERFLOW;
        }
        if (fprintf(fp, "%s,%s,%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
                    aggs[i].cif_no,
                    aggs[i].sess_dt,
                    aggs[i].gross_long_amt,
                    aggs[i].gross_short_amt,
                    aggs[i].net_exposure_amt,
                    aggs[i].limit_util_pct) < 0) {
            fprintf(stderr, "SCEXPF明細出力失敗 行=%zu\n", i + 1);
            fclose(fp);
            return 異常終了_IO;
        }
    }
    if (fclose(fp) != 0) {
        fprintf(stderr, "SCEXPFクローズ失敗\n");
        return 異常終了_IO;
    }
    return 判定_ACCEPT;
}

int main(void)
{
    static pos_key_t keys[建玉_MAX];
    static scexpf_rec_t aggs[集約_MAX];
    size_t key_count = 0;
    size_t agg_count = 0;
    int rc;

    rc = scposf_read(keys, &key_count);
    if (rc != 判定_ACCEPT) {
        return rc;
    }
    rc = scm2mf_read(keys, key_count, aggs, &agg_count);
    if (rc != 判定_ACCEPT) {
        return rc;
    }
    rc = scexpf_write(aggs, agg_count);
    if (rc != 判定_ACCEPT) {
        return rc;
    }
    return 判定_ACCEPT;
}
