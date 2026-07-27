/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210715  西村 亮 (E-204)  顧客リスク事前判定の初版作成
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_LINE_MAX 512
#define MIHFT_PATH_SCCUST "sccust.csv"
#define MIHFT_PATH_HFRISKC "hfriskc.csv"
#define MIHFT_PATH_ORDER "orders.csv"
#define MIHFT_PATH_HFDECLOG "hfdeclog.csv"

typedef struct {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    char board_code[4];
    int instr_tier;
    int64_t qty;
    int64_t price_x100;
    int64_t fee_x100;
} ORDERREC;

typedef struct {
    char cif_no[32];
    int64_t group_limit;
    int64_t group_used_amt;
    int64_t acct_used_amt;
} SCCUST_ROW;

typedef struct {
    char cif_no[32];
    char instr_code[32];
    int64_t open_notional_amt;
    int reject_cnt;
    char last_upd_ts[16];
} HFRISKC_ROW;

typedef struct {
    char decision_id[16];
    char order_id[32];
    char instr_code[32];
    int action_code;
    char reason_code[16];
    char decision_ts[16];
} DECLOG_ROW;

static void trim(char *s)
{
    size_t n;
    char *p = s;

    while (*p != '\0' && isspace((unsigned char)*p)) {
        p++;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1U);
    }

    n = strlen(s);
    while (n > 0U && isspace((unsigned char)s[n - 1U])) {
        s[--n] = '\0';
    }
}

static int next_field(char **cur, char *dst, size_t dstsz)
{
    char *p = *cur;
    char *q;
    size_t len;

    if (p == NULL || *p == '\0') {
        return -1;
    }

    q = strchr(p, ',');
    if (q != NULL) {
        len = (size_t)(q - p);
        *cur = q + 1;
    } else {
        len = strlen(p);
        *cur = p + len;
    }

    if (len >= dstsz) {
        return -1;
    }

    memcpy(dst, p, len);
    dst[len] = '\0';
    trim(dst);
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s) {
        return -1;
    }

    while (*end != '\0') {
        if (!isspace((unsigned char)*end)) {
            return -1;
        }
        end++;
    }

    *out = (int64_t)v;
    return 0;
}

static int parse_i32(const char *s, int *out)
{
    char *end;
    long v;

    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || end == s || v < INT32_MIN || v > INT32_MAX) {
        return -1;
    }

    while (*end != '\0') {
        if (!isspace((unsigned char)*end)) {
            return -1;
        }
        end++;
    }

    *out = (int)v;
    return 0;
}

static int add_i64(int64_t a, int64_t b, int64_t *out)
{
    if (b > 0 && a > INT64_MAX - b) {
        return -1;
    }
    if (b < 0 && a < INT64_MIN - b) {
        return -1;
    }

    *out = a + b;
    return 0;
}

static int mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a < 0 || b < 0) {
        return -1;
    }
    if (a != 0 && b > INT64_MAX / a) {
        return -1;
    }

    *out = a * b;
    return 0;
}

static int is_header_line(const char *line)
{
    return strstr(line, "CIF") != NULL || strstr(line, "ORDER") != NULL;
}

static int tier_rate_bp(int tier, int *rate_bp)
{
    if (tier == 1) {
        *rate_bp = 1000;
        return 0;
    }
    if (tier == 2) {
        *rate_bp = 2000;
        return 0;
    }
    if (tier == 3) {
        *rate_bp = 4000;
        return 0;
    }
    return -1;
}

static int tier_tick_x100(int tier, int64_t *tick_x100)
{
    if (tier == 1) {
        *tick_x100 = 100;
        return 0;
    }
    if (tier == 2) {
        *tick_x100 = 500;
        return 0;
    }
    if (tier == 3) {
        *tick_x100 = 1000;
        return 0;
    }
    return -1;
}

static int read_sccust(SCCUST_ROW *rows, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];

    *count = 0U;
    fp = fopen(MIHFT_PATH_SCCUST, "r");
    if (fp == NULL) {
        fputs("SCCUST入力を開けません\n", stderr);
        return 20;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *cur;
        char f0[64], f1[64], f2[64], f3[64];

        trim(line);
        if (line[0] == '\0' || is_header_line(line)) {
            continue;
        }
        if (*count >= cap) {
            fclose(fp);
            fputs("SCCUST件数が上限を超過しました\n", stderr);
            return 21;
        }

        cur = line;
        if (next_field(&cur, f0, sizeof f0) != 0 ||
            next_field(&cur, f1, sizeof f1) != 0 ||
            next_field(&cur, f2, sizeof f2) != 0 ||
            next_field(&cur, f3, sizeof f3) != 0) {
            fclose(fp);
            fputs("SCCUST項目数が不正です\n", stderr);
            return 22;
        }

        memset(&rows[*count], 0, sizeof rows[*count]);
        strncpy(rows[*count].cif_no, f0, sizeof rows[*count].cif_no - 1U);

        if (parse_i64(f1, &rows[*count].group_limit) != 0 ||
            parse_i64(f2, &rows[*count].group_used_amt) != 0 ||
            parse_i64(f3, &rows[*count].acct_used_amt) != 0) {
            fclose(fp);
            fputs("SCCUST金額項目が不正です\n", stderr);
            return 23;
        }

        (*count)++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fputs("SCCUST読込で入出力異常が発生しました\n", stderr);
        return 24;
    }

    fclose(fp);
    return 0;
}

static int read_hfriskc(HFRISKC_ROW *rows, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];

    *count = 0U;
    fp = fopen(MIHFT_PATH_HFRISKC, "r");
    if (fp == NULL) {
        fputs("HFRISKC入力を開けません\n", stderr);
        return 30;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *cur;
        char f0[64], f1[64], f2[64], f3[64], f4[64];

        trim(line);
        if (line[0] == '\0' || is_header_line(line)) {
            continue;
        }
        if (*count >= cap) {
            fclose(fp);
            fputs("HFRISKC件数が上限を超過しました\n", stderr);
            return 31;
        }

        cur = line;
        if (next_field(&cur, f0, sizeof f0) != 0 ||
            next_field(&cur, f1, sizeof f1) != 0 ||
            next_field(&cur, f2, sizeof f2) != 0 ||
            next_field(&cur, f3, sizeof f3) != 0 ||
            next_field(&cur, f4, sizeof f4) != 0) {
            fclose(fp);
            fputs("HFRISKC項目数が不正です\n", stderr);
            return 32;
        }

        memset(&rows[*count], 0, sizeof rows[*count]);
        strncpy(rows[*count].cif_no, f0, sizeof rows[*count].cif_no - 1U);
        strncpy(rows[*count].instr_code, f1, sizeof rows[*count].instr_code - 1U);
        strncpy(rows[*count].last_upd_ts, f4, sizeof rows[*count].last_upd_ts - 1U);

        if (parse_i64(f2, &rows[*count].open_notional_amt) != 0 ||
            parse_i32(f3, &rows[*count].reject_cnt) != 0) {
            fclose(fp);
            fputs("HFRISKC数値項目が不正です\n", stderr);
            return 33;
        }

        (*count)++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fputs("HFRISKC読込で入出力異常が発生しました\n", stderr);
        return 34;
    }

    fclose(fp);
    return 0;
}

static int read_order(FILE *fp, ORDERREC *o)
{
    char line[MIHFT_LINE_MAX];
    char *cur;
    char f0[64], f1[64], f2[64], f3[8], f4[8], f5[8], f6[8], f7[32];
    char f8[64], f9[64], f10[64];

    while (fgets(line, sizeof line, fp) != NULL) {
        trim(line);
        if (line[0] == '\0' || is_header_line(line)) {
            continue;
        }

        cur = line;
        if (next_field(&cur, f0, sizeof f0) != 0 ||
            next_field(&cur, f1, sizeof f1) != 0 ||
            next_field(&cur, f2, sizeof f2) != 0 ||
            next_field(&cur, f3, sizeof f3) != 0 ||
            next_field(&cur, f4, sizeof f4) != 0 ||
            next_field(&cur, f5, sizeof f5) != 0 ||
            next_field(&cur, f6, sizeof f6) != 0 ||
            next_field(&cur, f7, sizeof f7) != 0 ||
            next_field(&cur, f8, sizeof f8) != 0 ||
            next_field(&cur, f9, sizeof f9) != 0 ||
            next_field(&cur, f10, sizeof f10) != 0) {
            fputs("注文項目数が不正です\n", stderr);
            return -2;
        }

        memset(o, 0, sizeof *o);
        strncpy(o->order_id, f0, sizeof o->order_id - 1U);
        strncpy(o->cif_no, f1, sizeof o->cif_no - 1U);
        strncpy(o->instr_code, f2, sizeof o->instr_code - 1U);
        o->side_kbn = f3[0];
        o->ord_type = f4[0];
        strncpy(o->tif_code, f5, sizeof o->tif_code - 1U);
        strncpy(o->board_code, f6, sizeof o->board_code - 1U);

        if (parse_i32(f7, &o->instr_tier) != 0 ||
            parse_i64(f8, &o->qty) != 0 ||
            parse_i64(f9, &o->price_x100) != 0 ||
            parse_i64(f10, &o->fee_x100) != 0) {
            fputs("注文数値項目が不正です\n", stderr);
            return -2;
        }

        return 1;
    }

    if (ferror(fp)) {
        fputs("注文読込で入出力異常が発生しました\n", stderr);
        return -2;
    }

    return 0;
}

static const SCCUST_ROW *find_sccust(const SCCUST_ROW *rows, size_t n, const char *cif_no)
{
    size_t i;

    for (i = 0U; i < n; i++) {
        if (strcmp(rows[i].cif_no, cif_no) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static const HFRISKC_ROW *find_hfriskc(const HFRISKC_ROW *rows, size_t n,
                                       const char *cif_no, const char *instr_code)
{
    size_t i;

    for (i = 0U; i < n; i++) {
        if (strcmp(rows[i].cif_no, cif_no) == 0 &&
            strcmp(rows[i].instr_code, instr_code) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static void make_ts(char *dst, size_t dstsz)
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_WIN32)
    localtime_s(&tmv, &now);
#else
    localtime_r(&now, &tmv);
#endif
    (void)strftime(dst, dstsz, "%Y%m%d%H%M%S", &tmv);
}

static int write_declog(FILE *fp, uint64_t seq, const ORDERREC *o,
                        int action_code, const char *reason_code)
{
    DECLOG_ROW logrec;
    char ts[16];

    memset(&logrec, 0, sizeof logrec);
    make_ts(ts, sizeof ts);

    snprintf(logrec.decision_id, sizeof logrec.decision_id, "D%012" PRIu64, seq);
    strncpy(logrec.order_id, o->order_id, sizeof logrec.order_id - 1U);
    strncpy(logrec.instr_code, o->instr_code, sizeof logrec.instr_code - 1U);
    logrec.action_code = action_code;
    strncpy(logrec.reason_code, reason_code, sizeof logrec.reason_code - 1U);
    strncpy(logrec.decision_ts, ts, sizeof logrec.decision_ts - 1U);

    if (fprintf(fp, "%s,%s,%s,%d,%s,%s\n",
                logrec.decision_id,
                logrec.order_id,
                logrec.instr_code,
                logrec.action_code,
                logrec.reason_code,
                logrec.decision_ts) < 0) {
        fputs("HFDECLOG書込で入出力異常が発生しました\n", stderr);
        return -1;
    }

    return 0;
}

static int decide_order(const SCCUST_ROW *cust, const HFRISKC_ROW *risk,
                        const ORDERREC *o, const char **reason)
{
    int rate_bp;
    int64_t tick_x100;
    int64_t gross_x100;
    int64_t margin_base;
    int64_t margin_x100;
    int64_t used_plus_open;
    int64_t after_used;

    if (cust == NULL || risk == NULL) {
        *reason = "MASTER";
        return 8;
    }

    if (o->side_kbn != 'B' && o->side_kbn != 'S') {
        *reason = "SIDE";
        return 8;
    }

    if (o->ord_type != 'L' && o->ord_type != 'M') {
        *reason = "ORDTYPE";
        return 8;
    }

    if (tier_rate_bp(o->instr_tier, &rate_bp) != 0 ||
        tier_tick_x100(o->instr_tier, &tick_x100) != 0) {
        *reason = "TIER";
        return 8;
    }

    if (o->price_x100 <= 0 || o->qty <= 0 || o->fee_x100 < 0) {
        *reason = "VALUE";
        return 8;
    }

    if (o->ord_type == 'L' && (o->price_x100 % tick_x100) != 0) {
        *reason = "TICK";
        return 12;
    }

    if (mul_i64(o->qty, o->price_x100, &gross_x100) != 0 ||
        add_i64(gross_x100, o->fee_x100, &gross_x100) != 0) {
        *reason = "OVERFLOW";
        return 8;
    }

    if (gross_x100 > (int64_t)MIHFT_MAX_NOTIONAL * 100) {
        *reason = "NOTIONAL";
        return 8;
    }

    if (mul_i64(gross_x100, (int64_t)rate_bp, &margin_base) != 0) {
        *reason = "OVERFLOW";
        return 8;
    }
    margin_x100 = (margin_base + 9999) / 10000;

    if (add_i64(cust->group_used_amt, risk->open_notional_amt, &used_plus_open) != 0 ||
        add_i64(used_plus_open, margin_x100, &after_used) != 0) {
        *reason = "OVERFLOW";
        return 8;
    }

    if (after_used > cust->group_limit) {
        *reason = "MARGIN";
        return 4;
    }

    *reason = "OK";
    return 0;
}

int main(void)
{
    SCCUST_ROW cust_rows[4096];
    HFRISKC_ROW risk_rows[8192];
    size_t cust_count;
    size_t risk_count;
    FILE *ofp;
    FILE *dfp;
    ORDERREC order;
    uint64_t seq = 1U;
    int rc;
    int final_code = 0;

    rc = read_sccust(cust_rows, sizeof cust_rows / sizeof cust_rows[0], &cust_count);
    if (rc != 0) {
        return rc;
    }

    rc = read_hfriskc(risk_rows, sizeof risk_rows / sizeof risk_rows[0], &risk_count);
    if (rc != 0) {
        return rc;
    }

    ofp = fopen(MIHFT_PATH_ORDER, "r");
    if (ofp == NULL) {
        fputs("注文入力を開けません\n", stderr);
        return 40;
    }

    dfp = fopen(MIHFT_PATH_HFDECLOG, "w");
    if (dfp == NULL) {
        fclose(ofp);
        fputs("HFDECLOG出力を開けません\n", stderr);
        return 50;
    }

    while ((rc = read_order(ofp, &order)) > 0) {
        const SCCUST_ROW *cust = find_sccust(cust_rows, cust_count, order.cif_no);
        const HFRISKC_ROW *risk = find_hfriskc(risk_rows, risk_count,
                                               order.cif_no, order.instr_code);
        const char *reason;
        int action_code = decide_order(cust, risk, &order, &reason);

        if (write_declog(dfp, seq, &order, action_code, reason) != 0) {
            fclose(dfp);
            fclose(ofp);
            return 51;
        }

        if (action_code != 0 && final_code == 0) {
            final_code = action_code;
        }
        seq++;
    }

    if (rc < 0) {
        fclose(dfp);
        fclose(ofp);
        return 41;
    }

    if (fclose(dfp) != 0) {
        fclose(ofp);
        fputs("HFDECLOG終了処理で入出力異常が発生しました\n", stderr);
        return 52;
    }

    if (fclose(ofp) != 0) {
        fputs("注文入力終了処理で入出力異常が発生しました\n", stderr);
        return 42;
    }

    return final_code;
}
