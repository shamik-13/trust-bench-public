/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20250603  市場基盤部  注文ゲート検証の初版作成
 * 1.01  20251103  市場基盤部  SCRISK2照合および構造化アラート出力を追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_RC_ACCEPT          0
#define MIHFT_RC_REJECT_MARGIN   4
#define MIHFT_RC_REJECT_NOTIONAL 8
#define MIHFT_RC_REJECT_TICK     12
#define MIHFT_RC_IOERR           16
#define MIHFT_RC_PARSEERR        20

#define MIHFT_MAX_LINE           1024
#define MIHFT_MAX_FIELD          16
#define MIHFT_MAX_RECORDS        4096
#define MIHFT_ALERT_ID_LEN       32

typedef struct {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} scordf_rec_t;

typedef struct {
    char instr_code[32];
    char instr_name[96];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[4];
} scinstf_rec_t;

typedef struct {
    char cif_no[32];
    int instr_tier;
    int64_t max_notional_amt;
    int64_t max_qty;
    char kill_sw_kbn;
} scrisk2_rec_t;

typedef struct {
    scinstf_rec_t rows[MIHFT_MAX_RECORDS];
    size_t count;
} scinstf_table_t;

typedef struct {
    scrisk2_rec_t rows[MIHFT_MAX_RECORDS];
    size_t count;
} scrisk2_table_t;

static void trim_crlf(char *s)
{
    size_t n = strlen(s);

    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int csv_split(char *line, char *field[], size_t max_field, size_t *out_count)
{
    size_t count = 0U;
    char *p = line;

    while (*p != '\0') {
        if (count >= max_field) {
            return -1;
        }

        if (*p == '"') {
            char *w;

            ++p;
            field[count++] = p;
            w = p;

            while (*p != '\0') {
                if (*p == '"' && p[1] == '"') {
                    *w++ = '"';
                    p += 2;
                } else if (*p == '"') {
                    ++p;
                    break;
                } else {
                    *w++ = *p++;
                }
            }
            *w = '\0';

            if (*p == ',') {
                ++p;
            } else if (*p != '\0') {
                return -1;
            }
        } else {
            field[count++] = p;
            while (*p != '\0' && *p != ',') {
                ++p;
            }
            if (*p == ',') {
                *p++ = '\0';
            }
        }
    }

    if (line[0] != '\0' && line[strlen(line) - 1U] == ',') {
        if (count >= max_field) {
            return -1;
        }
        field[count++] = p;
    }

    *out_count = count;
    return 0;
}

static int copy_text(char *dst, size_t dst_len, const char *src)
{
    size_t n = strlen(src);

    if (n >= dst_len) {
        return -1;
    }
    memcpy(dst, src, n + 1U);
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
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

static int parse_int(const char *s, int *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int parse_scordf(char *line, scordf_rec_t *rec)
{
    char *f[MIHFT_MAX_FIELD];
    size_t n = 0U;

    if (csv_split(line, f, MIHFT_MAX_FIELD, &n) != 0 || n != 9U) {
        return -1;
    }

    if (copy_text(rec->order_id, sizeof rec->order_id, f[0]) != 0 ||
        copy_text(rec->cif_no, sizeof rec->cif_no, f[1]) != 0 ||
        copy_text(rec->instr_code, sizeof rec->instr_code, f[2]) != 0 ||
        strlen(f[3]) != 1U ||
        strlen(f[4]) != 1U ||
        copy_text(rec->tif_code, sizeof rec->tif_code, f[5]) != 0 ||
        parse_i64(f[6], &rec->ord_qty) != 0 ||
        parse_i64(f[7], &rec->price_amt) != 0 ||
        parse_int(f[8], &rec->instr_tier) != 0) {
        return -1;
    }

    rec->side_kbn = f[3][0];
    rec->ord_type = f[4][0];
    return 0;
}

static int parse_scinstf(char *line, scinstf_rec_t *rec)
{
    char *f[MIHFT_MAX_FIELD];
    size_t n = 0U;

    if (csv_split(line, f, MIHFT_MAX_FIELD, &n) != 6U) {
        return -1;
    }

    if (copy_text(rec->instr_code, sizeof rec->instr_code, f[0]) != 0 ||
        copy_text(rec->instr_name, sizeof rec->instr_name, f[1]) != 0 ||
        parse_int(f[2], &rec->instr_tier) != 0 ||
        parse_i64(f[3], &rec->tick_amt) != 0 ||
        parse_i64(f[4], &rec->lot_qty) != 0 ||
        copy_text(rec->board_code, sizeof rec->board_code, f[5]) != 0) {
        return -1;
    }

    return 0;
}

static int parse_scrisk2(char *line, scrisk2_rec_t *rec)
{
    char *f[MIHFT_MAX_FIELD];
    size_t n = 0U;

    if (csv_split(line, f, MIHFT_MAX_FIELD, &n) != 5U) {
        return -1;
    }

    if (copy_text(rec->cif_no, sizeof rec->cif_no, f[0]) != 0 ||
        parse_int(f[1], &rec->instr_tier) != 0 ||
        parse_i64(f[2], &rec->max_notional_amt) != 0 ||
        parse_i64(f[3], &rec->max_qty) != 0 ||
        strlen(f[4]) != 1U) {
        return -1;
    }

    rec->kill_sw_kbn = f[4][0];
    return 0;
}

static int load_scinstf(const char *path, scinstf_table_t *table)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];

    if (fp == NULL) {
        return -1;
    }

    table->count = 0U;
    while (fgets(line, sizeof line, fp) != NULL) {
        scinstf_rec_t rec;

        trim_crlf(line);
        if (line[0] == '\0') {
            continue;
        }
        if (table->count >= MIHFT_MAX_RECORDS || parse_scinstf(line, &rec) != 0) {
            fclose(fp);
            return -2;
        }
        table->rows[table->count++] = rec;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int load_scrisk2(const char *path, scrisk2_table_t *table)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];

    if (fp == NULL) {
        return -1;
    }

    table->count = 0U;
    while (fgets(line, sizeof line, fp) != NULL) {
        scrisk2_rec_t rec;

        trim_crlf(line);
        if (line[0] == '\0') {
            continue;
        }
        if (table->count >= MIHFT_MAX_RECORDS || parse_scrisk2(line, &rec) != 0) {
            fclose(fp);
            return -2;
        }
        table->rows[table->count++] = rec;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static const scinstf_rec_t *find_inst(const scinstf_table_t *table, const char *instr_code)
{
    size_t i;

    for (i = 0U; i < table->count; ++i) {
        if (strcmp(table->rows[i].instr_code, instr_code) == 0) {
            return &table->rows[i];
        }
    }
    return NULL;
}

static const scrisk2_rec_t *find_risk(const scrisk2_table_t *table, const char *cif_no, int instr_tier)
{
    size_t i;

    for (i = 0U; i < table->count; ++i) {
        if (table->rows[i].instr_tier == instr_tier &&
            strcmp(table->rows[i].cif_no, cif_no) == 0) {
            return &table->rows[i];
        }
    }
    return NULL;
}

static int tier_rate_bp(int instr_tier, int64_t *rate_bp)
{
    switch (instr_tier) {
    case 1:
        *rate_bp = 1000;
        return 0;
    case 2:
        *rate_bp = 2000;
        return 0;
    case 3:
        *rate_bp = 4000;
        return 0;
    default:
        return -1;
    }
}

static int checked_mul_i64(int64_t a, int64_t b, int64_t *out)
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

static void event_ts(char *buf, size_t len)
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_POSIX_VERSION)
    if (localtime_r(&now, &tmv) == NULL) {
        buf[0] = '\0';
        return;
    }
#else
    {
        struct tm *tmp = localtime(&now);
        if (tmp == NULL) {
            buf[0] = '\0';
            return;
        }
        tmv = *tmp;
    }
#endif

    (void)strftime(buf, len, "%Y%m%d%H%M%S", &tmv);
}

static int write_alert(FILE *fp,
                       unsigned long long seq,
                       const char *instr_code,
                       const char *alert_kbn,
                       const char *severity_cd,
                       int64_t observed_amt,
                       int64_t limit_amt)
{
    char ts[16];
    char alert_id[MIHFT_ALERT_ID_LEN];

    event_ts(ts, sizeof ts);
    if (snprintf(alert_id, sizeof alert_id, "ALT%014llu", seq) >= (int)sizeof alert_id) {
        return -1;
    }

    if (fprintf(fp, "%s,%s,%s,%s,%" PRId64 ",%" PRId64 ",%s\n",
                alert_id, instr_code, alert_kbn, severity_cd,
                observed_amt, limit_amt, ts) < 0) {
        return -1;
    }

    return 0;
}

static int is_valid_tif(const char *tif_code)
{
    return strcmp(tif_code, "DAY") == 0 ||
           strcmp(tif_code, "IOC") == 0 ||
           strcmp(tif_code, "FOK") == 0;
}

static int validate_order(const scordf_rec_t *ord,
                          const scinstf_table_t *inst_table,
                          const scrisk2_table_t *risk_table,
                          FILE *alert_fp,
                          unsigned long long *alert_seq)
{
    const scinstf_rec_t *inst = find_inst(inst_table, ord->instr_code);
    const scrisk2_rec_t *risk;
    int64_t notional = 0;
    int64_t rate_bp = 0;
    int64_t margin_amt = 0;
    int64_t client_notional_limit;

    if (ord->side_kbn != 'B' && ord->side_kbn != 'S') {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "SIDE", "E", 0, 0) == 0
            ? MIHFT_RC_REJECT_MARGIN : MIHFT_RC_IOERR;
    }

    if (ord->ord_type != 'L' && ord->ord_type != 'M') {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "OTYP", "E", 0, 0) == 0
            ? MIHFT_RC_REJECT_TICK : MIHFT_RC_IOERR;
    }

    if (!is_valid_tif(ord->tif_code)) {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "TIF", "E", 0, 0) == 0
            ? MIHFT_RC_REJECT_TICK : MIHFT_RC_IOERR;
    }

    if (inst == NULL || inst->instr_tier != ord->instr_tier ||
        inst->lot_qty <= 0 || inst->tick_amt <= 0) {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "INST", "E", 0, 0) == 0
            ? MIHFT_RC_REJECT_TICK : MIHFT_RC_IOERR;
    }

    if (ord->ord_qty <= 0 || ord->ord_qty % inst->lot_qty != 0) {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "LOT", "E",
                           ord->ord_qty, inst->lot_qty) == 0
            ? MIHFT_RC_REJECT_TICK : MIHFT_RC_IOERR;
    }

    if (ord->ord_type == 'L') {
        if (ord->price_amt <= 0 || ord->price_amt % inst->tick_amt != 0) {
            ++*alert_seq;
            return write_alert(alert_fp, *alert_seq, ord->instr_code, "TICK", "E",
                               ord->price_amt, inst->tick_amt) == 0
                ? MIHFT_RC_REJECT_TICK : MIHFT_RC_IOERR;
        }
    } else if (ord->price_amt != 0) {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "MKT", "E",
                           ord->price_amt, 0) == 0
            ? MIHFT_RC_REJECT_TICK : MIHFT_RC_IOERR;
    }

    risk = find_risk(risk_table, ord->cif_no, ord->instr_tier);
    if (risk == NULL) {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "RISK", "E", 0, 0) == 0
            ? MIHFT_RC_REJECT_MARGIN : MIHFT_RC_IOERR;
    }

    if (risk->kill_sw_kbn != '0') {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "KILL", "C", 1, 0) == 0
            ? MIHFT_RC_REJECT_MARGIN : MIHFT_RC_IOERR;
    }

    if (risk->max_qty > 0 && ord->ord_qty > risk->max_qty) {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "QTY", "E",
                           ord->ord_qty, risk->max_qty) == 0
            ? MIHFT_RC_REJECT_NOTIONAL : MIHFT_RC_IOERR;
    }

    if (ord->ord_type == 'L') {
        if (checked_mul_i64(ord->ord_qty, ord->price_amt, &notional) != 0) {
            ++*alert_seq;
            return write_alert(alert_fp, *alert_seq, ord->instr_code, "OVFL", "C",
                               ord->ord_qty, ord->price_amt) == 0
                ? MIHFT_RC_REJECT_NOTIONAL : MIHFT_RC_IOERR;
        }
    } else {
        int64_t proxy_price;

        if (inst->tick_amt > INT64_MAX / 1000) {
            ++*alert_seq;
            return write_alert(alert_fp, *alert_seq, ord->instr_code, "OVFL", "C",
                               inst->tick_amt, 1000) == 0
                ? MIHFT_RC_REJECT_NOTIONAL : MIHFT_RC_IOERR;
        }
        proxy_price = inst->tick_amt * 1000;
        if (checked_mul_i64(ord->ord_qty, proxy_price, &notional) != 0) {
            ++*alert_seq;
            return write_alert(alert_fp, *alert_seq, ord->instr_code, "OVFL", "C",
                               ord->ord_qty, proxy_price) == 0
                ? MIHFT_RC_REJECT_NOTIONAL : MIHFT_RC_IOERR;
        }
    }

    client_notional_limit = risk->max_notional_amt;
    if (client_notional_limit <= 0 || client_notional_limit > MIHFT_MAX_NOTIONAL) {
        client_notional_limit = MIHFT_MAX_NOTIONAL;
    }

    if (notional > client_notional_limit) {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "NOTL", "E",
                           notional, client_notional_limit) == 0
            ? MIHFT_RC_REJECT_NOTIONAL : MIHFT_RC_IOERR;
    }

    if (tier_rate_bp(ord->instr_tier, &rate_bp) != 0 ||
        checked_mul_i64(notional, rate_bp, &margin_amt) != 0) {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "MRGN", "E",
                           notional, 0) == 0
            ? MIHFT_RC_REJECT_MARGIN : MIHFT_RC_IOERR;
    }
    margin_amt /= 10000;

    if (risk->max_notional_amt > 0 && margin_amt > risk->max_notional_amt) {
        ++*alert_seq;
        return write_alert(alert_fp, *alert_seq, ord->instr_code, "MRGN", "E",
                           margin_amt, risk->max_notional_amt) == 0
            ? MIHFT_RC_REJECT_MARGIN : MIHFT_RC_IOERR;
    }

    return MIHFT_RC_ACCEPT;
}

int main(void)
{
    scinstf_table_t inst_table;
    scrisk2_table_t risk_table;
    FILE *ord_fp;
    FILE *alert_fp;
    char line[MIHFT_MAX_LINE];
    int final_rc = MIHFT_RC_ACCEPT;
    unsigned long long alert_seq = 0ULL;

    if (load_scinstf("SCINSTF.csv", &inst_table) != 0) {
        fputs("銘柄マスタ読込失敗\n", stderr);
        return MIHFT_RC_IOERR;
    }

    if (load_scrisk2("SCRISK2.csv", &risk_table) != 0) {
        fputs("リスク状態読込失敗\n", stderr);
        return MIHFT_RC_IOERR;
    }

    ord_fp = fopen("SCORDF.csv", "r");
    if (ord_fp == NULL) {
        fputs("注文入力オープン失敗\n", stderr);
        return MIHFT_RC_IOERR;
    }

    alert_fp = fopen("SCHALT.csv", "a");
    if (alert_fp == NULL) {
        fclose(ord_fp);
        fputs("アラート出力オープン失敗\n", stderr);
        return MIHFT_RC_IOERR;
    }

    while (fgets(line, sizeof line, ord_fp) != NULL) {
        scordf_rec_t ord;
        int rc;

        trim_crlf(line);
        if (line[0] == '\0') {
            continue;
        }

        if (parse_scordf(line, &ord) != 0) {
            fclose(alert_fp);
            fclose(ord_fp);
            fputs("注文入力解析失敗\n", stderr);
            return MIHFT_RC_PARSEERR;
        }

        rc = validate_order(&ord, &inst_table, &risk_table, alert_fp, &alert_seq);
        if (rc == MIHFT_RC_IOERR) {
            fclose(alert_fp);
            fclose(ord_fp);
            fputs("アラート出力失敗\n", stderr);
            return MIHFT_RC_IOERR;
        }
        if (rc != MIHFT_RC_ACCEPT && final_rc == MIHFT_RC_ACCEPT) {
            final_rc = rc;
        }
    }

    if (ferror(ord_fp)) {
        fclose(alert_fp);
        fclose(ord_fp);
        fputs("注文入力読込失敗\n", stderr);
        return MIHFT_RC_IOERR;
    }

    if (fclose(alert_fp) != 0) {
        fclose(ord_fp);
        fputs("アラート出力クローズ失敗\n", stderr);
        return MIHFT_RC_IOERR;
    }

    if (fclose(ord_fp) != 0) {
        fputs("注文入力クローズ失敗\n", stderr);
        return MIHFT_RC_IOERR;
    }

    return final_rc;
}
