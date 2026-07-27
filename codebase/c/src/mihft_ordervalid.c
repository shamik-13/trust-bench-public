/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240709  藤田 和也 (E-271)     注文妥当性チェック初版
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_ERR_IO 64
#define MIHFT_ERR_PARSE 65
#define MIHFT_REJECT_ID_MAX 999999999L

#define SCORDF_PATH "SCORDF.csv"
#define SCINSTF_PATH "SCINSTF.csv"
#define SCCALF_PATH "SCCALF.csv"
#define SCREJ_PATH "SCREJ"

#define MAX_LINE_LEN 1024
#define MAX_ORDERS 200000
#define MAX_INSTR 20000
#define MAX_SESS 64

typedef struct {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn[4];
    char ord_type[4];
    char tif_code[8];
    long long ord_qty;
    long long price_amt;
    int instr_tier;
} scordf_rec;

typedef struct {
    char instr_code[32];
    char instr_name[128];
    int instr_tier;
    long long tick_amt;
    long long lot_qty;
    char board_code[8];
} scinstf_rec;

typedef struct {
    char sess_dt[9];
    char sess_kbn[8];
    char open_ts[15];
    char close_ts[15];
} sccalf_rec;

static int trim_line(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
    return n > 0;
}

static int split_csv(char *line, char **field, size_t need)
{
    size_t i = 0;
    char *p = line;

    while (i < need) {
        field[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return i == need && strchr(field[need - 1], ',') == NULL;
}

static int parse_ll(const char *s, long long *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
        return 0;
    }
    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return 0;
    }
    *out = v;
    return 1;
}

static int parse_int(const char *s, int *out)
{
    long long v;

    if (!parse_ll(s, &v) || v < INT_MIN || v > INT_MAX) {
        return 0;
    }
    *out = (int)v;
    return 1;
}

static int copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (n >= dstsz) {
        return 0;
    }
    memcpy(dst, src, n + 1);
    return 1;
}

static long long tier_tick_amt(int tier)
{
    if (tier == 1) {
        return 100;
    }
    if (tier == 2) {
        return 500;
    }
    if (tier == 3) {
        return 1000;
    }
    return 0;
}

static int valid_board_code(const char *s)
{
    return strcmp(s, "T1") == 0 || strcmp(s, "ST") == 0 || strcmp(s, "ETF") == 0;
}

static int valid_side_kbn(const char *s)
{
    return strcmp(s, "B") == 0 || strcmp(s, "S") == 0;
}

static int valid_ord_type(const char *s)
{
    return strcmp(s, "L") == 0 || strcmp(s, "M") == 0;
}

static int valid_tif_code(const char *s)
{
    return strcmp(s, "DAY") == 0 || strcmp(s, "IOC") == 0 || strcmp(s, "FOK") == 0;
}

static int checked_notional(long long qty, long long price, long long *out)
{
    if (qty <= 0 || price < 0) {
        return 0;
    }
    if (price != 0 && qty > LLONG_MAX / price) {
        return 0;
    }
    *out = qty * price;
    return 1;
}

static int price_tick_ok(long long price_amt, const scinstf_rec *inst)
{
    long long tick = tier_tick_amt(inst->instr_tier);

    if (price_amt <= 0 || tick <= 0) {
        return 0;
    }
    if (inst->tick_amt != tick) {
        return 0;
    }
    return price_amt % tick == 0;
}

static void make_today(char out[9])
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_WIN32)
    localtime_s(&tmv, &now);
#else
    localtime_r(&now, &tmv);
#endif
    snprintf(out, 9, "%04d%02d%02d", tmv.tm_year + 1900, tmv.tm_mon + 1, tmv.tm_mday);
}

static void make_reject_ts(char out[15])
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_WIN32)
    localtime_s(&tmv, &now);
#else
    localtime_r(&now, &tmv);
#endif
    snprintf(out, 15, "%04d%02d%02d%02d%02d%02d",
             tmv.tm_year + 1900, tmv.tm_mon + 1, tmv.tm_mday,
             tmv.tm_hour, tmv.tm_min, tmv.tm_sec);
}

static int ts_now_between(const sccalf_rec *sess)
{
    char now_ts[15];

    make_reject_ts(now_ts);
    return strcmp(now_ts, sess->open_ts) >= 0 && strcmp(now_ts, sess->close_ts) <= 0;
}

static int session_eligible(const sccalf_rec *sess, size_t sess_count)
{
    size_t i;
    char today[9];

    make_today(today);
    for (i = 0; i < sess_count; i++) {
        if (strcmp(sess[i].sess_dt, today) == 0 && ts_now_between(&sess[i])) {
            return 1;
        }
    }
    return 0;
}

static const scinstf_rec *find_instr(const scinstf_rec *inst, size_t inst_count, const char *code)
{
    size_t i;

    for (i = 0; i < inst_count; i++) {
        if (strcmp(inst[i].instr_code, code) == 0) {
            return &inst[i];
        }
    }
    return NULL;
}

static int load_scordf(scordf_rec *rows, size_t *count)
{
    FILE *fp = fopen(SCORDF_PATH, "r");
    char line[MAX_LINE_LEN];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCORDFオープン失敗\n");
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[9];

        if (!trim_line(line)) {
            continue;
        }
        if (n >= MAX_ORDERS || !split_csv(line, f, 9)) {
            fclose(fp);
            fprintf(stderr, "SCORDF解析失敗\n");
            return MIHFT_ERR_PARSE;
        }
        if (!copy_field(rows[n].order_id, sizeof rows[n].order_id, f[0]) ||
            !copy_field(rows[n].cif_no, sizeof rows[n].cif_no, f[1]) ||
            !copy_field(rows[n].instr_code, sizeof rows[n].instr_code, f[2]) ||
            !copy_field(rows[n].side_kbn, sizeof rows[n].side_kbn, f[3]) ||
            !copy_field(rows[n].ord_type, sizeof rows[n].ord_type, f[4]) ||
            !copy_field(rows[n].tif_code, sizeof rows[n].tif_code, f[5]) ||
            !parse_ll(f[6], &rows[n].ord_qty) ||
            !parse_ll(f[7], &rows[n].price_amt) ||
            !parse_int(f[8], &rows[n].instr_tier)) {
            fclose(fp);
            fprintf(stderr, "SCORDF項目不正\n");
            return MIHFT_ERR_PARSE;
        }
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCORDF読込失敗\n");
        return MIHFT_ERR_IO;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static int load_scinstf(scinstf_rec *rows, size_t *count)
{
    FILE *fp = fopen(SCINSTF_PATH, "r");
    char line[MAX_LINE_LEN];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCINSTFオープン失敗\n");
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[6];

        if (!trim_line(line)) {
            continue;
        }
        if (n >= MAX_INSTR || !split_csv(line, f, 6)) {
            fclose(fp);
            fprintf(stderr, "SCINSTF解析失敗\n");
            return MIHFT_ERR_PARSE;
        }
        if (!copy_field(rows[n].instr_code, sizeof rows[n].instr_code, f[0]) ||
            !copy_field(rows[n].instr_name, sizeof rows[n].instr_name, f[1]) ||
            !parse_int(f[2], &rows[n].instr_tier) ||
            !parse_ll(f[3], &rows[n].tick_amt) ||
            !parse_ll(f[4], &rows[n].lot_qty) ||
            !copy_field(rows[n].board_code, sizeof rows[n].board_code, f[5]) ||
            tier_tick_amt(rows[n].instr_tier) == 0 ||
            rows[n].lot_qty <= 0 ||
            !valid_board_code(rows[n].board_code)) {
            fclose(fp);
            fprintf(stderr, "SCINSTF項目不正\n");
            return MIHFT_ERR_PARSE;
        }
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCINSTF読込失敗\n");
        return MIHFT_ERR_IO;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static int load_sccalf(sccalf_rec *rows, size_t *count)
{
    FILE *fp = fopen(SCCALF_PATH, "r");
    char line[MAX_LINE_LEN];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCCALFオープン失敗\n");
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[4];

        if (!trim_line(line)) {
            continue;
        }
        if (n >= MAX_SESS || !split_csv(line, f, 4)) {
            fclose(fp);
            fprintf(stderr, "SCCALF解析失敗\n");
            return MIHFT_ERR_PARSE;
        }
        if (!copy_field(rows[n].sess_dt, sizeof rows[n].sess_dt, f[0]) ||
            !copy_field(rows[n].sess_kbn, sizeof rows[n].sess_kbn, f[1]) ||
            !copy_field(rows[n].open_ts, sizeof rows[n].open_ts, f[2]) ||
            !copy_field(rows[n].close_ts, sizeof rows[n].close_ts, f[3]) ||
            strlen(rows[n].sess_dt) != 8 ||
            strlen(rows[n].open_ts) != 14 ||
            strlen(rows[n].close_ts) != 14 ||
            strcmp(rows[n].open_ts, rows[n].close_ts) >= 0) {
            fclose(fp);
            fprintf(stderr, "SCCALF項目不正\n");
            return MIHFT_ERR_PARSE;
        }
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCCALF読込失敗\n");
        return MIHFT_ERR_IO;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static int reject_code_for_order(const scordf_rec *ord, const scinstf_rec *inst, int sess_ok)
{
    long long notional;

    if (inst == NULL || !sess_ok || !valid_side_kbn(ord->side_kbn) ||
        !valid_ord_type(ord->ord_type) || !valid_tif_code(ord->tif_code)) {
        return 8;
    }
    if (ord->instr_tier != inst->instr_tier || ord->ord_qty <= 0 ||
        inst->lot_qty <= 0 || ord->ord_qty % inst->lot_qty != 0) {
        return 8;
    }
    if (strcmp(ord->ord_type, "L") == 0 && !price_tick_ok(ord->price_amt, inst)) {
        return 12;
    }
    if (strcmp(ord->ord_type, "M") == 0 && ord->price_amt != 0) {
        return 12;
    }
    if (!checked_notional(ord->ord_qty, ord->price_amt, &notional) ||
        notional > MIHFT_MAX_NOTIONAL) {
        return 8;
    }
    return 0;
}

static int write_reject(FILE *fp, long reject_id, const scordf_rec *ord, int reject_cd)
{
    char ts[15];

    make_reject_ts(ts);
    if (fprintf(fp, "%09ld,%s,%s,%s,%d,%s\n",
                reject_id, ord->order_id, ord->cif_no,
                ord->instr_code, reject_cd, ts) < 0) {
        fprintf(stderr, "SCREJ書込失敗\n");
        return MIHFT_ERR_IO;
    }
    return 0;
}

int main(void)
{
    static scordf_rec orders[MAX_ORDERS];
    static scinstf_rec insts[MAX_INSTR];
    static sccalf_rec sessions[MAX_SESS];

    size_t order_count = 0;
    size_t inst_count = 0;
    size_t sess_count = 0;
    size_t i;
    long reject_id = 1;
    int rc;
    int normal_rc = 0;
    int sess_ok;
    FILE *rej_fp;

    rc = load_scinstf(insts, &inst_count);
    if (rc != 0) {
        return rc;
    }
    rc = load_sccalf(sessions, &sess_count);
    if (rc != 0) {
        return rc;
    }
    rc = load_scordf(orders, &order_count);
    if (rc != 0) {
        return rc;
    }

    rej_fp = fopen(SCREJ_PATH, "w");
    if (rej_fp == NULL) {
        fprintf(stderr, "SCREJオープン失敗\n");
        return MIHFT_ERR_IO;
    }

    sess_ok = session_eligible(sessions, sess_count);

    for (i = 0; i < order_count; i++) {
        const scinstf_rec *inst = find_instr(insts, inst_count, orders[i].instr_code);
        int reject_cd = reject_code_for_order(&orders[i], inst, sess_ok);

        if (reject_cd != 0) {
            if (reject_id > MIHFT_REJECT_ID_MAX) {
                fclose(rej_fp);
                fprintf(stderr, "SCREJ採番上限超過\n");
                return MIHFT_ERR_PARSE;
            }
            rc = write_reject(rej_fp, reject_id++, &orders[i], reject_cd);
            if (rc != 0) {
                fclose(rej_fp);
                return rc;
            }
            if (reject_cd > normal_rc) {
                normal_rc = reject_cd;
            }
        }
    }

    if (fclose(rej_fp) != 0) {
        fprintf(stderr, "SCREJクローズ失敗\n");
        return MIHFT_ERR_IO;
    }

    return normal_rc;
}
