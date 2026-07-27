/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210128  渡辺 隆 (E-260)  価格帯ホットパス検査の初版作成
 */
#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_LINE_MAX 1024
#define MIHFT_KEY_MAX 64
#define MIHFT_NAME_MAX 128
#define MIHFT_ALERT_MAX 64
#define MIHFT_SRC_MAX 16
#define MIHFT_TS_MAX 32

#define MIHFT_DECISION_ACCEPT 0
#define MIHFT_DECISION_REJECT_NOTIONAL 8
#define MIHFT_DECISION_REJECT_TICK 12
#define MIHFT_IOERR 2
#define MIHFT_PARSEERR 3

typedef struct {
    char order_id[MIHFT_KEY_MAX];
    char cif_no[MIHFT_KEY_MAX];
    char instr_code[MIHFT_KEY_MAX];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} scordf_rec_t;

typedef struct {
    char instr_code[MIHFT_KEY_MAX];
    int64_t lower_amt;
    int64_t upper_amt;
    char band_ts[MIHFT_TS_MAX];
    char source_kbn[MIHFT_SRC_MAX];
} scband_rec_t;

typedef struct {
    char instr_code[MIHFT_KEY_MAX];
    char instr_name[MIHFT_NAME_MAX];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[8];
} scinstf_rec_t;

static void chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int next_field(char **cursor, char *out, size_t out_sz)
{
    char *p = *cursor;
    size_t n = 0;

    if (out_sz == 0) {
        return -1;
    }

    while (*p != '\0' && *p != ',') {
        if (n + 1 >= out_sz) {
            return -1;
        }
        out[n++] = *p++;
    }
    out[n] = '\0';

    if (*p == ',') {
        p++;
    }
    *cursor = p;
    return 0;
}

static int parse_i64(const char *s, int64_t *v)
{
    char *end = NULL;
    long long tmp;

    errno = 0;
    tmp = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }
    *v = (int64_t)tmp;
    return 0;
}

static int parse_int(const char *s, int *v)
{
    int64_t tmp;

    if (parse_i64(s, &tmp) != 0 || tmp < INT_MIN || tmp > INT_MAX) {
        return -1;
    }
    *v = (int)tmp;
    return 0;
}

static int read_scordf(const char *path, scordf_rec_t *rec)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    char *cur;
    char qty[32], price[32], tier[16];

    fp = fopen(path, "r");
    if (fp == NULL) {
        return -1;
    }
    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        return -1;
    }
    fclose(fp);

    chomp(line);
    cur = line;
    if (next_field(&cur, rec->order_id, sizeof(rec->order_id)) != 0 ||
        next_field(&cur, rec->cif_no, sizeof(rec->cif_no)) != 0 ||
        next_field(&cur, rec->instr_code, sizeof(rec->instr_code)) != 0 ||
        next_field(&cur, qty, sizeof(qty)) != 0) {
        return -1;
    }
    rec->side_kbn = qty[0];

    if (next_field(&cur, qty, sizeof(qty)) != 0) {
        return -1;
    }
    rec->ord_type = qty[0];

    if (next_field(&cur, rec->tif_code, sizeof(rec->tif_code)) != 0 ||
        next_field(&cur, qty, sizeof(qty)) != 0 ||
        next_field(&cur, price, sizeof(price)) != 0 ||
        next_field(&cur, tier, sizeof(tier)) != 0) {
        return -1;
    }

    if ((rec->side_kbn != 'B' && rec->side_kbn != 'S') ||
        (rec->ord_type != 'L' && rec->ord_type != 'M') ||
        (strcmp(rec->tif_code, "DAY") != 0 && strcmp(rec->tif_code, "IOC") != 0 && strcmp(rec->tif_code, "FOK") != 0)) {
        return -1;
    }

    if (parse_i64(qty, &rec->ord_qty) != 0 ||
        parse_i64(price, &rec->price_amt) != 0 ||
        parse_int(tier, &rec->instr_tier) != 0 ||
        rec->ord_qty <= 0 || rec->price_amt < 0) {
        return -1;
    }

    return 0;
}

static int read_scband(const char *path, const char *instr_code, scband_rec_t *rec)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    char lower[32], upper[32];
    int found = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cur;

        chomp(line);
        cur = line;
        if (next_field(&cur, rec->instr_code, sizeof(rec->instr_code)) != 0 ||
            next_field(&cur, lower, sizeof(lower)) != 0 ||
            next_field(&cur, upper, sizeof(upper)) != 0 ||
            next_field(&cur, rec->band_ts, sizeof(rec->band_ts)) != 0 ||
            next_field(&cur, rec->source_kbn, sizeof(rec->source_kbn)) != 0) {
            fclose(fp);
            return -1;
        }
        if (strcmp(rec->instr_code, instr_code) == 0) {
            found = 1;
            break;
        }
    }
    fclose(fp);

    if (!found ||
        parse_i64(lower, &rec->lower_amt) != 0 ||
        parse_i64(upper, &rec->upper_amt) != 0 ||
        rec->lower_amt < 0 || rec->upper_amt < rec->lower_amt) {
        return -1;
    }

    return 0;
}

static int read_scinstf(const char *path, const char *instr_code, scinstf_rec_t *rec)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    char tier[16], tick[32], lot[32];
    int found = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cur;

        chomp(line);
        cur = line;
        if (next_field(&cur, rec->instr_code, sizeof(rec->instr_code)) != 0 ||
            next_field(&cur, rec->instr_name, sizeof(rec->instr_name)) != 0 ||
            next_field(&cur, tier, sizeof(tier)) != 0 ||
            next_field(&cur, tick, sizeof(tick)) != 0 ||
            next_field(&cur, lot, sizeof(lot)) != 0 ||
            next_field(&cur, rec->board_code, sizeof(rec->board_code)) != 0) {
            fclose(fp);
            return -1;
        }
        if (strcmp(rec->instr_code, instr_code) == 0) {
            found = 1;
            break;
        }
    }
    fclose(fp);

    if (!found ||
        parse_int(tier, &rec->instr_tier) != 0 ||
        parse_i64(tick, &rec->tick_amt) != 0 ||
        parse_i64(lot, &rec->lot_qty) != 0 ||
        rec->tick_amt <= 0 || rec->lot_qty <= 0) {
        return -1;
    }

    return 0;
}

static int checked_notional(int64_t qty, int64_t price, int64_t *notional)
{
    if (qty < 0 || price < 0) {
        return -1;
    }
    if (price != 0 && qty > INT64_MAX / price) {
        return -1;
    }
    *notional = qty * price;
    return 0;
}

static void now_ts(char *buf, size_t sz)
{
    time_t t = time(NULL);
    struct tm tmv;

#if defined(_WIN32)
    localtime_s(&tmv, &t);
#else
    localtime_r(&t, &tmv);
#endif
    (void)strftime(buf, sz, "%Y%m%d%H%M%S", &tmv);
}

static int write_schalt(const char *path, const char *instr_code, const char *alert_kbn,
                        int64_t observed_amt, int64_t limit_amt)
{
    FILE *fp;
    char ts[MIHFT_TS_MAX];
    char alert_id[MIHFT_ALERT_MAX];

    now_ts(ts, sizeof(ts));
    if (snprintf(alert_id, sizeof(alert_id), "AL%s%s", ts, instr_code) < 0) {
        return -1;
    }

    fp = fopen(path, "a");
    if (fp == NULL) {
        return -1;
    }

    if (fprintf(fp, "%s,%s,%s,H,%" PRId64 ",%" PRId64 ",%s\n",
                alert_id, instr_code, alert_kbn, observed_amt, limit_amt, ts) < 0) {
        fclose(fp);
        return -1;
    }

    if (fclose(fp) != 0) {
        return -1;
    }

    return 0;
}

int main(void)
{
    scordf_rec_t ord;
    scband_rec_t band;
    scinstf_rec_t inst;
    int64_t notional;
    int hard_breach = 0;
    int64_t breach_limit = 0;
    const char *breach_kbn = NULL;

    if (read_scordf("SCORDF.csv", &ord) != 0) {
        fprintf(stderr, "SCORDF読込失敗\n");
        return MIHFT_PARSEERR;
    }

    if (read_scband("SCBAND.csv", ord.instr_code, &band) != 0) {
        fprintf(stderr, "SCBAND読込失敗\n");
        return MIHFT_PARSEERR;
    }

    if (read_scinstf("SCINSTF.csv", ord.instr_code, &inst) != 0) {
        fprintf(stderr, "SCINSTF読込失敗\n");
        return MIHFT_PARSEERR;
    }

    if (ord.instr_tier != inst.instr_tier || ord.ord_qty % inst.lot_qty != 0) {
        fprintf(stderr, "銘柄属性不整合\n");
        return MIHFT_PARSEERR;
    }

    if (checked_notional(ord.ord_qty, ord.price_amt, &notional) != 0 ||
        notional > MIHFT_MAX_NOTIONAL) {
        return MIHFT_DECISION_REJECT_NOTIONAL;
    }

    if (ord.ord_type == 'L' && mihft_tick(ord.price_amt, inst.tick_amt) != 0) {
        return MIHFT_DECISION_REJECT_TICK;
    }

    if (ord.ord_type == 'L' && ord.price_amt < band.lower_amt) {
        hard_breach = 1;
        breach_limit = band.lower_amt;
        breach_kbn = "LOWER";
    } else if (ord.ord_type == 'L' && ord.price_amt > band.upper_amt) {
        hard_breach = 1;
        breach_limit = band.upper_amt;
        breach_kbn = "UPPER";
    }

    if (hard_breach) {
        if (write_schalt("SCHALT.csv", ord.instr_code, breach_kbn, ord.price_amt, breach_limit) != 0) {
            fprintf(stderr, "SCHALT出力失敗\n");
            return MIHFT_IOERR;
        }
        return MIHFT_DECISION_REJECT_NOTIONAL;
    }

    return MIHFT_DECISION_ACCEPT;
}
