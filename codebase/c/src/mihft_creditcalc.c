/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20190416  村上 健司 (E-301)  初版作成
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_DECISION_ACCEPT          0
#define MIHFT_DECISION_REJECT_MARGIN   4
#define MIHFT_DECISION_REJECT_NOTIONAL 8
#define MIHFT_DECISION_REJECT_TICK     12
#define MIHFT_DECISION_PARSE_ERROR     16

#define MIHFT_CSV_LINE_MAX 512
#define MIHFT_CODE_MAX     32

struct pos_rec {
    char cif_no[MIHFT_CODE_MAX];
    char instr_code[MIHFT_CODE_MAX];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
};

struct cust_rec {
    char cif_no[MIHFT_CODE_MAX];
    int64_t group_limit;
    int64_t group_used_amt;
    int64_t acct_used_amt;
};

struct order_rec {
    char cif_no[MIHFT_CODE_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    char board_code[4];
    int tier;
    int64_t qty;
    int64_t price;
};

static void trim_field(char *s)
{
    char *p;
    size_t n;

    while (isspace((unsigned char)*s)) {
        memmove(s, s + 1, strlen(s));
    }

    n = strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1])) {
        s[--n] = '\0';
    }

    p = strchr(s, '\r');
    if (p != NULL) {
        *p = '\0';
    }
}

static int split_csv(char *line, char **field, size_t max_field)
{
    size_t count = 0;
    char *p = line;

    while (count < max_field) {
        field[count++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    for (size_t i = 0; i < count; i++) {
        trim_field(field[i]);
    }

    return (int)count;
}

static int parse_i64(const char *s, int64_t *out)
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

    *out = (int64_t)v;
    return 0;
}

static int parse_order_env(struct order_rec *ord)
{
    const char *v;
    int64_t tier64;

    memset(ord, 0, sizeof(*ord));

    v = getenv("MIHFT_CIF_NO");
    if (v == NULL || strlen(v) >= sizeof(ord->cif_no)) {
        return -1;
    }
    strcpy(ord->cif_no, v);

    v = getenv("MIHFT_INSTR_CODE");
    if (v == NULL || strlen(v) >= sizeof(ord->instr_code)) {
        return -1;
    }
    strcpy(ord->instr_code, v);

    v = getenv("MIHFT_SIDE_KBN");
    if (v == NULL || (v[0] != 'B' && v[0] != 'S') || v[1] != '\0') {
        return -1;
    }
    ord->side_kbn = v[0];

    v = getenv("MIHFT_ORD_TYPE");
    if (v == NULL || (v[0] != 'L' && v[0] != 'M') || v[1] != '\0') {
        return -1;
    }
    ord->ord_type = v[0];

    v = getenv("MIHFT_TIF_CODE");
    if (v == NULL || strlen(v) >= sizeof(ord->tif_code)) {
        return -1;
    }
    strcpy(ord->tif_code, v);

    v = getenv("MIHFT_BOARD_CODE");
    if (v == NULL || strlen(v) >= sizeof(ord->board_code)) {
        return -1;
    }
    strcpy(ord->board_code, v);

    if (parse_i64(getenv("MIHFT_TIER"), &tier64) != 0 || tier64 < 1 || tier64 > 3) {
        return -1;
    }
    ord->tier = (int)tier64;

    if (parse_i64(getenv("MIHFT_QTY"), &ord->qty) != 0 || ord->qty <= 0) {
        return -1;
    }

    if (parse_i64(getenv("MIHFT_PRICE"), &ord->price) != 0 || ord->price <= 0) {
        return -1;
    }

    if (strcmp(ord->tif_code, "DAY") != 0 &&
        strcmp(ord->tif_code, "IOC") != 0 &&
        strcmp(ord->tif_code, "FOK") != 0) {
        return -1;
    }

    if (strcmp(ord->board_code, "T1") != 0 &&
        strcmp(ord->board_code, "ST") != 0 &&
        strcmp(ord->board_code, "ETF") != 0) {
        return -1;
    }

    return 0;
}

static int mul_i64_sat(int64_t a, int64_t b, int64_t *out)
{
    if (a != 0 && b > INT64_MAX / a) {
        *out = INT64_MAX;
        return -1;
    }

    *out = a * b;
    return 0;
}

static int add_i64_sat(int64_t a, int64_t b, int64_t *out)
{
    if (b > 0 && a > INT64_MAX - b) {
        *out = INT64_MAX;
        return -1;
    }
    if (b < 0 && a < INT64_MIN - b) {
        *out = INT64_MIN;
        return -1;
    }

    *out = a + b;
    return 0;
}

static int rate_bp_for_tier(int tier)
{
    if (tier == 1) {
        return 1000;
    }
    if (tier == 2) {
        return 2000;
    }
    return 4000;
}

static int tick_for_tier(int tier)
{
    if (tier == 1) {
        return 100;
    }
    if (tier == 2) {
        return 500;
    }
    return 1000;
}

static int read_position(const char *path, const struct order_rec *ord, struct pos_rec *pos)
{
    FILE *fp;
    char line[MIHFT_CSV_LINE_MAX];
    char *f[5];

    memset(pos, 0, sizeof(*pos));

    fp = fopen(path, "r");
    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        if (split_csv(line, f, 5) != 5) {
            fclose(fp);
            return -1;
        }

        if (strcmp(f[0], "CIF-NO") == 0) {
            continue;
        }

        if (strcmp(f[0], ord->cif_no) == 0 && strcmp(f[1], ord->instr_code) == 0) {
            if (strlen(f[0]) >= sizeof(pos->cif_no) || strlen(f[1]) >= sizeof(pos->instr_code)) {
                fclose(fp);
                return -1;
            }
            strcpy(pos->cif_no, f[0]);
            strcpy(pos->instr_code, f[1]);
            if (parse_i64(f[2], &pos->net_qty) != 0 ||
                parse_i64(f[3], &pos->avg_amt) != 0 ||
                parse_i64(f[4], &pos->rlzd_amt) != 0) {
                fclose(fp);
                return -1;
            }
            fclose(fp);
            return 0;
        }
    }

    fclose(fp);
    strcpy(pos->cif_no, ord->cif_no);
    strcpy(pos->instr_code, ord->instr_code);
    return 0;
}

static int read_customer(const char *path, const char *cif_no, struct cust_rec *cust)
{
    FILE *fp;
    char line[MIHFT_CSV_LINE_MAX];
    char *f[4];

    fp = fopen(path, "r");
    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        if (split_csv(line, f, 4) != 4) {
            fclose(fp);
            return -1;
        }

        if (strcmp(f[0], "CIF-NO") == 0) {
            continue;
        }

        if (strcmp(f[0], cif_no) == 0) {
            if (strlen(f[0]) >= sizeof(cust->cif_no)) {
                fclose(fp);
                return -1;
            }
            strcpy(cust->cif_no, f[0]);
            if (parse_i64(f[1], &cust->group_limit) != 0 ||
                parse_i64(f[2], &cust->group_used_amt) != 0 ||
                parse_i64(f[3], &cust->acct_used_amt) != 0) {
                fclose(fp);
                return -1;
            }
            fclose(fp);
            return 0;
        }
    }

    fclose(fp);
    return -1;
}

static int calc_incremental_margin(const struct order_rec *ord, const struct pos_rec *pos, int64_t *margin)
{
    int overflow = 0;
    int64_t consuming_qty;
    int64_t notional;
    int64_t raw_margin;
    int rate_bp = rate_bp_for_tier(ord->tier);

    if (ord->side_kbn == 'S' && pos->net_qty > 0) {
        consuming_qty = ord->qty > pos->net_qty ? ord->qty - pos->net_qty : 0;
    } else {
        consuming_qty = ord->qty;
    }

    overflow |= mul_i64_sat(consuming_qty, ord->price, &notional);
    overflow |= mul_i64_sat(notional, (int64_t)rate_bp, &raw_margin);

    if (raw_margin == INT64_MAX) {
        *margin = INT64_MAX;
        return -1;
    }

    *margin = (raw_margin + 9999) / 10000;
    return overflow;
}

int main(void)
{
    struct order_rec ord;
    struct pos_rec pos;
    struct cust_rec cust;
    int64_t order_notional;
    int64_t incr_margin;
    int64_t total_used;
    int tick;
    int overflow = 0;

    if (parse_order_env(&ord) != 0) {
        fputs("入力環境変数不正\n", stderr);
        return MIHFT_DECISION_PARSE_ERROR;
    }

    if (read_position("SCPOSF.csv", &ord, &pos) != 0) {
        fputs("SCPOSF読込不正\n", stderr);
        return MIHFT_DECISION_PARSE_ERROR;
    }

    if (read_customer("SCCUST.csv", ord.cif_no, &cust) != 0) {
        fputs("SCCUST読込不正\n", stderr);
        return MIHFT_DECISION_PARSE_ERROR;
    }

    tick = tick_for_tier(ord.tier);
    if (ord.ord_type == 'L' && ord.price % tick != 0) {
        return MIHFT_DECISION_REJECT_TICK;
    }

    if (mul_i64_sat(ord.qty, ord.price, &order_notional) != 0 ||
        order_notional > (int64_t)MIHFT_MAX_NOTIONAL) {
        return MIHFT_DECISION_REJECT_NOTIONAL;
    }

    overflow |= calc_incremental_margin(&ord, &pos, &incr_margin);
    overflow |= add_i64_sat(cust.group_used_amt, cust.acct_used_amt, &total_used);
    overflow |= add_i64_sat(total_used, incr_margin, &total_used);

    if (overflow != 0 || total_used > cust.group_limit) {
        return MIHFT_DECISION_REJECT_MARGIN;
    }

    return MIHFT_DECISION_ACCEPT;
}
