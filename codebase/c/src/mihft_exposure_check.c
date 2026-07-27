/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20190416  小林 直樹 (E-252)  初版作成、建玉エクスポージャ確認処理を追加
 */
#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_OK_ACCEPT 0
#define MIHFT_NG_MARGIN 4
#define MIHFT_NG_NOTIONAL 8
#define MIHFT_NG_TICK 12
#define MIHFT_HARD_ERROR 99

#define MAX_LINE_LEN 512
#define MAX_POS 4096
#define MAX_MKT 4096
#define MAX_EXPR 4096
#define MAX_ID_LEN 64
#define MAX_TS_LEN 32

typedef struct {
    char cif_no[MAX_ID_LEN];
    char instr_code[MAX_ID_LEN];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
} pos_rec_t;

typedef struct {
    char order_id[MAX_ID_LEN];
    char cif_no[MAX_ID_LEN];
    char instr_code[MAX_ID_LEN];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} ord_rec_t;

typedef struct {
    char instr_code[MAX_ID_LEN];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    char tick_ts[MAX_TS_LEN];
} mkt_rec_t;

typedef struct {
    char cif_no[MAX_ID_LEN];
    char instr_code[MAX_ID_LEN];
    int64_t net_notional_amt;
    int64_t buy_open_amt;
    int64_t sell_open_amt;
    char updated_ts[MAX_TS_LEN];
} expr_rec_t;

static int64_t abs_i64(int64_t v)
{
    if (v == INT64_MIN) {
        return INT64_MAX;
    }
    return v < 0 ? -v : v;
}

static int copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t len;

    if (dst_sz == 0) {
        return 0;
    }
    len = strlen(src);
    while (len > 0 && (src[len - 1] == '\n' || src[len - 1] == '\r')) {
        len--;
    }
    if (len >= dst_sz) {
        return 0;
    }
    memcpy(dst, src, len);
    dst[len] = '\0';
    return 1;
}

static int split_csv(char *line, char **cols, size_t need)
{
    size_t n = 0;
    char *p = line;

    while (n < need) {
        cols[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p = '\0';
        p++;
    }
    return n == need && strchr(cols[need - 1], ',') == NULL;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || (*end != '\0' && *end != '\n' && *end != '\r')) {
        return 0;
    }
    *out = (int64_t)v;
    return 1;
}

static int parse_int(const char *s, int *out)
{
    int64_t v;

    if (!parse_i64(s, &v) || v < INT_MIN || v > INT_MAX) {
        return 0;
    }
    *out = (int)v;
    return 1;
}

static int add_overflow_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return 1;
    }
    *out = a + b;
    return 0;
}

static int mul_overflow_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a != 0 && b != 0 && abs_i64(a) > INT64_MAX / abs_i64(b)) {
        return 1;
    }
    *out = a * b;
    return 0;
}

static int tier_margin_bp(int tier, int *bp)
{
    if (tier == 1) {
        *bp = 1000;
        return 1;
    }
    if (tier == 2) {
        *bp = 2000;
        return 1;
    }
    if (tier == 3) {
        *bp = 4000;
        return 1;
    }
    return 0;
}

static int tier_tick(int tier, int64_t *tick)
{
    if (tier == 1) {
        *tick = 100;
        return 1;
    }
    if (tier == 2) {
        *tick = 500;
        return 1;
    }
    if (tier == 3) {
        *tick = 1000;
        return 1;
    }
    return 0;
}

static int load_pos(pos_rec_t *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen("SCPOSF.csv", "r");
    char line[MAX_LINE_LEN];

    *count = 0;
    if (fp == NULL) {
        return 0;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        char *c[5];
        if (*count >= cap || !split_csv(line, c, 5)) {
            fclose(fp);
            return 0;
        }
        if (!copy_field(rows[*count].cif_no, sizeof rows[*count].cif_no, c[0]) ||
            !copy_field(rows[*count].instr_code, sizeof rows[*count].instr_code, c[1]) ||
            !parse_i64(c[2], &rows[*count].net_qty) ||
            !parse_i64(c[3], &rows[*count].avg_amt) ||
            !parse_i64(c[4], &rows[*count].rlzd_amt)) {
            fclose(fp);
            return 0;
        }
        (*count)++;
    }
    fclose(fp);
    return 1;
}

static int load_mkt(mkt_rec_t *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen("SCMKTD.csv", "r");
    char line[MAX_LINE_LEN];

    *count = 0;
    if (fp == NULL) {
        return 0;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        char *c[6];
        if (*count >= cap || !split_csv(line, c, 6)) {
            fclose(fp);
            return 0;
        }
        if (!copy_field(rows[*count].instr_code, sizeof rows[*count].instr_code, c[0]) ||
            !parse_i64(c[1], &rows[*count].bid_amt) ||
            !parse_i64(c[2], &rows[*count].ask_amt) ||
            !parse_i64(c[3], &rows[*count].last_amt) ||
            !parse_i64(c[4], &rows[*count].vol_qty) ||
            !copy_field(rows[*count].tick_ts, sizeof rows[*count].tick_ts, c[5])) {
            fclose(fp);
            return 0;
        }
        (*count)++;
    }
    fclose(fp);
    return 1;
}

static int load_expr(expr_rec_t *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen("SCEXPR.csv", "r");
    char line[MAX_LINE_LEN];

    *count = 0;
    if (fp == NULL) {
        return 0;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        char *c[6];
        if (*count >= cap || !split_csv(line, c, 6)) {
            fclose(fp);
            return 0;
        }
        if (!copy_field(rows[*count].cif_no, sizeof rows[*count].cif_no, c[0]) ||
            !copy_field(rows[*count].instr_code, sizeof rows[*count].instr_code, c[1]) ||
            !parse_i64(c[2], &rows[*count].net_notional_amt) ||
            !parse_i64(c[3], &rows[*count].buy_open_amt) ||
            !parse_i64(c[4], &rows[*count].sell_open_amt) ||
            !copy_field(rows[*count].updated_ts, sizeof rows[*count].updated_ts, c[5])) {
            fclose(fp);
            return 0;
        }
        (*count)++;
    }
    fclose(fp);
    return 1;
}

static const pos_rec_t *find_pos(const pos_rec_t *rows, size_t count, const char *cif, const char *instr)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(rows[i].cif_no, cif) == 0 && strcmp(rows[i].instr_code, instr) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static const mkt_rec_t *find_mkt(const mkt_rec_t *rows, size_t count, const char *instr)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(rows[i].instr_code, instr) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static const expr_rec_t *find_expr(const expr_rec_t *rows, size_t count, const char *cif, const char *instr)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(rows[i].cif_no, cif) == 0 && strcmp(rows[i].instr_code, instr) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static int read_order(FILE *fp, ord_rec_t *ord, int *eof_reached)
{
    char line[MAX_LINE_LEN];
    char *c[9];

    *eof_reached = 0;
    if (fgets(line, sizeof line, fp) == NULL) {
        if (ferror(fp)) {
            return 0;
        }
        *eof_reached = 1;
        return 1;
    }
    if (!split_csv(line, c, 9)) {
        return 0;
    }
    if (!copy_field(ord->order_id, sizeof ord->order_id, c[0]) ||
        !copy_field(ord->cif_no, sizeof ord->cif_no, c[1]) ||
        !copy_field(ord->instr_code, sizeof ord->instr_code, c[2]) ||
        !copy_field(ord->tif_code, sizeof ord->tif_code, c[5]) ||
        strlen(c[3]) != 1 || strlen(c[4]) != 1 ||
        !parse_i64(c[6], &ord->ord_qty) ||
        !parse_i64(c[7], &ord->price_amt) ||
        !parse_int(c[8], &ord->instr_tier)) {
        return 0;
    }
    ord->side_kbn = c[3][0];
    ord->ord_type = c[4][0];
    return 1;
}

static void make_ts(char *buf, size_t sz)
{
    time_t now = time(NULL);
    struct tm *tmv = localtime(&now);

    if (tmv == NULL || strftime(buf, sz, "%Y%m%d%H%M%S", tmv) == 0) {
        copy_field(buf, sz, "00000000000000");
    }
}

static int write_decision(FILE *fp, int64_t seq, const ord_rec_t *ord, int cd,
                          const char *reason, int64_t notional, int64_t limit_used,
                          const char *ts)
{
    return fprintf(fp, "D%012lld,%s,%s,%s,%d,%s,%lld,%lld,%s\n",
                   (long long)seq, ord->order_id, ord->cif_no, ord->instr_code,
                   cd, reason, (long long)notional, (long long)limit_used, ts) >= 0;
}

static int write_reject(FILE *fp, int64_t seq, const ord_rec_t *ord, int cd,
                        const char *detail, const char *ts)
{
    return fprintf(fp, "R%012lld,%s,%s,%s,%d,%s,%s\n",
                   (long long)seq, ord->order_id, ord->cif_no, ord->instr_code,
                   cd, detail, ts) >= 0;
}

static int price_for_order(const ord_rec_t *ord, const mkt_rec_t *mkt, int64_t *price)
{
    if (ord->ord_type == 'L') {
        *price = ord->price_amt;
        return *price > 0;
    }
    if (ord->ord_type != 'M') {
        return 0;
    }
    if (ord->side_kbn == 'B') {
        *price = mkt->ask_amt > 0 ? mkt->ask_amt : mkt->last_amt;
        return *price > 0;
    }
    if (ord->side_kbn == 'S') {
        *price = mkt->bid_amt > 0 ? mkt->bid_amt : mkt->last_amt;
        return *price > 0;
    }
    return 0;
}

static int decide_order(const ord_rec_t *ord, const pos_rec_t *pos, const mkt_rec_t *mkt,
                        const expr_rec_t *expr, int64_t *notional_out, int64_t *limit_out,
                        const char **reason_out)
{
    int margin_bp;
    int64_t tick;
    int64_t exec_price;
    int64_t order_notional;
    int64_t pos_notional;
    int64_t base_net;
    int64_t open_add;
    int64_t signed_order;
    int64_t after_net;
    int64_t limit_used;
    int expanding;

    if (ord->ord_qty <= 0 || !tier_margin_bp(ord->instr_tier, &margin_bp) ||
        !tier_tick(ord->instr_tier, &tick) || mkt == NULL ||
        (ord->side_kbn != 'B' && ord->side_kbn != 'S') ||
        (strcmp(ord->tif_code, "DAY") != 0 && strcmp(ord->tif_code, "IOC") != 0 &&
         strcmp(ord->tif_code, "FOK") != 0)) {
        *notional_out = 0;
        *limit_out = 0;
        *reason_out = "入力不備";
        return MIHFT_NG_NOTIONAL;
    }
    if (!price_for_order(ord, mkt, &exec_price)) {
        *notional_out = 0;
        *limit_out = 0;
        *reason_out = "価格不備";
        return MIHFT_NG_NOTIONAL;
    }
    if (ord->ord_type == 'L' && exec_price % tick != 0) {
        *notional_out = exec_price;
        *limit_out = tick;
        *reason_out = "呼値不正";
        return MIHFT_NG_TICK;
    }
    if (mul_overflow_i64(ord->ord_qty, exec_price, &order_notional)) {
        *notional_out = INT64_MAX;
        *limit_out = MIHFT_MAX_NOTIONAL;
        *reason_out = "金額桁溢れ";
        return MIHFT_NG_NOTIONAL;
    }

    pos_notional = 0;
    if (pos != NULL && mul_overflow_i64(pos->net_qty, pos->avg_amt, &pos_notional)) {
        *notional_out = INT64_MAX;
        *limit_out = MIHFT_MAX_NOTIONAL;
        *reason_out = "建玉桁溢れ";
        return MIHFT_NG_NOTIONAL;
    }

    base_net = pos_notional;
    if (expr != NULL && add_overflow_i64(base_net, expr->net_notional_amt, &base_net)) {
        *notional_out = INT64_MAX;
        *limit_out = MIHFT_MAX_NOTIONAL;
        *reason_out = "未約定桁溢れ";
        return MIHFT_NG_NOTIONAL;
    }

    signed_order = ord->side_kbn == 'B' ? order_notional : -order_notional;
    if (add_overflow_i64(base_net, signed_order, &after_net)) {
        *notional_out = INT64_MAX;
        *limit_out = MIHFT_MAX_NOTIONAL;
        *reason_out = "注文後桁溢れ";
        return MIHFT_NG_NOTIONAL;
    }

    open_add = 0;
    if (expr != NULL) {
        open_add = ord->side_kbn == 'B' ? expr->buy_open_amt : expr->sell_open_amt;
    }
    if (add_overflow_i64(order_notional, open_add, &open_add)) {
        *notional_out = INT64_MAX;
        *limit_out = MIHFT_MAX_NOTIONAL;
        *reason_out = "片側桁溢れ";
        return MIHFT_NG_NOTIONAL;
    }

    expanding = (pos != NULL && pos->net_qty > 0 && ord->side_kbn == 'B') ||
                (pos != NULL && pos->net_qty < 0 && ord->side_kbn == 'S');
    limit_used = expanding ? (MIHFT_MAX_NOTIONAL * 8) / 10 : MIHFT_MAX_NOTIONAL;

    *notional_out = abs_i64(after_net);
    *limit_out = limit_used;
    if (abs_i64(after_net) > limit_used) {
        *reason_out = expanding ? "建玉拡大超過" : "想定元本超過";
        return MIHFT_NG_NOTIONAL;
    }
    if (open_add > (limit_used * (int64_t)margin_bp) / 10000) {
        *notional_out = open_add;
        *reason_out = "証拠金超過";
        return MIHFT_NG_MARGIN;
    }

    *reason_out = "許容";
    return MIHFT_OK_ACCEPT;
}

int main(void)
{
    pos_rec_t pos[MAX_POS];
    mkt_rec_t mkt[MAX_MKT];
    expr_rec_t expr[MAX_EXPR];
    size_t pos_count;
    size_t mkt_count;
    size_t expr_count;
    FILE *ord_fp;
    FILE *dec_fp;
    FILE *rjct_fp;
    int rc = MIHFT_OK_ACCEPT;
    int64_t dec_seq = 1;
    int64_t rjct_seq = 1;

    if (!load_pos(pos, MAX_POS, &pos_count) ||
        !load_mkt(mkt, MAX_MKT, &mkt_count) ||
        !load_expr(expr, MAX_EXPR, &expr_count)) {
        fprintf(stderr, "初期入力ファイル読込失敗\n");
        return MIHFT_HARD_ERROR;
    }

    ord_fp = fopen("SCORDF.csv", "r");
    dec_fp = fopen("HFDEC.dat", "w");
    rjct_fp = fopen("HFRJCT.dat", "w");
    if (ord_fp == NULL || dec_fp == NULL || rjct_fp == NULL) {
        fprintf(stderr, "入出力ファイルオープン失敗\n");
        if (ord_fp != NULL) {
            fclose(ord_fp);
        }
        if (dec_fp != NULL) {
            fclose(dec_fp);
        }
        if (rjct_fp != NULL) {
            fclose(rjct_fp);
        }
        return MIHFT_HARD_ERROR;
    }

    for (;;) {
        ord_rec_t ord;
        const pos_rec_t *p;
        const mkt_rec_t *m;
        const expr_rec_t *e;
        const char *reason;
        char ts[MAX_TS_LEN];
        int eof_reached;
        int decision;
        int64_t notional;
        int64_t limit_used;

        if (!read_order(ord_fp, &ord, &eof_reached)) {
            fprintf(stderr, "注文入力レコード解析失敗\n");
            rc = MIHFT_HARD_ERROR;
            break;
        }
        if (eof_reached) {
            break;
        }

        p = find_pos(pos, pos_count, ord.cif_no, ord.instr_code);
        m = find_mkt(mkt, mkt_count, ord.instr_code);
        e = find_expr(expr, expr_count, ord.cif_no, ord.instr_code);
        make_ts(ts, sizeof ts);

        decision = decide_order(&ord, p, m, e, &notional, &limit_used, &reason);
        if (!write_decision(dec_fp, dec_seq++, &ord, decision, reason, notional, limit_used, ts)) {
            fprintf(stderr, "判定出力書込失敗\n");
            rc = MIHFT_HARD_ERROR;
            break;
        }
        if (decision != MIHFT_OK_ACCEPT) {
            if (!write_reject(rjct_fp, rjct_seq++, &ord, decision, reason, ts)) {
                fprintf(stderr, "拒否出力書込失敗\n");
                rc = MIHFT_HARD_ERROR;
                break;
            }
            if (rc == MIHFT_OK_ACCEPT) {
                rc = decision;
            }
        }
    }

    if (fclose(ord_fp) != 0 || fclose(dec_fp) != 0 || fclose(rjct_fp) != 0) {
        fprintf(stderr, "ファイルクローズ失敗\n");
        return MIHFT_HARD_ERROR;
    }

    return rc;
}
