/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20231114  市場基盤部  初版作成、SCEXECからSCTCAPを生成
 * 1.01  20240414  市場基盤部  注文照合、約定累計、桁あふれ検査を追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_RC_ACCEPT 0
#define MIHFT_RC_REJECT_MARGIN 4
#define MIHFT_RC_REJECT_NOTIONAL 8
#define MIHFT_RC_REJECT_TICK 12
#define MIHFT_RC_IOERR 16
#define MIHFT_LINE_MAX 512
#define MIHFT_ORDERS_MAX 20000
#define MIHFT_STATES_MAX 20000
#define MIHFT_ID_MAX 64
#define MIHFT_CODE_MAX 16
#define MIHFT_TS_MAX 32

typedef struct {
    char order_id[MIHFT_ID_MAX];
    char cif_no[MIHFT_ID_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    char ord_type;
    char tif_code[MIHFT_CODE_MAX];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} MihftOrderRow;

typedef struct {
    char exec_id[MIHFT_ID_MAX];
    char order_id[MIHFT_ID_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[MIHFT_TS_MAX];
} MihftExecRow;

typedef struct {
    char order_id[MIHFT_ID_MAX];
    int64_t cum_qty;
    int64_t cum_amt;
} MihftOrderState;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n;

    if (dstsz == 0) {
        return 0;
    }

    n = strlen(src);
    if (n >= dstsz) {
        return 0;
    }

    memcpy(dst, src, n + 1);
    return 1;
}

static int mihft_next_field(char **cur, char *dst, size_t dstsz)
{
    char *p;
    char *comma;

    if (*cur == NULL) {
        return 0;
    }

    p = *cur;
    comma = strchr(p, ',');
    if (comma != NULL) {
        *comma = '\0';
        *cur = comma + 1;
    } else {
        *cur = NULL;
    }

    return mihft_copy_field(dst, dstsz, p);
}

static int mihft_parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s[0] == '\0') {
        return 0;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return 0;
    }

    *out = (int64_t)v;
    return 1;
}

static int mihft_parse_int(const char *s, int *out)
{
    int64_t v;

    if (!mihft_parse_i64(s, &v) || v < INT_MIN || v > INT_MAX) {
        return 0;
    }

    *out = (int)v;
    return 1;
}

static int mihft_mul_over_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a < 0 || b < 0) {
        return 1;
    }
    if (a != 0 && b > INT64_MAX / a) {
        return 1;
    }

    *out = a * b;
    return 0;
}

static int mihft_add_over_i64(int64_t a, int64_t b, int64_t *out)
{
    if (b > 0 && a > INT64_MAX - b) {
        return 1;
    }
    if (b < 0 && a < INT64_MIN - b) {
        return 1;
    }

    *out = a + b;
    return 0;
}

static int mihft_tier_rate_bp(int tier, int *rate_bp)
{
    if (tier == 1) {
        *rate_bp = 1000;
        return 1;
    }
    if (tier == 2) {
        *rate_bp = 2000;
        return 1;
    }
    if (tier == 3) {
        *rate_bp = 4000;
        return 1;
    }

    return 0;
}

static int mihft_tier_tick(int tier, int64_t *tick)
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

static int mihft_valid_side(char side)
{
    return side == 'B' || side == 'S';
}

static int mihft_valid_ord_type(char ord_type)
{
    return ord_type == 'L' || ord_type == 'M';
}

static int mihft_valid_tif(const char *tif)
{
    return strcmp(tif, "DAY") == 0 || strcmp(tif, "IOC") == 0 || strcmp(tif, "FOK") == 0;
}

static int mihft_parse_order(char *line, MihftOrderRow *row)
{
    char *cur = line;
    char side[MIHFT_CODE_MAX];
    char ord_type[MIHFT_CODE_MAX];
    char qty[MIHFT_CODE_MAX];
    char price[MIHFT_CODE_MAX];
    char tier[MIHFT_CODE_MAX];

    if (!mihft_next_field(&cur, row->order_id, sizeof(row->order_id)) ||
        !mihft_next_field(&cur, row->cif_no, sizeof(row->cif_no)) ||
        !mihft_next_field(&cur, row->instr_code, sizeof(row->instr_code)) ||
        !mihft_next_field(&cur, side, sizeof(side)) ||
        !mihft_next_field(&cur, ord_type, sizeof(ord_type)) ||
        !mihft_next_field(&cur, row->tif_code, sizeof(row->tif_code)) ||
        !mihft_next_field(&cur, qty, sizeof(qty)) ||
        !mihft_next_field(&cur, price, sizeof(price)) ||
        !mihft_next_field(&cur, tier, sizeof(tier))) {
        return 0;
    }

    if (cur != NULL || strlen(side) != 1 || strlen(ord_type) != 1) {
        return 0;
    }

    row->side_kbn = side[0];
    row->ord_type = ord_type[0];

    if (!mihft_valid_side(row->side_kbn) ||
        !mihft_valid_ord_type(row->ord_type) ||
        !mihft_valid_tif(row->tif_code) ||
        !mihft_parse_i64(qty, &row->ord_qty) ||
        !mihft_parse_i64(price, &row->price_amt) ||
        !mihft_parse_int(tier, &row->instr_tier)) {
        return 0;
    }

    return row->ord_qty > 0 && row->price_amt >= 0;
}

static int mihft_parse_exec(char *line, MihftExecRow *row)
{
    char *cur = line;
    char side[MIHFT_CODE_MAX];
    char qty[MIHFT_CODE_MAX];
    char amt[MIHFT_CODE_MAX];

    if (!mihft_next_field(&cur, row->exec_id, sizeof(row->exec_id)) ||
        !mihft_next_field(&cur, row->order_id, sizeof(row->order_id)) ||
        !mihft_next_field(&cur, row->instr_code, sizeof(row->instr_code)) ||
        !mihft_next_field(&cur, side, sizeof(side)) ||
        !mihft_next_field(&cur, qty, sizeof(qty)) ||
        !mihft_next_field(&cur, amt, sizeof(amt)) ||
        !mihft_next_field(&cur, row->exec_ts, sizeof(row->exec_ts))) {
        return 0;
    }

    if (cur != NULL || strlen(side) != 1) {
        return 0;
    }

    row->side_kbn = side[0];

    if (!mihft_valid_side(row->side_kbn) ||
        !mihft_parse_i64(qty, &row->fill_qty) ||
        !mihft_parse_i64(amt, &row->fill_amt)) {
        return 0;
    }

    return row->fill_qty > 0 && row->fill_amt > 0;
}

static int mihft_load_orders(const char *path, MihftOrderRow *orders, size_t *count)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    size_t n = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "SCORDFを開けません\n");
        return MIHFT_RC_IOERR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        MihftOrderRow row;

        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }

        if (n == MIHFT_ORDERS_MAX || !mihft_parse_order(line, &row)) {
            fclose(fp);
            fprintf(stderr, "SCORDFの形式が不正です\n");
            return MIHFT_RC_IOERR;
        }

        orders[n++] = row;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCORDFの読込に失敗しました\n");
        return MIHFT_RC_IOERR;
    }

    fclose(fp);
    *count = n;
    return MIHFT_RC_ACCEPT;
}

static const MihftOrderRow *mihft_find_order(const MihftOrderRow *orders, size_t count, const char *order_id)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(orders[i].order_id, order_id) == 0) {
            return &orders[i];
        }
    }

    return NULL;
}

static MihftOrderState *mihft_find_or_add_state(MihftOrderState *states, size_t *count, const char *order_id)
{
    size_t i;

    for (i = 0; i < *count; i++) {
        if (strcmp(states[i].order_id, order_id) == 0) {
            return &states[i];
        }
    }

    if (*count == MIHFT_STATES_MAX) {
        return NULL;
    }

    if (!mihft_copy_field(states[*count].order_id, sizeof(states[*count].order_id), order_id)) {
        return NULL;
    }

    states[*count].cum_qty = 0;
    states[*count].cum_amt = 0;
    (*count)++;
    return &states[*count - 1];
}

static int mihft_validate_trade(const MihftExecRow *exec, const MihftOrderRow *ord)
{
    int rate_bp;
    int64_t tick;
    int64_t limit_notional;
    int64_t order_notional;
    int64_t margin;

    if (strcmp(exec->instr_code, ord->instr_code) != 0 || exec->side_kbn != ord->side_kbn) {
        return MIHFT_RC_IOERR;
    }

    if (!mihft_tier_rate_bp(ord->instr_tier, &rate_bp) || !mihft_tier_tick(ord->instr_tier, &tick)) {
        return MIHFT_RC_IOERR;
    }

    if (exec->fill_amt > MIHFT_MAX_NOTIONAL) {
        return MIHFT_RC_REJECT_NOTIONAL;
    }

    if (ord->ord_type == 'L' && ord->price_amt % tick != 0) {
        return MIHFT_RC_REJECT_TICK;
    }

    if (mihft_mul_over_i64(ord->ord_qty, ord->price_amt, &order_notional)) {
        return MIHFT_RC_REJECT_NOTIONAL;
    }

    if (mihft_mul_over_i64(order_notional, (int64_t)rate_bp, &margin)) {
        return MIHFT_RC_REJECT_MARGIN;
    }

    limit_notional = margin / 10000;
    if (ord->side_kbn == 'B' && exec->fill_amt > limit_notional) {
        return MIHFT_RC_REJECT_MARGIN;
    }

    return MIHFT_RC_ACCEPT;
}

static int mihft_write_state(MihftOrderState *states, size_t *state_count, const MihftExecRow *exec, const MihftOrderRow *ord)
{
    MihftOrderState *st;
    int64_t next_qty;
    int64_t next_amt;

    st = mihft_find_or_add_state(states, state_count, exec->order_id);
    if (st == NULL) {
        fprintf(stderr, "注文状態の領域が不足しています\n");
        return MIHFT_RC_IOERR;
    }

    if (mihft_add_over_i64(st->cum_qty, exec->fill_qty, &next_qty) ||
        mihft_add_over_i64(st->cum_amt, exec->fill_amt, &next_amt)) {
        fprintf(stderr, "注文状態の累計が桁あふれしました\n");
        return MIHFT_RC_IOERR;
    }

    if (next_qty > ord->ord_qty) {
        return MIHFT_RC_REJECT_NOTIONAL;
    }

    st->cum_qty = next_qty;
    st->cum_amt = next_amt;
    return MIHFT_RC_ACCEPT;
}

static int mihft_emit_capture(FILE *out, int64_t seq, const MihftExecRow *exec, const MihftOrderRow *ord)
{
    int rc;

    rc = fprintf(out,
                 "T%012lld,%s,%s,%s,%s,%lld,%lld,%s\n",
                 (long long)seq,
                 exec->exec_id,
                 exec->order_id,
                 exec->instr_code,
                 ord->cif_no,
                 (long long)exec->fill_qty,
                 (long long)exec->fill_amt,
                 exec->exec_ts);

    if (rc < 0) {
        fprintf(stderr, "SCTCAPの書込に失敗しました\n");
        return MIHFT_RC_IOERR;
    }

    return MIHFT_RC_ACCEPT;
}

int main(void)
{
    MihftOrderRow orders[MIHFT_ORDERS_MAX];
    MihftOrderState states[MIHFT_STATES_MAX];
    size_t order_count = 0;
    size_t state_count = 0;
    FILE *in;
    FILE *out;
    char line[MIHFT_LINE_MAX];
    int64_t trade_seq = 1;
    int decision = MIHFT_RC_ACCEPT;
    int rc;

    rc = mihft_load_orders("SCORDF.csv", orders, &order_count);
    if (rc != MIHFT_RC_ACCEPT) {
        return rc;
    }

    in = fopen("SCEXEC.csv", "r");
    if (in == NULL) {
        fprintf(stderr, "SCEXECを開けません\n");
        return MIHFT_RC_IOERR;
    }

    out = fopen("SCTCAP.csv", "w");
    if (out == NULL) {
        fclose(in);
        fprintf(stderr, "SCTCAPを開けません\n");
        return MIHFT_RC_IOERR;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        MihftExecRow exec;
        const MihftOrderRow *ord;

        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }

        if (!mihft_parse_exec(line, &exec)) {
            fclose(out);
            fclose(in);
            fprintf(stderr, "SCEXECの形式が不正です\n");
            return MIHFT_RC_IOERR;
        }

        ord = mihft_find_order(orders, order_count, exec.order_id);
        if (ord == NULL) {
            fclose(out);
            fclose(in);
            fprintf(stderr, "注文が見つかりません\n");
            return MIHFT_RC_IOERR;
        }

        rc = mihft_validate_trade(&exec, ord);
        if (rc != MIHFT_RC_ACCEPT) {
            if (rc > decision) {
                decision = rc;
            }
            continue;
        }

        rc = mihft_write_state(states, &state_count, &exec, ord);
        if (rc != MIHFT_RC_ACCEPT) {
            if (rc == MIHFT_RC_IOERR) {
                fclose(out);
                fclose(in);
                return rc;
            }
            if (rc > decision) {
                decision = rc;
            }
            continue;
        }

        rc = mihft_emit_capture(out, trade_seq, &exec, ord);
        if (rc != MIHFT_RC_ACCEPT) {
            fclose(out);
            fclose(in);
            return rc;
        }

        trade_seq++;
    }

    if (ferror(in)) {
        fclose(out);
        fclose(in);
        fprintf(stderr, "SCEXECの読込に失敗しました\n");
        return MIHFT_RC_IOERR;
    }

    if (fclose(out) != 0) {
        fclose(in);
        fprintf(stderr, "SCTCAPの完了処理に失敗しました\n");
        return MIHFT_RC_IOERR;
    }

    fclose(in);
    return decision;
}
