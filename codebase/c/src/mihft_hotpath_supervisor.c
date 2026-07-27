/* 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240709  小林 直樹 (E-252)  初版作成、ホットパス合成監視ドライバ
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_MAX_REC 4096
#define MIHFT_LINE_MAX 1024
#define MIHFT_KEY_MAX 64
#define MIHFT_NAME_MAX 128

#define MIHFT_DEC_ACCEPT 0
#define MIHFT_DEC_REJECT_MARGIN 4
#define MIHFT_DEC_REJECT_NOTIONAL 8
#define MIHFT_DEC_REJECT_TICK 12

typedef struct {
    char order_id[MIHFT_KEY_MAX];
    char cif_no[MIHFT_KEY_MAX];
    char instr_code[MIHFT_KEY_MAX];
    char side_kbn;
    char ord_type;
    char tif_code[8];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} HotOrder;

typedef struct {
    char instr_code[MIHFT_KEY_MAX];
    char side_kbn;
    int64_t level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;
    int64_t entry_ts;
} BookRow;

typedef struct {
    char instr_code[MIHFT_KEY_MAX];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    int64_t tick_ts;
} MarketRow;

typedef struct {
    char exec_id[MIHFT_KEY_MAX];
    char order_id[MIHFT_KEY_MAX];
    char instr_code[MIHFT_KEY_MAX];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    int64_t exec_ts;
} ExecRow;

typedef struct {
    char cif_no[MIHFT_KEY_MAX];
    char instr_code[MIHFT_KEY_MAX];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
} PosRow;

typedef struct {
    char instr_code[MIHFT_KEY_MAX];
    char instr_name[MIHFT_NAME_MAX];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[16];
} InstRow;

typedef struct {
    char cif_no[MIHFT_KEY_MAX];
    int64_t group_limit;
    int64_t group_used_amt;
    int64_t acct_used_amt;
} CustRow;

typedef struct {
    char board_code[16];
    int64_t fee_rate;
    int64_t min_fee_amt;
} FeeRow;

typedef struct {
    char sess_dt[16];
    char sess_kbn[16];
    int64_t open_ts;
    int64_t close_ts;
} CalRow;

typedef struct {
    char decision_id[MIHFT_KEY_MAX];
    char order_id[MIHFT_KEY_MAX];
    char cif_no[MIHFT_KEY_MAX];
    char instr_code[MIHFT_KEY_MAX];
    int decision_cd;
    char reason_cd[32];
    int64_t notional_amt;
    int64_t limit_used_amt;
    int64_t decision_ts;
} DecRow;

typedef struct {
    char reject_id[MIHFT_KEY_MAX];
    char order_id[MIHFT_KEY_MAX];
    char cif_no[MIHFT_KEY_MAX];
    char instr_code[MIHFT_KEY_MAX];
    char reject_cd[32];
    char detail_cd[32];
    int64_t reject_ts;
} RejectRow;

typedef struct {
    char scope_key[MIHFT_KEY_MAX];
    int kill_flg;
    char reason_cd[32];
    int64_t updated_ts;
    char updated_by[32];
} KillRow;

typedef struct {
    char bucket_key[MIHFT_KEY_MAX];
    int64_t window_ts;
    int64_t order_cnt;
    int64_t notional_amt;
    int64_t drop_cnt;
} RateRow;

typedef struct {
    char cif_no[MIHFT_KEY_MAX];
    char instr_code[MIHFT_KEY_MAX];
    int64_t net_notional_amt;
    int64_t buy_open_amt;
    int64_t sell_open_amt;
    int64_t updated_ts;
} ExprRow;

typedef struct {
    char cif_no[MIHFT_KEY_MAX];
    int instr_tier;
    int64_t max_notional_amt;
    int64_t max_order_qty;
    int64_t max_rate_cnt;
    int64_t updated_ts;
} LimitRow;

typedef struct {
    HotOrder orders[MIHFT_MAX_REC];
    BookRow books[MIHFT_MAX_REC];
    MarketRow markets[MIHFT_MAX_REC];
    ExecRow execs[MIHFT_MAX_REC];
    PosRow positions[MIHFT_MAX_REC];
    InstRow insts[MIHFT_MAX_REC];
    CustRow custs[MIHFT_MAX_REC];
    FeeRow fees[MIHFT_MAX_REC];
    CalRow cals[MIHFT_MAX_REC];
    DecRow decs[MIHFT_MAX_REC];
    RejectRow rejects[MIHFT_MAX_REC];
    KillRow kills[MIHFT_MAX_REC];
    RateRow rates[MIHFT_MAX_REC];
    ExprRow exprs[MIHFT_MAX_REC];
    LimitRow limits[MIHFT_MAX_REC];
    size_t n_orders, n_books, n_markets, n_execs, n_positions, n_insts, n_custs;
    size_t n_fees, n_cals, n_decs, n_rejects, n_kills, n_rates, n_exprs, n_limits;
} Store;

static void trim(char *s)
{
    size_t n;
    while (*s != '\0' && isspace((unsigned char)*s)) {
        memmove(s, s + 1, strlen(s));
    }
    n = strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1])) {
        s[--n] = '\0';
    }
}

static int split_csv(char *line, char **out, size_t cap)
{
    size_t n = 0;
    char *p = line;
    while (n < cap) {
        out[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    for (size_t i = 0; i < n; i++) {
        trim(out[i]);
    }
    return (int)n;
}

static int parse_i64(const char *s, int64_t *v)
{
    char *end = NULL;
    long long x;
    errno = 0;
    if (s == NULL || *s == '\0') {
        return -1;
    }
    x = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }
    *v = (int64_t)x;
    return 0;
}

static int parse_int(const char *s, int *v)
{
    int64_t x;
    if (parse_i64(s, &x) != 0 || x < INT32_MIN || x > INT32_MAX) {
        return -1;
    }
    *v = (int)x;
    return 0;
}

static void copy_text(char *dst, size_t cap, const char *src)
{
    if (cap == 0) {
        return;
    }
    snprintf(dst, cap, "%s", src == NULL ? "" : src);
}

static FILE *open_input(const char *base)
{
    char name[128];
    FILE *fp = fopen(base, "r");
    if (fp != NULL) {
        return fp;
    }
    snprintf(name, sizeof(name), "%s.csv", base);
    return fopen(name, "r");
}

static int read_each(const char *name, int (*load)(char **, int, void *), void *ctx)
{
    FILE *fp = open_input(name);
    char line[MIHFT_LINE_MAX];
    int lineno = 0;
    if (fp == NULL) {
        fprintf(stderr, "入力を開けません:%s\n", name);
        return -1;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[16];
        int n;
        lineno++;
        line[strcspn(line, "\r\n")] = '\0';
        trim(line);
        if (line[0] == '\0' || line[0] == '#') {
            continue;
        }
        n = split_csv(line, cols, 16);
        if (lineno == 1 && strchr(cols[0], '-') != NULL) {
            continue;
        }
        if (load(cols, n, ctx) != 0) {
            fprintf(stderr, "入力形式不正:%s:%d\n", name, lineno);
            fclose(fp);
            return -1;
        }
    }
    fclose(fp);
    return 0;
}

static int load_order(char **c, int n, void *ctx)
{
    Store *s = ctx;
    HotOrder *r;
    if (n != 9 || s->n_orders >= MIHFT_MAX_REC) return -1;
    r = &s->orders[s->n_orders++];
    copy_text(r->order_id, sizeof(r->order_id), c[0]);
    copy_text(r->cif_no, sizeof(r->cif_no), c[1]);
    copy_text(r->instr_code, sizeof(r->instr_code), c[2]);
    r->side_kbn = c[3][0];
    r->ord_type = c[4][0];
    copy_text(r->tif_code, sizeof(r->tif_code), c[5]);
    return parse_i64(c[6], &r->ord_qty) || parse_i64(c[7], &r->price_amt) || parse_int(c[8], &r->instr_tier);
}

static int load_book(char **c, int n, void *ctx)
{
    Store *s = ctx;
    BookRow *r;
    if (n != 7 || s->n_books >= MIHFT_MAX_REC) return -1;
    r = &s->books[s->n_books++];
    copy_text(r->instr_code, sizeof(r->instr_code), c[0]);
    r->side_kbn = c[1][0];
    return parse_i64(c[2], &r->level_cnt) || parse_i64(c[3], &r->price_amt) ||
           parse_i64(c[4], &r->book_qty) || parse_i64(c[5], &r->order_cnt) ||
           parse_i64(c[6], &r->entry_ts);
}

static int load_market(char **c, int n, void *ctx)
{
    Store *s = ctx;
    MarketRow *r;
    if (n != 6 || s->n_markets >= MIHFT_MAX_REC) return -1;
    r = &s->markets[s->n_markets++];
    copy_text(r->instr_code, sizeof(r->instr_code), c[0]);
    return parse_i64(c[1], &r->bid_amt) || parse_i64(c[2], &r->ask_amt) ||
           parse_i64(c[3], &r->last_amt) || parse_i64(c[4], &r->vol_qty) ||
           parse_i64(c[5], &r->tick_ts);
}

static int load_exec(char **c, int n, void *ctx)
{
    Store *s = ctx;
    ExecRow *r;
    if (n != 7 || s->n_execs >= MIHFT_MAX_REC) return -1;
    r = &s->execs[s->n_execs++];
    copy_text(r->exec_id, sizeof(r->exec_id), c[0]);
    copy_text(r->order_id, sizeof(r->order_id), c[1]);
    copy_text(r->instr_code, sizeof(r->instr_code), c[2]);
    r->side_kbn = c[3][0];
    return parse_i64(c[4], &r->fill_qty) || parse_i64(c[5], &r->fill_amt) || parse_i64(c[6], &r->exec_ts);
}

static int load_pos(char **c, int n, void *ctx)
{
    Store *s = ctx;
    PosRow *r;
    if (n != 5 || s->n_positions >= MIHFT_MAX_REC) return -1;
    r = &s->positions[s->n_positions++];
    copy_text(r->cif_no, sizeof(r->cif_no), c[0]);
    copy_text(r->instr_code, sizeof(r->instr_code), c[1]);
    return parse_i64(c[2], &r->net_qty) || parse_i64(c[3], &r->avg_amt) || parse_i64(c[4], &r->rlzd_amt);
}

static int load_inst(char **c, int n, void *ctx)
{
    Store *s = ctx;
    InstRow *r;
    if (n != 6 || s->n_insts >= MIHFT_MAX_REC) return -1;
    r = &s->insts[s->n_insts++];
    copy_text(r->instr_code, sizeof(r->instr_code), c[0]);
    copy_text(r->instr_name, sizeof(r->instr_name), c[1]);
    copy_text(r->board_code, sizeof(r->board_code), c[5]);
    return parse_int(c[2], &r->instr_tier) || parse_i64(c[3], &r->tick_amt) || parse_i64(c[4], &r->lot_qty);
}

static int load_cust(char **c, int n, void *ctx)
{
    Store *s = ctx;
    CustRow *r;
    if (n != 4 || s->n_custs >= MIHFT_MAX_REC) return -1;
    r = &s->custs[s->n_custs++];
    copy_text(r->cif_no, sizeof(r->cif_no), c[0]);
    return parse_i64(c[1], &r->group_limit) || parse_i64(c[2], &r->group_used_amt) || parse_i64(c[3], &r->acct_used_amt);
}

static int load_fee(char **c, int n, void *ctx)
{
    Store *s = ctx;
    FeeRow *r;
    if (n != 3 || s->n_fees >= MIHFT_MAX_REC) return -1;
    r = &s->fees[s->n_fees++];
    copy_text(r->board_code, sizeof(r->board_code), c[0]);
    return parse_i64(c[1], &r->fee_rate) || parse_i64(c[2], &r->min_fee_amt);
}

static int load_cal(char **c, int n, void *ctx)
{
    Store *s = ctx;
    CalRow *r;
    if (n != 4 || s->n_cals >= MIHFT_MAX_REC) return -1;
    r = &s->cals[s->n_cals++];
    copy_text(r->sess_dt, sizeof(r->sess_dt), c[0]);
    copy_text(r->sess_kbn, sizeof(r->sess_kbn), c[1]);
    return parse_i64(c[2], &r->open_ts) || parse_i64(c[3], &r->close_ts);
}

static int load_dec(char **c, int n, void *ctx)
{
    Store *s = ctx;
    DecRow *r;
    if (n != 9 || s->n_decs >= MIHFT_MAX_REC) return -1;
    r = &s->decs[s->n_decs++];
    copy_text(r->decision_id, sizeof(r->decision_id), c[0]);
    copy_text(r->order_id, sizeof(r->order_id), c[1]);
    copy_text(r->cif_no, sizeof(r->cif_no), c[2]);
    copy_text(r->instr_code, sizeof(r->instr_code), c[3]);
    copy_text(r->reason_cd, sizeof(r->reason_cd), c[5]);
    return parse_int(c[4], &r->decision_cd) || parse_i64(c[6], &r->notional_amt) ||
           parse_i64(c[7], &r->limit_used_amt) || parse_i64(c[8], &r->decision_ts);
}

static int load_reject(char **c, int n, void *ctx)
{
    Store *s = ctx;
    RejectRow *r;
    if (n != 7 || s->n_rejects >= MIHFT_MAX_REC) return -1;
    r = &s->rejects[s->n_rejects++];
    copy_text(r->reject_id, sizeof(r->reject_id), c[0]);
    copy_text(r->order_id, sizeof(r->order_id), c[1]);
    copy_text(r->cif_no, sizeof(r->cif_no), c[2]);
    copy_text(r->instr_code, sizeof(r->instr_code), c[3]);
    copy_text(r->reject_cd, sizeof(r->reject_cd), c[4]);
    copy_text(r->detail_cd, sizeof(r->detail_cd), c[5]);
    return parse_i64(c[6], &r->reject_ts);
}

static int load_kill(char **c, int n, void *ctx)
{
    Store *s = ctx;
    KillRow *r;
    if (n != 5 || s->n_kills >= MIHFT_MAX_REC) return -1;
    r = &s->kills[s->n_kills++];
    copy_text(r->scope_key, sizeof(r->scope_key), c[0]);
    copy_text(r->reason_cd, sizeof(r->reason_cd), c[2]);
    copy_text(r->updated_by, sizeof(r->updated_by), c[4]);
    return parse_int(c[1], &r->kill_flg) || parse_i64(c[3], &r->updated_ts);
}

static int load_rate(char **c, int n, void *ctx)
{
    Store *s = ctx;
    RateRow *r;
    if (n != 5 || s->n_rates >= MIHFT_MAX_REC) return -1;
    r = &s->rates[s->n_rates++];
    copy_text(r->bucket_key, sizeof(r->bucket_key), c[0]);
    return parse_i64(c[1], &r->window_ts) || parse_i64(c[2], &r->order_cnt) ||
           parse_i64(c[3], &r->notional_amt) || parse_i64(c[4], &r->drop_cnt);
}

static int load_expr(char **c, int n, void *ctx)
{
    Store *s = ctx;
    ExprRow *r;
    if (n != 6 || s->n_exprs >= MIHFT_MAX_REC) return -1;
    r = &s->exprs[s->n_exprs++];
    copy_text(r->cif_no, sizeof(r->cif_no), c[0]);
    copy_text(r->instr_code, sizeof(r->instr_code), c[1]);
    return parse_i64(c[2], &r->net_notional_amt) || parse_i64(c[3], &r->buy_open_amt) ||
           parse_i64(c[4], &r->sell_open_amt) || parse_i64(c[5], &r->updated_ts);
}

static int load_limit(char **c, int n, void *ctx)
{
    Store *s = ctx;
    LimitRow *r;
    if (n != 6 || s->n_limits >= MIHFT_MAX_REC) return -1;
    r = &s->limits[s->n_limits++];
    copy_text(r->cif_no, sizeof(r->cif_no), c[0]);
    return parse_int(c[1], &r->instr_tier) || parse_i64(c[2], &r->max_notional_amt) ||
           parse_i64(c[3], &r->max_order_qty) || parse_i64(c[4], &r->max_rate_cnt) ||
           parse_i64(c[5], &r->updated_ts);
}

static const InstRow *find_inst(const Store *s, const char *instr)
{
    for (size_t i = 0; i < s->n_insts; i++) if (strcmp(s->insts[i].instr_code, instr) == 0) return &s->insts[i];
    return NULL;
}

static const MarketRow *find_market(const Store *s, const char *instr)
{
    const MarketRow *best = NULL;
    for (size_t i = 0; i < s->n_markets; i++) {
        if (strcmp(s->markets[i].instr_code, instr) == 0 && (best == NULL || s->markets[i].tick_ts > best->tick_ts)) {
            best = &s->markets[i];
        }
    }
    return best;
}

static const CustRow *find_cust(const Store *s, const char *cif)
{
    for (size_t i = 0; i < s->n_custs; i++) if (strcmp(s->custs[i].cif_no, cif) == 0) return &s->custs[i];
    return NULL;
}

static const LimitRow *find_limit(const Store *s, const char *cif, int tier)
{
    for (size_t i = 0; i < s->n_limits; i++) {
        if (strcmp(s->limits[i].cif_no, cif) == 0 && s->limits[i].instr_tier == tier) return &s->limits[i];
    }
    return NULL;
}

static RateRow *find_rate(Store *s, const char *bucket)
{
    for (size_t i = 0; i < s->n_rates; i++) if (strcmp(s->rates[i].bucket_key, bucket) == 0) return &s->rates[i];
    if (s->n_rates >= MIHFT_MAX_REC) return NULL;
    copy_text(s->rates[s->n_rates].bucket_key, sizeof(s->rates[s->n_rates].bucket_key), bucket);
    s->rates[s->n_rates].window_ts = 0;
    return &s->rates[s->n_rates++];
}

static KillRow *find_kill(Store *s, const char *scope)
{
    for (size_t i = 0; i < s->n_kills; i++) if (strcmp(s->kills[i].scope_key, scope) == 0) return &s->kills[i];
    if (s->n_kills >= MIHFT_MAX_REC) return NULL;
    copy_text(s->kills[s->n_kills].scope_key, sizeof(s->kills[s->n_kills].scope_key), scope);
    copy_text(s->kills[s->n_kills].updated_by, sizeof(s->kills[s->n_kills].updated_by), "MIHFT");
    return &s->kills[s->n_kills++];
}

static int64_t choose_price(const HotOrder *o, const MarketRow *m)
{
    if (o->ord_type == 'L') {
        return o->price_amt;
    }
    if (m == NULL) {
        return 0;
    }
    return o->side_kbn == 'B' ? m->ask_amt : m->bid_amt;
}

static int tier_margin_bp(int tier)
{
    if (tier == 1) return 1000;
    if (tier == 2) return 2000;
    if (tier == 3) return 4000;
    return 10000;
}

static int64_t book_qty(const Store *s, const HotOrder *o, int64_t px)
{
    int64_t qty = 0;
    char opp = o->side_kbn == 'B' ? 'S' : 'B';
    for (size_t i = 0; i < s->n_books; i++) {
        const BookRow *b = &s->books[i];
        if (strcmp(b->instr_code, o->instr_code) != 0 || b->side_kbn != opp) continue;
        if ((o->side_kbn == 'B' && b->price_amt <= px) || (o->side_kbn == 'S' && b->price_amt >= px)) {
            if (INT64_MAX - qty < b->book_qty) return INT64_MAX;
            qty += b->book_qty;
        }
    }
    return qty;
}

static int checked_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a < 0 || b < 0 || (a != 0 && b > INT64_MAX / a)) {
        return -1;
    }
    *out = a * b;
    return 0;
}

static int decide_one(Store *s, const HotOrder *o, DecRow *d, RejectRow *r, size_t seq)
{
    const InstRow *inst = find_inst(s, o->instr_code);
    const MarketRow *m = find_market(s, o->instr_code);
    const CustRow *cust = find_cust(s, o->cif_no);
    const LimitRow *lim;
    RateRow *rate;
    KillRow *kill;
    int64_t px, notional, margin, liquid;
    int decision = MIHFT_DEC_ACCEPT;
    char reason[32] = "OK";

    if (inst == NULL || cust == NULL) {
        decision = MIHFT_DEC_REJECT_NOTIONAL;
        copy_text(reason, sizeof(reason), "MST");
        px = o->price_amt;
        notional = 0;
    } else {
        px = choose_price(o, m);
        lim = find_limit(s, o->cif_no, inst->instr_tier);
        if (px <= 0 || o->ord_qty <= 0 || checked_mul_i64(px, o->ord_qty, &notional) != 0) {
            decision = MIHFT_DEC_REJECT_NOTIONAL;
            copy_text(reason, sizeof(reason), "AMT");
            notional = 0;
        } else if (inst->tick_amt <= 0 || (o->ord_type == 'L' && (px % inst->tick_amt) != 0)) {
            decision = MIHFT_DEC_REJECT_TICK;
            copy_text(reason, sizeof(reason), "TICK");
        } else if (notional > MIHFT_MAX_NOTIONAL || (lim != NULL && (notional > lim->max_notional_amt || o->ord_qty > lim->max_order_qty))) {
            decision = MIHFT_DEC_REJECT_NOTIONAL;
            copy_text(reason, sizeof(reason), "LIMIT");
        } else {
            margin = (notional * tier_margin_bp(inst->instr_tier) + 9999) / 10000;
            if (cust->group_limit - cust->group_used_amt - cust->acct_used_amt < margin) {
                decision = MIHFT_DEC_REJECT_MARGIN;
                copy_text(reason, sizeof(reason), "MARGIN");
            } else {
                liquid = book_qty(s, o, px);
                if ((strcmp(o->tif_code, "FOK") == 0 && liquid < o->ord_qty) ||
                    (strcmp(o->tif_code, "IOC") == 0 && liquid <= 0)) {
                    decision = MIHFT_DEC_REJECT_NOTIONAL;
                    copy_text(reason, sizeof(reason), "LIQ");
                }
            }
        }
    }

    rate = find_rate(s, o->cif_no);
    kill = find_kill(s, o->cif_no);
    if (rate == NULL || kill == NULL) {
        return -1;
    }
    if (decision == MIHFT_DEC_ACCEPT) {
        rate->order_cnt++;
        rate->notional_amt += notional;
    } else {
        rate->drop_cnt++;
    }
    if (rate->drop_cnt >= 3) {
        kill->kill_flg = 1;
        kill->updated_ts = 20250115090000LL + (int64_t)seq;
        copy_text(kill->reason_cd, sizeof(kill->reason_cd), "DROP");
    }
    if (kill->kill_flg != 0 && decision == MIHFT_DEC_ACCEPT) {
        decision = MIHFT_DEC_REJECT_NOTIONAL;
        copy_text(reason, sizeof(reason), "KILL");
        rate->drop_cnt++;
    }

    snprintf(d->decision_id, sizeof(d->decision_id), "D%08zu", seq);
    copy_text(d->order_id, sizeof(d->order_id), o->order_id);
    copy_text(d->cif_no, sizeof(d->cif_no), o->cif_no);
    copy_text(d->instr_code, sizeof(d->instr_code), o->instr_code);
    d->decision_cd = decision;
    copy_text(d->reason_cd, sizeof(d->reason_cd), reason);
    d->notional_amt = notional;
    d->limit_used_amt = cust == NULL ? 0 : cust->group_used_amt + cust->acct_used_amt;
    d->decision_ts = 20250115090000LL + (int64_t)seq;

    if (decision != MIHFT_DEC_ACCEPT) {
        snprintf(r->reject_id, sizeof(r->reject_id), "R%08zu", seq);
        copy_text(r->order_id, sizeof(r->order_id), o->order_id);
        copy_text(r->cif_no, sizeof(r->cif_no), o->cif_no);
        copy_text(r->instr_code, sizeof(r->instr_code), o->instr_code);
        snprintf(r->reject_cd, sizeof(r->reject_cd), "%d", decision);
        copy_text(r->detail_cd, sizeof(r->detail_cd), reason);
        r->reject_ts = d->decision_ts;
    }
    return decision;
}

static int write_outputs(const Store *s, const DecRow *out_dec, size_t n_dec, const RejectRow *out_rej, size_t n_rej)
{
    FILE *fd = fopen("HFDEC.out", "w");
    FILE *fr = fopen("HFRJCT.out", "w");
    FILE *ft = fopen("HFRATE.out", "w");
    FILE *fk = fopen("HFKILL.out", "w");
    if (fd == NULL || fr == NULL || ft == NULL || fk == NULL) {
        fprintf(stderr, "出力を開けません\n");
        if (fd) fclose(fd);
        if (fr) fclose(fr);
        if (ft) fclose(ft);
        if (fk) fclose(fk);
        return -1;
    }
    for (size_t i = 0; i < n_dec; i++) {
        fprintf(fd, "%s,%s,%s,%s,%d,%s,%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
                out_dec[i].decision_id, out_dec[i].order_id, out_dec[i].cif_no,
                out_dec[i].instr_code, out_dec[i].decision_cd, out_dec[i].reason_cd,
                out_dec[i].notional_amt, out_dec[i].limit_used_amt, out_dec[i].decision_ts);
    }
    for (size_t i = 0; i < n_rej; i++) {
        fprintf(fr, "%s,%s,%s,%s,%s,%s,%" PRId64 "\n",
                out_rej[i].reject_id, out_rej[i].order_id, out_rej[i].cif_no,
                out_rej[i].instr_code, out_rej[i].reject_cd, out_rej[i].detail_cd,
                out_rej[i].reject_ts);
    }
    for (size_t i = 0; i < s->n_rates; i++) {
        fprintf(ft, "%s,%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
                s->rates[i].bucket_key, s->rates[i].window_ts, s->rates[i].order_cnt,
                s->rates[i].notional_amt, s->rates[i].drop_cnt);
    }
    for (size_t i = 0; i < s->n_kills; i++) {
        fprintf(fk, "%s,%d,%s,%" PRId64 ",%s\n",
                s->kills[i].scope_key, s->kills[i].kill_flg, s->kills[i].reason_cd,
                s->kills[i].updated_ts, s->kills[i].updated_by);
    }
    fclose(fd);
    fclose(fr);
    fclose(ft);
    fclose(fk);
    return 0;
}

int main(void)
{
    Store s;
    DecRow out_dec[MIHFT_MAX_REC];
    RejectRow out_rej[MIHFT_MAX_REC];
    size_t n_dec = 0, n_rej = 0;
    int final_decision = MIHFT_DEC_ACCEPT;

    memset(&s, 0, sizeof(s));
    memset(out_dec, 0, sizeof(out_dec));
    memset(out_rej, 0, sizeof(out_rej));

    if (read_each("SCORDF", load_order, &s) || read_each("SCBOOK", load_book, &s) ||
        read_each("SCMKTD", load_market, &s) || read_each("SCEXEC", load_exec, &s) ||
        read_each("SCPOSF", load_pos, &s) || read_each("SCINSTF", load_inst, &s) ||
        read_each("SCCUST", load_cust, &s) || read_each("SCFEEF", load_fee, &s) ||
        read_each("SCCALF", load_cal, &s) || read_each("HFDEC", load_dec, &s) ||
        read_each("HFRJCT", load_reject, &s) || read_each("HFKILL", load_kill, &s) ||
        read_each("HFRATE", load_rate, &s) || read_each("SCEXPR", load_expr, &s) ||
        read_each("SCLMTF", load_limit, &s)) {
        return 64;
    }

    for (size_t i = 0; i < s.n_orders; i++) {
        int d = decide_one(&s, &s.orders[i], &out_dec[n_dec], &out_rej[n_rej], i + 1);
        if (d < 0) {
            fprintf(stderr, "判定領域不足\n");
            return 65;
        }
        if (n_dec >= MIHFT_MAX_REC) {
            fprintf(stderr, "判定件数超過\n");
            return 66;
        }
        n_dec++;
        if (d != MIHFT_DEC_ACCEPT) {
            if (n_rej >= MIHFT_MAX_REC) {
                fprintf(stderr, "拒否件数超過\n");
                return 67;
            }
            n_rej++;
            if (d > final_decision) {
                final_decision = d;
            }
        }
    }

    if (write_outputs(&s, out_dec, n_dec, out_rej, n_rej) != 0) {
        return 68;
    }
    return final_decision;
}
