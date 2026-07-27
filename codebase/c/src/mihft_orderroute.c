/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20210715  岡本 涼 (E-294)    注文ルーティング判定の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_ERR_IO       96
#define MIHFT_ERR_PARSE    97
#define MIHFT_ERR_INTERNAL 98

enum {
    IDX_ORDER_ID = 0,
    IDX_CIF_NO,
    IDX_INSTR_CODE,
    IDX_SIDE_KBN,
    IDX_ORD_TYPE,
    IDX_TIF_CODE,
    IDX_ORD_QTY,
    IDX_PRICE_AMT,
    IDX_ORDER_TIER,
    SCORDF_COLS
};

enum {
    IDX_INST_CODE = 0,
    IDX_INST_NAME,
    IDX_INST_TIER,
    IDX_TICK_AMT,
    IDX_LOT_QTY,
    IDX_BOARD_CODE,
    SCINSTF_COLS
};

enum {
    IDX_QUOTE_CODE = 0,
    IDX_BID_AMT,
    IDX_ASK_AMT,
    IDX_MID_AMT,
    IDX_SPREAD_AMT,
    IDX_QUOTE_TS,
    HFQUOTF_COLS
};

typedef struct {
    char code[32];
    int tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[8];
} inst_rec_t;

typedef struct {
    char code[32];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t mid_amt;
    int64_t spread_amt;
    char quote_ts[32];
} quote_rec_t;

typedef struct {
    inst_rec_t rows[2048];
    size_t count;
} inst_table_t;

typedef struct {
    quote_rec_t rows[4096];
    size_t count;
} quote_table_t;

static void trim_eol(char *s)
{
    size_t n = strlen(s);

    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static char *trim_space(char *s)
{
    char *end;

    while (*s == ' ' || *s == '\t') {
        ++s;
    }

    end = s + strlen(s);
    while (end > s && (end[-1] == ' ' || end[-1] == '\t')) {
        *--end = '\0';
    }

    return s;
}

static int split_csv(char *line, char **cols, size_t need)
{
    size_t n = 0U;
    char *p = line;

    while (n < need) {
        cols[n++] = trim_space(p);
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    return n == need && strchr(cols[need - 1U], ',') == NULL;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end;
    long long v;

    if (s == NULL || *s == '\0') {
        return 0;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || *end != '\0') {
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

static int same_code(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
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

static int tier_tick_amt(int tier, int64_t *tick)
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

static int mul_i64_checked(int64_t a, int64_t b, int64_t *out)
{
    if (a < 0 || b < 0) {
        return 0;
    }
    if (a != 0 && b > INT64_MAX / a) {
        return 0;
    }
    *out = a * b;
    return 1;
}

static int load_inst(inst_table_t *tab)
{
    FILE *fp = fopen("SCINSTF.csv", "r");
    char line[1024];

    if (fp == NULL) {
        fp = fopen("SCINSTF", "r");
    }
    if (fp == NULL) {
        return MIHFT_ERR_IO;
    }

    tab->count = 0U;
    while (fgets(line, sizeof line, fp) != NULL) {
        char *cols[SCINSTF_COLS];
        inst_rec_t *r;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (!split_csv(line, cols, SCINSTF_COLS)) {
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (same_code(cols[IDX_INST_CODE], "INSTR-CODE")) {
            continue;
        }
        if (tab->count >= sizeof tab->rows / sizeof tab->rows[0]) {
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        r = &tab->rows[tab->count];
        snprintf(r->code, sizeof r->code, "%s", cols[IDX_INST_CODE]);
        snprintf(r->board_code, sizeof r->board_code, "%s", cols[IDX_BOARD_CODE]);
        if (!parse_int(cols[IDX_INST_TIER], &r->tier) ||
            !parse_i64(cols[IDX_TICK_AMT], &r->tick_amt) ||
            !parse_i64(cols[IDX_LOT_QTY], &r->lot_qty) ||
            r->lot_qty <= 0) {
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        ++tab->count;
    }

    if (ferror(fp)) {
        fclose(fp);
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    return 0;
}

static int load_quote(quote_table_t *tab)
{
    FILE *fp = fopen("HFQUOTF.csv", "r");
    char line[1024];

    if (fp == NULL) {
        fp = fopen("HFQUOTF", "r");
    }
    if (fp == NULL) {
        return MIHFT_ERR_IO;
    }

    tab->count = 0U;
    while (fgets(line, sizeof line, fp) != NULL) {
        char *cols[HFQUOTF_COLS];
        quote_rec_t *r;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (!split_csv(line, cols, HFQUOTF_COLS)) {
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (same_code(cols[IDX_QUOTE_CODE], "INSTR-CODE")) {
            continue;
        }
        if (tab->count >= sizeof tab->rows / sizeof tab->rows[0]) {
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        r = &tab->rows[tab->count];
        snprintf(r->code, sizeof r->code, "%s", cols[IDX_QUOTE_CODE]);
        snprintf(r->quote_ts, sizeof r->quote_ts, "%s", cols[IDX_QUOTE_TS]);
        if (!parse_i64(cols[IDX_BID_AMT], &r->bid_amt) ||
            !parse_i64(cols[IDX_ASK_AMT], &r->ask_amt) ||
            !parse_i64(cols[IDX_MID_AMT], &r->mid_amt) ||
            !parse_i64(cols[IDX_SPREAD_AMT], &r->spread_amt)) {
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        ++tab->count;
    }

    if (ferror(fp)) {
        fclose(fp);
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    return 0;
}

static const inst_rec_t *find_inst(const inst_table_t *tab, const char *code)
{
    size_t i;

    for (i = 0U; i < tab->count; ++i) {
        if (same_code(tab->rows[i].code, code)) {
            return &tab->rows[i];
        }
    }

    return NULL;
}

static const quote_rec_t *find_quote(const quote_table_t *tab, const char *code)
{
    size_t i;

    for (i = 0U; i < tab->count; ++i) {
        if (same_code(tab->rows[i].code, code)) {
            return &tab->rows[i];
        }
    }

    return NULL;
}

static int current_ts(char *buf, size_t len)
{
    time_t now = time(NULL);
    struct tm tmv;

    if (now == (time_t)-1) {
        return 0;
    }

#if defined(_POSIX_THREAD_SAFE_FUNCTIONS)
    if (localtime_r(&now, &tmv) == NULL) {
        return 0;
    }
#else
    {
        struct tm *tmp = localtime(&now);
        if (tmp == NULL) {
            return 0;
        }
        tmv = *tmp;
    }
#endif

    return strftime(buf, len, "%Y%m%d%H%M%S", &tmv) > 0U;
}

static int price_for_order(const char *side, const char *ord_type,
                           int64_t limit_price, const quote_rec_t *quote,
                           int64_t *price)
{
    if (same_code(ord_type, "L")) {
        if (limit_price <= 0) {
            return 0;
        }
        *price = limit_price;
        return 1;
    }

    if (!same_code(ord_type, "M") || quote == NULL) {
        return 0;
    }

    if (same_code(side, "B")) {
        *price = quote->ask_amt;
        return *price > 0;
    }
    if (same_code(side, "S")) {
        *price = quote->bid_amt;
        return *price > 0;
    }

    return 0;
}

static int tick_ok(int64_t price, int tier)
{
    int64_t tick;

    if (!tier_tick_amt(tier, &tick) || tick <= 0) {
        return 0;
    }

    return price > 0 && price % tick == 0;
}

static int throttle_ok(const char *tif, const quote_rec_t *quote, int tier)
{
    int64_t spread_limit;

    if (quote == NULL || quote->bid_amt <= 0 || quote->ask_amt <= 0 ||
        quote->ask_amt < quote->bid_amt) {
        return 0;
    }

    spread_limit = (tier == 1) ? 500 : (tier == 2) ? 1500 : 3000;
    if (quote->spread_amt > spread_limit) {
        return 0;
    }

    if (same_code(tif, "FOK") && quote->spread_amt > spread_limit / 2) {
        return 0;
    }

    return same_code(tif, "DAY") || same_code(tif, "IOC") || same_code(tif, "FOK");
}

static const char *route_action(const char *ord_type, const char *tif,
                                const inst_rec_t *inst, const quote_rec_t *quote)
{
    if (quote == NULL) {
        return "HLD";
    }

    if (same_code(ord_type, "L") && same_code(tif, "DAY") &&
        same_code(inst->board_code, "T1")) {
        return "INT";
    }

    return "EXT";
}

static int decide_reason(const inst_rec_t *inst, const quote_rec_t *quote,
                         const char *side, const char *ord_type,
                         const char *tif, int64_t qty, int64_t limit_price,
                         int order_tier)
{
    int margin_bp;
    int64_t price;
    int64_t notional;
    int64_t margin;

    if (inst == NULL || inst->tier != order_tier ||
        !tier_margin_bp(order_tier, &margin_bp)) {
        return 8;
    }

    if (!price_for_order(side, ord_type, limit_price, quote, &price)) {
        return 8;
    }

    if (!tick_ok(price, order_tier)) {
        return 12;
    }

    if (qty <= 0 || qty % inst->lot_qty != 0 ||
        !mul_i64_checked(qty, price, &notional) ||
        notional > MIHFT_MAX_NOTIONAL) {
        return 8;
    }

    if (notional > INT64_MAX / margin_bp) {
        return 4;
    }
    margin = (notional * margin_bp + 9999) / 10000;
    if (margin > MIHFT_MAX_NOTIONAL / 2) {
        return 4;
    }

    if (!throttle_ok(tif, quote, order_tier)) {
        return 8;
    }

    return 0;
}

int main(void)
{
    inst_table_t inst_tab;
    quote_table_t quote_tab;
    FILE *in;
    FILE *out;
    char line[2048];
    char ts[32];
    unsigned long decision_seq = 1UL;
    int final_code = 0;
    int rc;

    rc = load_inst(&inst_tab);
    if (rc != 0) {
        return rc;
    }

    rc = load_quote(&quote_tab);
    if (rc != 0) {
        return rc;
    }

    in = fopen("SCORDF.csv", "r");
    if (in == NULL) {
        in = fopen("SCORDF", "r");
    }
    if (in == NULL) {
        return MIHFT_ERR_IO;
    }

    out = fopen("HFDECLOG.csv", "w");
    if (out == NULL) {
        fclose(in);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof line, in) != NULL) {
        char *cols[SCORDF_COLS];
        const inst_rec_t *inst;
        const quote_rec_t *quote;
        const char *action;
        int64_t qty;
        int64_t limit_price;
        int order_tier;
        int reason;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (!split_csv(line, cols, SCORDF_COLS)) {
            fclose(out);
            fclose(in);
            return MIHFT_ERR_PARSE;
        }
        if (same_code(cols[IDX_ORDER_ID], "ORDER-ID")) {
            continue;
        }

        if (!parse_i64(cols[IDX_ORD_QTY], &qty) ||
            !parse_i64(cols[IDX_PRICE_AMT], &limit_price) ||
            !parse_int(cols[IDX_ORDER_TIER], &order_tier)) {
            fclose(out);
            fclose(in);
            return MIHFT_ERR_PARSE;
        }

        inst = find_inst(&inst_tab, cols[IDX_INSTR_CODE]);
        quote = find_quote(&quote_tab, cols[IDX_INSTR_CODE]);
        reason = decide_reason(inst, quote, cols[IDX_SIDE_KBN], cols[IDX_ORD_TYPE],
                               cols[IDX_TIF_CODE], qty, limit_price, order_tier);

        if (reason != 0) {
            if (final_code == 0) {
                final_code = reason;
            }
            continue;
        }

        if (!current_ts(ts, sizeof ts)) {
            fclose(out);
            fclose(in);
            return MIHFT_ERR_INTERNAL;
        }

        action = route_action(cols[IDX_ORD_TYPE], cols[IDX_TIF_CODE], inst, quote);
        if (fprintf(out, "D%010lu,%s,%s,%s,%d,%s\n",
                    decision_seq++, cols[IDX_ORDER_ID], cols[IDX_INSTR_CODE],
                    action, reason, ts) < 0) {
            fclose(out);
            fclose(in);
            return MIHFT_ERR_IO;
        }
    }

    if (ferror(in) || fflush(out) != 0) {
        fclose(out);
        fclose(in);
        return MIHFT_ERR_IO;
    }

    if (fclose(out) != 0) {
        fclose(in);
        return MIHFT_ERR_IO;
    }
    if (fclose(in) != 0) {
        return MIHFT_ERR_IO;
    }

    return final_code;
}
