/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210128  中川 美和 (E-283)   初版作成、注文・相場・板情報の時刻順再生と判定ログ出力
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_ERR_IO      64
#define MIHFT_ERR_PARSE   65
#define MIHFT_ERR_MEMORY  66
#define MIHFT_LINE_MAX    1024
#define MIHFT_FIELD_MAX   16
#define MIHFT_PATH_ORD    "SCORDF.csv"
#define MIHFT_PATH_MKT    "SCMKTD.csv"
#define MIHFT_PATH_BOOK   "SCBOOK.csv"
#define MIHFT_PATH_LOG    "HFDECLOG.csv"

typedef struct {
    char order_id[32];
    char cif_no[24];
    char instr_code[24];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    uint64_t ord_qty;
    uint64_t price_amt;
    int instr_tier;
    uint64_t event_ts;
    size_t seq_no;
} MihftOrderRow;

typedef struct {
    char instr_code[24];
    uint64_t bid_amt;
    uint64_t ask_amt;
    uint64_t last_amt;
    uint64_t vol_qty;
    uint64_t tick_ts;
    size_t seq_no;
} MihftMarketRow;

typedef struct {
    char instr_code[24];
    char side_kbn;
    uint32_t level_cnt;
    uint64_t price_amt;
    uint64_t book_qty;
    uint32_t order_cnt;
    uint64_t entry_ts;
    size_t seq_no;
} MihftBookRow;

typedef struct {
    char instr_code[24];
    uint64_t bid_amt;
    uint64_t ask_amt;
    uint64_t last_amt;
    uint64_t vol_qty;
    uint64_t tick_ts;
    uint64_t buy_best_qty;
    uint64_t sell_best_qty;
    uint64_t buy_best_amt;
    uint64_t sell_best_amt;
    uint64_t book_ts;
    int has_market;
    int has_book;
} MihftInstrumentState;

typedef enum {
    MIHFT_EV_ORDER = 1,
    MIHFT_EV_MARKET = 2,
    MIHFT_EV_BOOK = 3
} MihftEventKind;

typedef struct {
    MihftEventKind kind;
    uint64_t ts;
    size_t index;
    size_t seq_no;
} MihftReplayEvent;

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t len = strlen(src);
    if (len + 1U > dst_len) {
        return 0;
    }
    memcpy(dst, src, len + 1U);
    return 1;
}

static char *trim_field(char *s)
{
    char *end;
    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n') {
        ++s;
    }
    end = s + strlen(s);
    while (end > s && (end[-1] == ' ' || end[-1] == '\t' || end[-1] == '\r' || end[-1] == '\n')) {
        --end;
    }
    *end = '\0';
    return s;
}

static int split_csv(char *line, char **fields, size_t expect)
{
    size_t n = 0;
    char *p = line;
    for (;;) {
        if (n >= expect) {
            return 0;
        }
        fields[n++] = trim_field(p);
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return n == expect;
}

static int parse_u64(const char *s, uint64_t *out)
{
    char *end = NULL;
    unsigned long long v;
    errno = 0;
    if (*s == '\0' || *s == '-') {
        return 0;
    }
    v = strtoull(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return 0;
    }
    *out = (uint64_t)v;
    return 1;
}

static int parse_u32(const char *s, uint32_t *out)
{
    uint64_t v;
    if (!parse_u64(s, &v) || v > UINT32_MAX) {
        return 0;
    }
    *out = (uint32_t)v;
    return 1;
}

static int parse_int_tier(const char *s, int *out)
{
    uint64_t v;
    if (!parse_u64(s, &v) || v < 1U || v > 3U) {
        return 0;
    }
    *out = (int)v;
    return 1;
}

static int parse_side(const char *s, char *out)
{
    if ((strcmp(s, "B") != 0) && (strcmp(s, "S") != 0)) {
        return 0;
    }
    *out = s[0];
    return 1;
}

static int parse_order_type(const char *s, char *out)
{
    if ((strcmp(s, "L") != 0) && (strcmp(s, "M") != 0)) {
        return 0;
    }
    *out = s[0];
    return 1;
}

static int parse_tif(const char *s, char out[4])
{
    if ((strcmp(s, "DAY") != 0) && (strcmp(s, "IOC") != 0) && (strcmp(s, "FOK") != 0)) {
        return 0;
    }
    return copy_field(out, 4U, s);
}

static int reserve_array(void **ptr, size_t elem_size, size_t *cap, size_t need)
{
    void *next;
    size_t next_cap = *cap == 0U ? 128U : *cap;
    while (next_cap < need) {
        if (next_cap > SIZE_MAX / 2U) {
            return 0;
        }
        next_cap *= 2U;
    }
    if (elem_size != 0U && next_cap > SIZE_MAX / elem_size) {
        return 0;
    }
    next = realloc(*ptr, elem_size * next_cap);
    if (next == NULL) {
        return 0;
    }
    *ptr = next;
    *cap = next_cap;
    return 1;
}

static int looks_like_order_header(char **f)
{
    return strcmp(f[0], "ORDER-ID") == 0;
}

static int looks_like_market_header(char **f)
{
    return strcmp(f[0], "INSTR-CODE") == 0 && strcmp(f[5], "TICK-TS") == 0;
}

static int looks_like_book_header(char **f)
{
    return strcmp(f[0], "INSTR-CODE") == 0 && strcmp(f[6], "ENTRY-TS") == 0;
}

static int load_orders(MihftOrderRow **rows, size_t *count)
{
    FILE *fp = fopen(MIHFT_PATH_ORD, "r");
    char line[MIHFT_LINE_MAX];
    size_t cap = 0;
    size_t n = 0;
    size_t line_no = 0;
    if (fp == NULL) {
        fprintf(stderr, "注文ファイルを開けません: %s\n", MIHFT_PATH_ORD);
        return MIHFT_ERR_IO;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        char *fields[9];
        MihftOrderRow row;
        ++line_no;
        if (!split_csv(line, fields, 9U)) {
            fprintf(stderr, "注文ファイルの項目数が不正です: %zu\n", line_no);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (line_no == 1U && looks_like_order_header(fields)) {
            continue;
        }
        memset(&row, 0, sizeof row);
        if (!copy_field(row.order_id, sizeof row.order_id, fields[0]) ||
            !copy_field(row.cif_no, sizeof row.cif_no, fields[1]) ||
            !copy_field(row.instr_code, sizeof row.instr_code, fields[2]) ||
            !parse_side(fields[3], &row.side_kbn) ||
            !parse_order_type(fields[4], &row.ord_type) ||
            !parse_tif(fields[5], row.tif_code) ||
            !parse_u64(fields[6], &row.ord_qty) ||
            !parse_u64(fields[7], &row.price_amt) ||
            !parse_int_tier(fields[8], &row.instr_tier) ||
            row.ord_qty == 0U) {
            fprintf(stderr, "注文ファイルの値が不正です: %zu\n", line_no);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        row.seq_no = n;
        row.event_ts = 1000000000ULL + (uint64_t)n * 1000ULL;
        if (!reserve_array((void **)rows, sizeof **rows, &cap, n + 1U)) {
            fclose(fp);
            return MIHFT_ERR_MEMORY;
        }
        (*rows)[n++] = row;
    }
    if (ferror(fp)) {
        fprintf(stderr, "注文ファイルの読込に失敗しました\n");
        fclose(fp);
        return MIHFT_ERR_IO;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static int load_markets(MihftMarketRow **rows, size_t *count)
{
    FILE *fp = fopen(MIHFT_PATH_MKT, "r");
    char line[MIHFT_LINE_MAX];
    size_t cap = 0;
    size_t n = 0;
    size_t line_no = 0;
    if (fp == NULL) {
        fprintf(stderr, "相場ファイルを開けません: %s\n", MIHFT_PATH_MKT);
        return MIHFT_ERR_IO;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        char *fields[6];
        MihftMarketRow row;
        ++line_no;
        if (!split_csv(line, fields, 6U)) {
            fprintf(stderr, "相場ファイルの項目数が不正です: %zu\n", line_no);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (line_no == 1U && looks_like_market_header(fields)) {
            continue;
        }
        memset(&row, 0, sizeof row);
        if (!copy_field(row.instr_code, sizeof row.instr_code, fields[0]) ||
            !parse_u64(fields[1], &row.bid_amt) ||
            !parse_u64(fields[2], &row.ask_amt) ||
            !parse_u64(fields[3], &row.last_amt) ||
            !parse_u64(fields[4], &row.vol_qty) ||
            !parse_u64(fields[5], &row.tick_ts) ||
            (row.bid_amt != 0U && row.ask_amt != 0U && row.bid_amt > row.ask_amt)) {
            fprintf(stderr, "相場ファイルの値が不正です: %zu\n", line_no);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        row.seq_no = n;
        if (!reserve_array((void **)rows, sizeof **rows, &cap, n + 1U)) {
            fclose(fp);
            return MIHFT_ERR_MEMORY;
        }
        (*rows)[n++] = row;
    }
    if (ferror(fp)) {
        fprintf(stderr, "相場ファイルの読込に失敗しました\n");
        fclose(fp);
        return MIHFT_ERR_IO;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static int load_books(MihftBookRow **rows, size_t *count)
{
    FILE *fp = fopen(MIHFT_PATH_BOOK, "r");
    char line[MIHFT_LINE_MAX];
    size_t cap = 0;
    size_t n = 0;
    size_t line_no = 0;
    if (fp == NULL) {
        fprintf(stderr, "板ファイルを開けません: %s\n", MIHFT_PATH_BOOK);
        return MIHFT_ERR_IO;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        char *fields[7];
        MihftBookRow row;
        ++line_no;
        if (!split_csv(line, fields, 7U)) {
            fprintf(stderr, "板ファイルの項目数が不正です: %zu\n", line_no);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (line_no == 1U && looks_like_book_header(fields)) {
            continue;
        }
        memset(&row, 0, sizeof row);
        if (!copy_field(row.instr_code, sizeof row.instr_code, fields[0]) ||
            !parse_side(fields[1], &row.side_kbn) ||
            !parse_u32(fields[2], &row.level_cnt) ||
            !parse_u64(fields[3], &row.price_amt) ||
            !parse_u64(fields[4], &row.book_qty) ||
            !parse_u32(fields[5], &row.order_cnt) ||
            !parse_u64(fields[6], &row.entry_ts) ||
            row.level_cnt == 0U) {
            fprintf(stderr, "板ファイルの値が不正です: %zu\n", line_no);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        row.seq_no = n;
        if (!reserve_array((void **)rows, sizeof **rows, &cap, n + 1U)) {
            fclose(fp);
            return MIHFT_ERR_MEMORY;
        }
        (*rows)[n++] = row;
    }
    if (ferror(fp)) {
        fprintf(stderr, "板ファイルの読込に失敗しました\n");
        fclose(fp);
        return MIHFT_ERR_IO;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static int add_event(MihftReplayEvent **events, size_t *count, size_t *cap,
                     MihftEventKind kind, uint64_t ts, size_t index, size_t seq_no)
{
    MihftReplayEvent ev;
    if (!reserve_array((void **)events, sizeof **events, cap, *count + 1U)) {
        return 0;
    }
    ev.kind = kind;
    ev.ts = ts;
    ev.index = index;
    ev.seq_no = seq_no;
    (*events)[(*count)++] = ev;
    return 1;
}

static int compare_event(const void *a, const void *b)
{
    const MihftReplayEvent *x = (const MihftReplayEvent *)a;
    const MihftReplayEvent *y = (const MihftReplayEvent *)b;
    if (x->ts < y->ts) {
        return -1;
    }
    if (x->ts > y->ts) {
        return 1;
    }
    if (x->kind < y->kind) {
        return -1;
    }
    if (x->kind > y->kind) {
        return 1;
    }
    return x->seq_no < y->seq_no ? -1 : x->seq_no > y->seq_no;
}

static MihftInstrumentState *find_state(MihftInstrumentState **states, size_t *count,
                                        size_t *cap, const char *instr_code)
{
    size_t i;
    for (i = 0; i < *count; ++i) {
        if (strcmp((*states)[i].instr_code, instr_code) == 0) {
            return &(*states)[i];
        }
    }
    if (!reserve_array((void **)states, sizeof **states, cap, *count + 1U)) {
        return NULL;
    }
    memset(&(*states)[*count], 0, sizeof (*states)[*count]);
    if (!copy_field((*states)[*count].instr_code, sizeof (*states)[*count].instr_code, instr_code)) {
        return NULL;
    }
    return &(*states)[(*count)++];
}

static uint64_t tier_tick(int tier)
{
    if (tier == 1) {
        return 100U;
    }
    if (tier == 2) {
        return 500U;
    }
    return 1000U;
}

static uint64_t tier_margin_bp(int tier)
{
    if (tier == 1) {
        return 1000U;
    }
    if (tier == 2) {
        return 2000U;
    }
    return 4000U;
}

static int mul_over_u64(uint64_t a, uint64_t b, uint64_t *out)
{
    if (a != 0U && b > UINT64_MAX / a) {
        return 1;
    }
    *out = a * b;
    return 0;
}

static uint64_t reference_price(const MihftOrderRow *ord, const MihftInstrumentState *st)
{
    if (ord->ord_type == 'L' && ord->price_amt != 0U) {
        return ord->price_amt;
    }
    if (st != NULL && st->has_market) {
        if (ord->side_kbn == 'B' && st->ask_amt != 0U) {
            return st->ask_amt;
        }
        if (ord->side_kbn == 'S' && st->bid_amt != 0U) {
            return st->bid_amt;
        }
        if (st->last_amt != 0U) {
            return st->last_amt;
        }
    }
    return ord->price_amt;
}

static int decide_order(const MihftOrderRow *ord, const MihftInstrumentState *st,
                        uint64_t *notional_out, uint64_t *margin_out)
{
    uint64_t px = reference_price(ord, st);
    uint64_t notional;
    uint64_t margin_product;
    if (px == 0U || mul_over_u64(ord->ord_qty, px, &notional)) {
        *notional_out = UINT64_MAX;
        *margin_out = UINT64_MAX;
        return 8;
    }
    *notional_out = notional;
    if (ord->ord_type == 'L' && (ord->price_amt % tier_tick(ord->instr_tier)) != 0U) {
        *margin_out = 0U;
        return 12;
    }
    if (notional > (uint64_t)MIHFT_MAX_NOTIONAL) {
        *margin_out = 0U;
        return 8;
    }
    if (mul_over_u64(notional, tier_margin_bp(ord->instr_tier), &margin_product)) {
        *margin_out = UINT64_MAX;
        return 4;
    }
    *margin_out = (margin_product + 9999U) / 10000U;
    if (*margin_out > 75000000ULL) {
        return 4;
    }
    return 0;
}

static const char *reason_code(int decision, const MihftOrderRow *ord, const MihftInstrumentState *st)
{
    if (decision == 12) {
        return "TICK";
    }
    if (decision == 8) {
        return "NOTIONAL";
    }
    if (decision == 4) {
        return "MARGIN";
    }
    if (ord->tif_code[0] == 'F' && (st == NULL || !st->has_book)) {
        return "NOBOOK";
    }
    return "OK";
}

static uint64_t now_ns(void)
{
    struct timespec ts;
    if (timespec_get(&ts, TIME_UTC) != TIME_UTC) {
        return 0U;
    }
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

static int write_decision(FILE *fp, uint64_t decision_id, const MihftOrderRow *ord,
                          int action_code, const char *reason, uint64_t decision_ts)
{
    if (fprintf(fp, "%" PRIu64 ",%s,%s,%d,%s,%" PRIu64 "\n",
                decision_id, ord->order_id, ord->instr_code, action_code, reason, decision_ts) < 0) {
        fprintf(stderr, "判定ログの書込に失敗しました\n");
        return 0;
    }
    return 1;
}

int main(void)
{
    MihftOrderRow *orders = NULL;
    MihftMarketRow *markets = NULL;
    MihftBookRow *books = NULL;
    MihftReplayEvent *events = NULL;
    MihftInstrumentState *states = NULL;
    size_t order_count = 0;
    size_t market_count = 0;
    size_t book_count = 0;
    size_t event_count = 0;
    size_t event_cap = 0;
    size_t state_count = 0;
    size_t state_cap = 0;
    uint64_t decision_id = 1U;
    int final_code = 0;
    int rc;
    FILE *logfp;
    size_t i;

    rc = load_orders(&orders, &order_count);
    if (rc != 0) {
        free(orders);
        return rc;
    }
    rc = load_markets(&markets, &market_count);
    if (rc != 0) {
        free(orders);
        free(markets);
        return rc;
    }
    rc = load_books(&books, &book_count);
    if (rc != 0) {
        free(orders);
        free(markets);
        free(books);
        return rc;
    }

    for (i = 0; i < order_count; ++i) {
        if (!add_event(&events, &event_count, &event_cap, MIHFT_EV_ORDER, orders[i].event_ts, i, orders[i].seq_no)) {
            free(orders);
            free(markets);
            free(books);
            free(events);
            return MIHFT_ERR_MEMORY;
        }
    }
    for (i = 0; i < market_count; ++i) {
        if (!add_event(&events, &event_count, &event_cap, MIHFT_EV_MARKET, markets[i].tick_ts, i, markets[i].seq_no)) {
            free(orders);
            free(markets);
            free(books);
            free(events);
            return MIHFT_ERR_MEMORY;
        }
    }
    for (i = 0; i < book_count; ++i) {
        if (!add_event(&events, &event_count, &event_cap, MIHFT_EV_BOOK, books[i].entry_ts, i, books[i].seq_no)) {
            free(orders);
            free(markets);
            free(books);
            free(events);
            return MIHFT_ERR_MEMORY;
        }
    }

    qsort(events, event_count, sizeof events[0], compare_event);

    logfp = fopen(MIHFT_PATH_LOG, "w");
    if (logfp == NULL) {
        fprintf(stderr, "判定ログを開けません: %s\n", MIHFT_PATH_LOG);
        free(orders);
        free(markets);
        free(books);
        free(events);
        return MIHFT_ERR_IO;
    }
    if (fprintf(logfp, "DECISION-ID,ORDER-ID,INSTR-CODE,ACTION-CODE,REASON-CODE,DECISION-TS\n") < 0) {
        fprintf(stderr, "判定ログの見出し書込に失敗しました\n");
        fclose(logfp);
        free(orders);
        free(markets);
        free(books);
        free(events);
        return MIHFT_ERR_IO;
    }

    for (i = 0; i < event_count; ++i) {
        MihftInstrumentState *st;
        if (events[i].kind == MIHFT_EV_MARKET) {
            const MihftMarketRow *m = &markets[events[i].index];
            st = find_state(&states, &state_count, &state_cap, m->instr_code);
            if (st == NULL) {
                fclose(logfp);
                free(orders);
                free(markets);
                free(books);
                free(events);
                free(states);
                return MIHFT_ERR_MEMORY;
            }
            st->bid_amt = m->bid_amt;
            st->ask_amt = m->ask_amt;
            st->last_amt = m->last_amt;
            st->vol_qty = m->vol_qty;
            st->tick_ts = m->tick_ts;
            st->has_market = 1;
        } else if (events[i].kind == MIHFT_EV_BOOK) {
            const MihftBookRow *b = &books[events[i].index];
            st = find_state(&states, &state_count, &state_cap, b->instr_code);
            if (st == NULL) {
                fclose(logfp);
                free(orders);
                free(markets);
                free(books);
                free(events);
                free(states);
                return MIHFT_ERR_MEMORY;
            }
            if (b->side_kbn == 'B') {
                st->buy_best_amt = b->price_amt;
                st->buy_best_qty = b->book_qty;
            } else {
                st->sell_best_amt = b->price_amt;
                st->sell_best_qty = b->book_qty;
            }
            st->book_ts = b->entry_ts;
            st->has_book = 1;
        } else {
            const MihftOrderRow *ord = &orders[events[i].index];
            uint64_t notional = 0U;
            uint64_t margin = 0U;
            int decision;
            uint64_t t0 = now_ns();
            uint64_t t1;
            st = find_state(&states, &state_count, &state_cap, ord->instr_code);
            if (st == NULL) {
                fclose(logfp);
                free(orders);
                free(markets);
                free(books);
                free(events);
                free(states);
                return MIHFT_ERR_MEMORY;
            }
            decision = decide_order(ord, st, &notional, &margin);
            t1 = now_ns();
            if (t1 == 0U) {
                t1 = events[i].ts;
            }
            if (!write_decision(logfp, decision_id++, ord, decision, reason_code(decision, ord, st), t1)) {
                fclose(logfp);
                free(orders);
                free(markets);
                free(books);
                free(events);
                free(states);
                return MIHFT_ERR_IO;
            }
            if (decision > final_code) {
                final_code = decision;
            }
            (void)notional;
            (void)margin;
            (void)t0;
        }
    }

    if (fclose(logfp) != 0) {
        fprintf(stderr, "判定ログの終了処理に失敗しました\n");
        free(orders);
        free(markets);
        free(books);
        free(events);
        free(states);
        return MIHFT_ERR_IO;
    }

    free(orders);
    free(markets);
    free(books);
    free(events);
    free(states);
    return final_code;
}
