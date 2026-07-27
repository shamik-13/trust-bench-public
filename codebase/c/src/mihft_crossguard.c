/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20200902  中川 美和 (E-283)  初版作成：クロス板検知および単回補修判定
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_ERR_IO 20
#define MIHFT_ERR_PARSE 24
#define MIHFT_ERR_RANGE 28

#define MIHFT_SIDE_BUY 'B'
#define MIHFT_SIDE_SELL 'S'

#define MIHFT_ALERT_CROSSED "CROSS"
#define MIHFT_ALERT_SEVERITY "H"
#define MIHFT_ALERT_KIND "XBD"

struct mihft_best_book {
    char instr_code[32];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t bid_qty;
    int64_t ask_qty;
    int bid_orders;
    int ask_orders;
    int64_t bid_ts;
    int64_t ask_ts;
    int has_bid;
    int has_ask;
};

struct mihft_market_tick {
    char instr_code[32];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    int64_t tick_ts;
};

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_next_field(char **cursor, char *out, size_t out_sz)
{
    char *p;
    char *q;
    size_t len;

    if (cursor == NULL || *cursor == NULL || out == NULL || out_sz == 0U) {
        return -1;
    }

    p = *cursor;
    q = strchr(p, ',');
    if (q != NULL) {
        len = (size_t)(q - p);
        *cursor = q + 1;
    } else {
        len = strlen(p);
        *cursor = p + len;
    }

    if (len >= out_sz) {
        return -1;
    }

    memcpy(out, p, len);
    out[len] = '\0';
    return 0;
}

static int mihft_parse_i64(const char *s, int64_t *v)
{
    char *end;
    long long tmp;

    if (s == NULL || *s == '\0' || v == NULL) {
        return -1;
    }

    errno = 0;
    tmp = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }

    *v = (int64_t)tmp;
    return 0;
}

static int mihft_parse_int(const char *s, int *v)
{
    int64_t tmp;

    if (mihft_parse_i64(s, &tmp) != 0 || tmp < INT_MIN || tmp > INT_MAX) {
        return -1;
    }

    *v = (int)tmp;
    return 0;
}

static int mihft_same_instr(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

static int mihft_abs_i64(int64_t x, int64_t *out)
{
    if (x == INT64_MIN || out == NULL) {
        return -1;
    }

    *out = x < 0 ? -x : x;
    return 0;
}

static int mihft_notional_ok(int64_t price_amt, int64_t qty)
{
    if (price_amt < 0 || qty < 0) {
        return 0;
    }

    if (qty != 0 && price_amt > INT64_MAX / qty) {
        return 0;
    }

    return price_amt * qty <= (int64_t)MIHFT_MAX_NOTIONAL;
}

static int mihft_update_best(struct mihft_best_book *best,
                             const char *instr,
                             char side,
                             int level_cnt,
                             int64_t price_amt,
                             int64_t book_qty,
                             int order_cnt,
                             int64_t entry_ts)
{
    if (best == NULL || instr == NULL || instr[0] == '\0') {
        return -1;
    }

    if (level_cnt <= 0 || price_amt <= 0 || book_qty < 0 || order_cnt < 0 || entry_ts <= 0) {
        return -1;
    }

    if (!mihft_notional_ok(price_amt, book_qty)) {
        return -1;
    }

    if (best->instr_code[0] == '\0') {
        if (strlen(instr) >= sizeof(best->instr_code)) {
            return -1;
        }
        strcpy(best->instr_code, instr);
    } else if (!mihft_same_instr(best->instr_code, instr)) {
        return 1;
    }

    if (side == MIHFT_SIDE_BUY) {
        if (!best->has_bid || price_amt > best->bid_amt ||
            (price_amt == best->bid_amt && entry_ts > best->bid_ts)) {
            best->bid_amt = price_amt;
            best->bid_qty = book_qty;
            best->bid_orders = order_cnt;
            best->bid_ts = entry_ts;
            best->has_bid = 1;
        }
        return 0;
    }

    if (side == MIHFT_SIDE_SELL) {
        if (!best->has_ask || price_amt < best->ask_amt ||
            (price_amt == best->ask_amt && entry_ts > best->ask_ts)) {
            best->ask_amt = price_amt;
            best->ask_qty = book_qty;
            best->ask_orders = order_cnt;
            best->ask_ts = entry_ts;
            best->has_ask = 1;
        }
        return 0;
    }

    return -1;
}

static int mihft_read_scbook(const char *path, struct mihft_best_book *best)
{
    FILE *fp;
    char line[512];
    int line_no = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "SCBOOKを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cur;
        char instr[32];
        char side_s[8];
        char level_s[32];
        char price_s[32];
        char qty_s[32];
        char orders_s[32];
        char ts_s[32];
        int level_cnt;
        int order_cnt;
        int64_t price_amt;
        int64_t book_qty;
        int64_t entry_ts;
        int rc;

        line_no++;
        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (line_no == 1 && strstr(line, "INSTR-CODE") != NULL) {
            continue;
        }

        cur = line;
        if (mihft_next_field(&cur, instr, sizeof(instr)) != 0 ||
            mihft_next_field(&cur, side_s, sizeof(side_s)) != 0 ||
            mihft_next_field(&cur, level_s, sizeof(level_s)) != 0 ||
            mihft_next_field(&cur, price_s, sizeof(price_s)) != 0 ||
            mihft_next_field(&cur, qty_s, sizeof(qty_s)) != 0 ||
            mihft_next_field(&cur, orders_s, sizeof(orders_s)) != 0 ||
            mihft_next_field(&cur, ts_s, sizeof(ts_s)) != 0) {
            fprintf(stderr, "SCBOOK項目数不正: %d\n", line_no);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        if (side_s[1] != '\0' ||
            mihft_parse_int(level_s, &level_cnt) != 0 ||
            mihft_parse_i64(price_s, &price_amt) != 0 ||
            mihft_parse_i64(qty_s, &book_qty) != 0 ||
            mihft_parse_int(orders_s, &order_cnt) != 0 ||
            mihft_parse_i64(ts_s, &entry_ts) != 0) {
            fprintf(stderr, "SCBOOK値不正: %d\n", line_no);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        rc = mihft_update_best(best, instr, side_s[0], level_cnt, price_amt,
                               book_qty, order_cnt, entry_ts);
        if (rc < 0) {
            fprintf(stderr, "SCBOOK範囲不正: %d\n", line_no);
            fclose(fp);
            return MIHFT_ERR_RANGE;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCBOOK読込失敗\n");
        fclose(fp);
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    return 0;
}

static int mihft_read_scmktd(const char *path,
                             const char *target_instr,
                             struct mihft_market_tick *tick,
                             int *found)
{
    FILE *fp;
    char line[512];
    int line_no = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "SCMKTDを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    *found = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cur;
        char instr[32];
        char bid_s[32];
        char ask_s[32];
        char last_s[32];
        char vol_s[32];
        char ts_s[32];

        line_no++;
        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (line_no == 1 && strstr(line, "INSTR-CODE") != NULL) {
            continue;
        }

        cur = line;
        if (mihft_next_field(&cur, instr, sizeof(instr)) != 0 ||
            mihft_next_field(&cur, bid_s, sizeof(bid_s)) != 0 ||
            mihft_next_field(&cur, ask_s, sizeof(ask_s)) != 0 ||
            mihft_next_field(&cur, last_s, sizeof(last_s)) != 0 ||
            mihft_next_field(&cur, vol_s, sizeof(vol_s)) != 0 ||
            mihft_next_field(&cur, ts_s, sizeof(ts_s)) != 0) {
            fprintf(stderr, "SCMKTD項目数不正: %d\n", line_no);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        if (!mihft_same_instr(instr, target_instr)) {
            continue;
        }

        if (strlen(instr) >= sizeof(tick->instr_code) ||
            mihft_parse_i64(bid_s, &tick->bid_amt) != 0 ||
            mihft_parse_i64(ask_s, &tick->ask_amt) != 0 ||
            mihft_parse_i64(last_s, &tick->last_amt) != 0 ||
            mihft_parse_i64(vol_s, &tick->vol_qty) != 0 ||
            mihft_parse_i64(ts_s, &tick->tick_ts) != 0 ||
            tick->bid_amt <= 0 || tick->ask_amt <= 0 ||
            tick->last_amt < 0 || tick->vol_qty < 0 || tick->tick_ts <= 0) {
            fprintf(stderr, "SCMKTD値不正: %d\n", line_no);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        strcpy(tick->instr_code, instr);
        *found = 1;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCMKTD読込失敗\n");
        fclose(fp);
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    return 0;
}

static int mihft_repair_once(struct mihft_best_book *best,
                             const struct mihft_market_tick *tick,
                             int tick_found)
{
    int64_t bid_gap;
    int64_t ask_gap;

    if (best == NULL || !best->has_bid || !best->has_ask || best->bid_amt < best->ask_amt) {
        return 0;
    }

    if (!tick_found || tick == NULL || tick->bid_amt < tick->ask_amt) {
        return 0;
    }

    if (mihft_abs_i64(best->bid_amt - tick->bid_amt, &bid_gap) != 0 ||
        mihft_abs_i64(best->ask_amt - tick->ask_amt, &ask_gap) != 0) {
        return -1;
    }

    if (bid_gap <= ask_gap && best->bid_ts <= tick->tick_ts) {
        best->bid_amt = tick->bid_amt;
        best->bid_ts = tick->tick_ts;
    } else if (best->ask_ts <= tick->tick_ts) {
        best->ask_amt = tick->ask_amt;
        best->ask_ts = tick->tick_ts;
    }

    return 0;
}

static int mihft_write_schalt(const char *path,
                              const struct mihft_best_book *best,
                              int64_t observed_amt,
                              int64_t limit_amt)
{
    FILE *fp;
    int64_t event_ts;

    fp = fopen(path, "a");
    if (fp == NULL) {
        fprintf(stderr, "SCHALTを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    event_ts = best->bid_ts > best->ask_ts ? best->bid_ts : best->ask_ts;
    if (event_ts <= 0) {
        event_ts = (int64_t)time(NULL);
    }

    if (fprintf(fp, "CG%lld,%s,%s,%s,%lld,%lld,%lld\n",
                (long long)event_ts,
                best->instr_code,
                MIHFT_ALERT_KIND,
                MIHFT_ALERT_SEVERITY,
                (long long)observed_amt,
                (long long)limit_amt,
                (long long)event_ts) < 0) {
        fprintf(stderr, "SCHALT書込失敗\n");
        fclose(fp);
        return MIHFT_ERR_IO;
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCHALT終了処理失敗\n");
        return MIHFT_ERR_IO;
    }

    return 0;
}

int main(void)
{
    const char *scbook_path = getenv("SCBOOK");
    const char *scmktd_path = getenv("SCMKTD");
    const char *schalt_path = getenv("SCHALT");
    struct mihft_best_book best;
    struct mihft_market_tick tick;
    int tick_found = 0;
    int rc;

    if (scbook_path == NULL || scbook_path[0] == '\0') {
        scbook_path = "SCBOOK.csv";
    }
    if (scmktd_path == NULL || scmktd_path[0] == '\0') {
        scmktd_path = "SCMKTD.csv";
    }
    if (schalt_path == NULL || schalt_path[0] == '\0') {
        schalt_path = "SCHALT.dat";
    }

    memset(&best, 0, sizeof(best));
    memset(&tick, 0, sizeof(tick));

    rc = mihft_read_scbook(scbook_path, &best);
    if (rc != 0) {
        return rc;
    }

    if (!best.has_bid || !best.has_ask) {
        return 0;
    }

    rc = mihft_read_scmktd(scmktd_path, best.instr_code, &tick, &tick_found);
    if (rc != 0) {
        return rc;
    }

    if (best.bid_amt >= best.ask_amt) {
        rc = mihft_repair_once(&best, &tick, tick_found);
        if (rc != 0) {
            return MIHFT_ERR_RANGE;
        }
    }

    if (best.bid_amt >= best.ask_amt) {
        int64_t observed_amt = best.bid_amt - best.ask_amt;
        rc = mihft_write_schalt(schalt_path, &best, observed_amt, 0);
        if (rc != 0) {
            return rc;
        }
        return 0;
    }

    return 0;
}
