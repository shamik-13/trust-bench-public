/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20231114  藤田 和也 (E-271)      初版作成
 * 1.01  20240414  三宅 拓也 (E-241)      ENTRY-TS/TICK-TS鮮度判定と価格スケール統一を追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_RC_ACCEPT 0
#define MIHFT_RC_IOERR  20
#define MIHFT_RC_PARSE  24

#define MIHFT_MAX_LINE 512
#define MIHFT_MAX_INSTR 4096
#define MIHFT_TS_LEN 32

struct quote_cache {
    char instr_code[32];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t mid_amt;
    int64_t spread_amt;
    char quote_ts[MIHFT_TS_LEN];
    int has_bid;
    int has_ask;
    int has_tick;
};

static int chomp(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
    return (int)n;
}

static int split_csv(char *line, char **cols, int max_cols)
{
    int n = 0;
    char *p = line;

    while (n < max_cols) {
        cols[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return n;
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

static int parse_i32(const char *s, int *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int ts_newer(const char *lhs, const char *rhs)
{
    if (rhs[0] == '\0') {
        return 1;
    }
    return strcmp(lhs, rhs) > 0;
}

static struct quote_cache *find_or_add(struct quote_cache *cache, size_t *used, const char *instr_code)
{
    size_t i;

    for (i = 0; i < *used; i++) {
        if (strcmp(cache[i].instr_code, instr_code) == 0) {
            return &cache[i];
        }
    }

    if (*used >= MIHFT_MAX_INSTR || strlen(instr_code) >= sizeof(cache[0].instr_code)) {
        return NULL;
    }

    memset(&cache[*used], 0, sizeof(cache[*used]));
    strcpy(cache[*used].instr_code, instr_code);
    (*used)++;
    return &cache[*used - 1];
}

static int update_book(struct quote_cache *cache, size_t *used, char **cols)
{
    struct quote_cache *q;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;

    if (parse_i32(cols[2], &level_cnt) != 0 ||
        parse_i64(cols[3], &price_amt) != 0 ||
        parse_i64(cols[4], &book_qty) != 0 ||
        parse_i64(cols[5], &order_cnt) != 0) {
        return -1;
    }

    if (cols[0][0] == '\0' || cols[1][0] == '\0' || cols[6][0] == '\0') {
        return -1;
    }

    if (level_cnt <= 0 || price_amt <= 0 || book_qty <= 0 || order_cnt <= 0) {
        return 0;
    }

    q = find_or_add(cache, used, cols[0]);
    if (q == NULL) {
        return -1;
    }

    if (!ts_newer(cols[6], q->quote_ts)) {
        return 0;
    }

    if (cols[1][0] == 'B' && cols[1][1] == '\0') {
        if (!q->has_bid || price_amt > q->bid_amt) {
            q->bid_amt = price_amt;
            q->has_bid = 1;
            strncpy(q->quote_ts, cols[6], sizeof(q->quote_ts) - 1);
            q->quote_ts[sizeof(q->quote_ts) - 1] = '\0';
        }
    } else if (cols[1][0] == 'S' && cols[1][1] == '\0') {
        if (!q->has_ask || price_amt < q->ask_amt) {
            q->ask_amt = price_amt;
            q->has_ask = 1;
            strncpy(q->quote_ts, cols[6], sizeof(q->quote_ts) - 1);
            q->quote_ts[sizeof(q->quote_ts) - 1] = '\0';
        }
    } else {
        return -1;
    }

    return 0;
}

static int update_tick(struct quote_cache *cache, size_t *used, char **cols)
{
    struct quote_cache *q;
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;

    if (parse_i64(cols[1], &bid_amt) != 0 ||
        parse_i64(cols[2], &ask_amt) != 0 ||
        parse_i64(cols[3], &last_amt) != 0 ||
        parse_i64(cols[4], &vol_qty) != 0) {
        return -1;
    }

    if (cols[0][0] == '\0' || cols[5][0] == '\0') {
        return -1;
    }

    if (bid_amt <= 0 || ask_amt <= 0 || last_amt <= 0 || vol_qty < 0) {
        return 0;
    }

    q = find_or_add(cache, used, cols[0]);
    if (q == NULL) {
        return -1;
    }

    if (!ts_newer(cols[5], q->quote_ts)) {
        return 0;
    }

    q->bid_amt = bid_amt;
    q->ask_amt = ask_amt;
    q->last_amt = last_amt;
    q->has_bid = 1;
    q->has_ask = 1;
    q->has_tick = 1;
    strncpy(q->quote_ts, cols[5], sizeof(q->quote_ts) - 1);
    q->quote_ts[sizeof(q->quote_ts) - 1] = '\0';

    return 0;
}

static int load_book(struct quote_cache *cache, size_t *used)
{
    FILE *fp = fopen("SCBOOK.csv", "r");
    char line[MIHFT_MAX_LINE];
    int row = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCBOOKオープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[7];

        row++;
        chomp(line);
        if (row == 1 && strstr(line, "INSTR-CODE") != NULL) {
            continue;
        }
        if (split_csv(line, cols, 7) != 7 || update_book(cache, used, cols) != 0) {
            fprintf(stderr, "SCBOOK解析失敗:%d\n", row);
            fclose(fp);
            return -1;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCBOOK読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int load_tick(struct quote_cache *cache, size_t *used)
{
    FILE *fp = fopen("SCMKTD.csv", "r");
    char line[MIHFT_MAX_LINE];
    int row = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCMKTDオープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[6];

        row++;
        chomp(line);
        if (row == 1 && strstr(line, "INSTR-CODE") != NULL) {
            continue;
        }
        if (split_csv(line, cols, 6) != 6 || update_tick(cache, used, cols) != 0) {
            fprintf(stderr, "SCMKTD解析失敗:%d\n", row);
            fclose(fp);
            return -1;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCMKTD読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int calc_quote(struct quote_cache *q)
{
    int64_t spread;

    if (!q->has_bid || !q->has_ask || q->bid_amt <= 0 || q->ask_amt <= 0) {
        return 0;
    }
    if (q->ask_amt < q->bid_amt) {
        return 0;
    }

    spread = q->ask_amt - q->bid_amt;
    if (q->bid_amt > MIHFT_MAX_NOTIONAL || q->ask_amt > MIHFT_MAX_NOTIONAL) {
        return 0;
    }

    q->spread_amt = spread;
    q->mid_amt = q->bid_amt + (spread / 2);
    return 1;
}

static int write_quote(const struct quote_cache *cache, size_t used)
{
    FILE *fp = fopen("HFQUOTF.csv", "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "HFQUOTFオープン失敗\n");
        return -1;
    }

    if (fprintf(fp, "INSTR-CODE,BID-AMT,ASK-AMT,MID-AMT,SPREAD-AMT,QUOTE-TS\n") < 0) {
        fprintf(stderr, "HFQUOTF書込失敗\n");
        fclose(fp);
        return -1;
    }

    for (i = 0; i < used; i++) {
        const struct quote_cache *q = &cache[i];

        if (q->mid_amt <= 0) {
            continue;
        }

        if (fprintf(fp, "%s,%lld,%lld,%lld,%lld,%s\n",
                    q->instr_code,
                    (long long)q->bid_amt,
                    (long long)q->ask_amt,
                    (long long)q->mid_amt,
                    (long long)q->spread_amt,
                    q->quote_ts) < 0) {
            fprintf(stderr, "HFQUOTF書込失敗\n");
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "HFQUOTFクローズ失敗\n");
        return -1;
    }

    return 0;
}

int main(void)
{
    static struct quote_cache cache[MIHFT_MAX_INSTR];
    size_t used = 0;
    size_t i;

    if (load_book(cache, &used) != 0) {
        return MIHFT_RC_PARSE;
    }

    if (load_tick(cache, &used) != 0) {
        return MIHFT_RC_PARSE;
    }

    for (i = 0; i < used; i++) {
        (void)calc_quote(&cache[i]);
    }

    if (write_quote(cache, used) != 0) {
        return MIHFT_RC_IOERR;
    }

    return MIHFT_RC_ACCEPT;
}
