/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20200310  岡本 涼 (E-294)  初版作成
 * 1.01  20200810  西村 亮 (E-204)  時刻逆行除外と桁あふれ検査を追加
 * 1.02  20210110  渡辺 隆 (E-260)  スプレッド検証と転送判定を調整
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MIHFT_MAX_LINE
#define MIHFT_MAX_LINE 512
#endif

#ifndef MIHFT_MAX_SYMBOLS
#define MIHFT_MAX_SYMBOLS 4096
#endif

#ifndef MIHFT_MAX_CODE_LEN
#define MIHFT_MAX_CODE_LEN 32
#endif

#ifndef MIHFT_MAX_SPREAD_AMT
#define MIHFT_MAX_SPREAD_AMT 500000LL
#endif

#ifndef MIHFT_MIN_TRADE_QTY
#define MIHFT_MIN_TRADE_QTY 1LL
#endif

#ifndef MIHFT_DECISION_FORWARD
#ifdef MIHFT_DECISION_CACHE_FORWARD
#define MIHFT_DECISION_FORWARD MIHFT_DECISION_CACHE_FORWARD
#elif defined(MIHFT_DECISION_ACCEPT)
#define MIHFT_DECISION_FORWARD MIHFT_DECISION_ACCEPT
#elif defined(MIHFT_DECISION_OK)
#define MIHFT_DECISION_FORWARD MIHFT_DECISION_OK
#elif defined(MIHFT_RC_FORWARD)
#define MIHFT_DECISION_FORWARD MIHFT_RC_FORWARD
#else
#define MIHFT_DECISION_FORWARD 0
#endif
#endif

#ifndef MIHFT_DECISION_STALE
#ifdef MIHFT_DECISION_DEDUP_STALE
#define MIHFT_DECISION_STALE MIHFT_DECISION_DEDUP_STALE
#elif defined(MIHFT_DECISION_DROP)
#define MIHFT_DECISION_STALE MIHFT_DECISION_DROP
#elif defined(MIHFT_RC_STALE)
#define MIHFT_DECISION_STALE MIHFT_RC_STALE
#else
#define MIHFT_DECISION_STALE MIHFT_DECISION_FORWARD
#endif
#endif

#ifndef MIHFT_DECISION_REJECT
#ifdef MIHFT_DECISION_INVALID
#define MIHFT_DECISION_REJECT MIHFT_DECISION_INVALID
#elif defined(MIHFT_DECISION_PARSE_REJECT)
#define MIHFT_DECISION_REJECT MIHFT_DECISION_PARSE_REJECT
#elif defined(MIHFT_RC_REJECT)
#define MIHFT_DECISION_REJECT MIHFT_RC_REJECT
#else
#define MIHFT_DECISION_REJECT MIHFT_DECISION_FORWARD
#endif
#endif

enum {
    ERR_IO = 10,
    ERR_PARSE = 11,
    ERR_OVERFLOW = 12
};

typedef struct {
    char code[MIHFT_MAX_CODE_LEN];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    int64_t tick_ts;
} parsed_tick_t;

typedef struct {
    char code[MIHFT_MAX_CODE_LEN];
    int64_t last_ts;
    int64_t last_amt;
    int64_t last_vol;
} symbol_state_t;

static int trim_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
    return n > 0U;
}

static int split_csv(char *line, char **field, size_t cap)
{
    size_t n = 0U;
    char *p = line;

    while (n < cap) {
        field[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return n == cap && strchr(field[cap - 1U], ',') == NULL;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
        return 0;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno == ERANGE || end == s || *end != '\0') {
        return 0;
    }

    *out = (int64_t)v;
    return 1;
}

static int parse_tick(char *line, parsed_tick_t *tick)
{
    char *f[6];

    if (!trim_eol(line)) {
        return 0;
    }
    if (!split_csv(line, f, 6U)) {
        return 0;
    }
    if (f[0][0] == '\0' || strlen(f[0]) >= sizeof(tick->code)) {
        return 0;
    }

    memcpy(tick->code, f[0], strlen(f[0]) + 1U);

    if (!parse_i64(f[1], &tick->bid_amt) ||
        !parse_i64(f[2], &tick->ask_amt) ||
        !parse_i64(f[3], &tick->last_amt) ||
        !parse_i64(f[4], &tick->vol_qty) ||
        !parse_i64(f[5], &tick->tick_ts)) {
        return 0;
    }

    return 1;
}

static int is_header(const char *line)
{
    return strncmp(line, "INSTR-CODE,", 11U) == 0;
}

static int validate_tick(const parsed_tick_t *tick)
{
    int64_t spread;

    if (tick->bid_amt <= 0 || tick->ask_amt <= 0 || tick->last_amt <= 0) {
        return 0;
    }
    if (tick->vol_qty < MIHFT_MIN_TRADE_QTY || tick->tick_ts <= 0) {
        return 0;
    }
    if (tick->ask_amt < tick->bid_amt) {
        return 0;
    }

    spread = tick->ask_amt - tick->bid_amt;
    if (spread > MIHFT_MAX_SPREAD_AMT) {
        return 0;
    }
    if (tick->last_amt < tick->bid_amt || tick->last_amt > tick->ask_amt) {
        return 0;
    }

    return 1;
}

static symbol_state_t *find_state(symbol_state_t *states, size_t *used, const char *code)
{
    size_t i;

    for (i = 0U; i < *used; i++) {
        if (strcmp(states[i].code, code) == 0) {
            return &states[i];
        }
    }

    if (*used >= MIHFT_MAX_SYMBOLS) {
        return NULL;
    }

    memset(&states[*used], 0, sizeof(states[*used]));
    memcpy(states[*used].code, code, strlen(code) + 1U);
    return &states[(*used)++];
}

static int is_stale_or_dup(symbol_state_t *state, const parsed_tick_t *tick)
{
    if (tick->tick_ts < state->last_ts) {
        return 1;
    }
    if (tick->tick_ts == state->last_ts &&
        tick->last_amt == state->last_amt &&
        tick->vol_qty <= state->last_vol) {
        return 1;
    }
    return 0;
}

static int ingest_tick(symbol_state_t *states, size_t *used, const parsed_tick_t *tick)
{
    symbol_state_t *state = find_state(states, used, tick->code);

    if (state == NULL) {
        return ERR_OVERFLOW;
    }
    if (!validate_tick(tick)) {
        return MIHFT_DECISION_REJECT;
    }
    if (is_stale_or_dup(state, tick)) {
        return MIHFT_DECISION_STALE;
    }

    state->last_ts = tick->tick_ts;
    state->last_amt = tick->last_amt;
    state->last_vol = tick->vol_qty;

    return MIHFT_DECISION_FORWARD;
}

int main(void)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];
    symbol_state_t states[MIHFT_MAX_SYMBOLS];
    size_t used = 0U;
    unsigned long lineno = 0UL;
    long forwarded = 0L;
    long stale = 0L;
    long rejected = 0L;
    int last_decision = MIHFT_DECISION_STALE;

    memset(states, 0, sizeof(states));

    fp = fopen("SCMKTD.csv", "r");
    if (fp == NULL) {
        fprintf(stderr, "E001:入力ファイルを開けません\n");
        return ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        parsed_tick_t tick;
        int decision;

        lineno++;
        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fprintf(stderr, "E002:%lu:行長超過\n", lineno);
            fclose(fp);
            return ERR_PARSE;
        }
        if (lineno == 1UL && is_header(line)) {
            continue;
        }

        memset(&tick, 0, sizeof(tick));
        if (!parse_tick(line, &tick)) {
            fprintf(stderr, "E003:%lu:形式不正\n", lineno);
            fclose(fp);
            return ERR_PARSE;
        }

        decision = ingest_tick(states, &used, &tick);
        if (decision == ERR_OVERFLOW) {
            fprintf(stderr, "E004:%lu:銘柄数超過\n", lineno);
            fclose(fp);
            return ERR_OVERFLOW;
        }

        last_decision = decision;
        if (decision == MIHFT_DECISION_FORWARD) {
            forwarded++;
        } else if (decision == MIHFT_DECISION_STALE) {
            stale++;
        } else {
            rejected++;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E005:入力読取失敗\n");
        fclose(fp);
        return ERR_IO;
    }
    if (fclose(fp) != 0) {
        fprintf(stderr, "E006:入力終了失敗\n");
        return ERR_IO;
    }

    if (forwarded > 0L) {
        return MIHFT_DECISION_FORWARD;
    }
    if (rejected > 0L && stale == 0L) {
        return MIHFT_DECISION_REJECT;
    }
    return last_decision;
}
