/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    20200310    市場基盤部  初版作成、SCMKTD鮮度判定の事前判定版
 * 1.01    20200810    市場基盤部  CSV境界検査と時刻差分の桁あふれ確認を追加
 * 1.02    20210110    市場基盤部  銘柄別stale/crossed/locked判定の出力を整理
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef MIHFT_MD_STALE_NS
#define MIHFT_LOCAL_STALE_NS 5000000LL
#else
#define MIHFT_LOCAL_STALE_NS MIHFT_MD_STALE_NS
#endif

#ifndef MIHFT_DECISION_OK
#define MIHFT_LOCAL_DECISION_OK 0
#else
#define MIHFT_LOCAL_DECISION_OK MIHFT_DECISION_OK
#endif

#ifndef MIHFT_DECISION_IO_ERROR
#define MIHFT_LOCAL_DECISION_IO_ERROR 2
#else
#define MIHFT_LOCAL_DECISION_IO_ERROR MIHFT_DECISION_IO_ERROR
#endif

#ifndef MIHFT_DECISION_PARSE_ERROR
#define MIHFT_LOCAL_DECISION_PARSE_ERROR 3
#else
#define MIHFT_LOCAL_DECISION_PARSE_ERROR MIHFT_DECISION_PARSE_ERROR
#endif

enum mihft_local_state {
    MIHFT_LOCAL_STATE_FRESH = 0,
    MIHFT_LOCAL_STATE_STALE = 1,
    MIHFT_LOCAL_STATE_CROSSED = 2,
    MIHFT_LOCAL_STATE_LOCKED = 3
};

struct mihft_local_record {
    char instr_code[32];
    long long bid_amt;
    long long ask_amt;
    long long last_amt;
    long long vol_qty;
    long long tick_ts;
};

static void mihft_chomp(char *s)
{
    size_t n;

    n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[n - 1U] = '\0';
        --n;
    }
}

static char *mihft_trim(char *s)
{
    char *e;

    while (*s != '\0' && isspace((unsigned char)*s)) {
        ++s;
    }

    e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) {
        --e;
    }
    *e = '\0';

    return s;
}

static int mihft_parse_i64(const char *s, long long *out)
{
    char *end;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno == ERANGE || end == s) {
        return -1;
    }

    while (*end != '\0') {
        if (!isspace((unsigned char)*end)) {
            return -1;
        }
        ++end;
    }

    *out = v;
    return 0;
}

static int mihft_copy_instr(char *dst, size_t dst_sz, const char *src)
{
    size_t n;

    if (dst_sz == 0U || src == NULL || *src == '\0') {
        return -1;
    }

    n = strlen(src);
    if (n >= dst_sz) {
        return -1;
    }

    memcpy(dst, src, n + 1U);
    return 0;
}

static int mihft_split_csv(char *line, char **cols, size_t want)
{
    size_t n;
    char *p;

    n = 0U;
    p = line;

    while (n < want) {
        cols[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p = '\0';
        ++p;
    }

    if (n != want || strchr(cols[want - 1U], ',') != NULL) {
        return -1;
    }

    for (n = 0U; n < want; ++n) {
        cols[n] = mihft_trim(cols[n]);
    }

    return 0;
}

static int mihft_parse_record(char *line, struct mihft_local_record *rec)
{
    char *cols[6];

    if (mihft_split_csv(line, cols, 6U) != 0) {
        return -1;
    }

    if (mihft_copy_instr(rec->instr_code, sizeof(rec->instr_code), cols[0]) != 0) {
        return -1;
    }
    if (mihft_parse_i64(cols[1], &rec->bid_amt) != 0 ||
        mihft_parse_i64(cols[2], &rec->ask_amt) != 0 ||
        mihft_parse_i64(cols[3], &rec->last_amt) != 0 ||
        mihft_parse_i64(cols[4], &rec->vol_qty) != 0 ||
        mihft_parse_i64(cols[5], &rec->tick_ts) != 0) {
        return -1;
    }

    if (rec->bid_amt < 0 || rec->ask_amt < 0 || rec->last_amt < 0 ||
        rec->vol_qty < 0 || rec->tick_ts < 0) {
        return -1;
    }

    return 0;
}

static int mihft_now_ns(long long *out)
{
    struct timespec ts;
    long long sec_ns;

    if (timespec_get(&ts, TIME_UTC) != TIME_UTC) {
        return -1;
    }

    if (ts.tv_sec > (time_t)(LLONG_MAX / 1000000000LL)) {
        return -1;
    }

    sec_ns = (long long)ts.tv_sec * 1000000000LL;
    if (ts.tv_nsec > LLONG_MAX - sec_ns) {
        return -1;
    }

    *out = sec_ns + (long long)ts.tv_nsec;
    return 0;
}

static int mihft_age_ns(long long now_ns, long long tick_ts, long long *age_ns)
{
    if (tick_ts > now_ns) {
        *age_ns = 0;
        return 0;
    }

    *age_ns = now_ns - tick_ts;
    return 0;
}

static enum mihft_local_state mihft_state_of(const struct mihft_local_record *rec,
                                             long long age_ns)
{
    if (age_ns > (long long)MIHFT_LOCAL_STALE_NS) {
        return MIHFT_LOCAL_STATE_STALE;
    }

    if (rec->bid_amt > rec->ask_amt) {
        return MIHFT_LOCAL_STATE_CROSSED;
    }

    if (rec->bid_amt == rec->ask_amt && rec->bid_amt > 0) {
        return MIHFT_LOCAL_STATE_LOCKED;
    }

    return MIHFT_LOCAL_STATE_FRESH;
}

static const char *mihft_state_name(enum mihft_local_state st)
{
    switch (st) {
    case MIHFT_LOCAL_STATE_FRESH:
        return "FRESH";
    case MIHFT_LOCAL_STATE_STALE:
        return "STALE";
    case MIHFT_LOCAL_STATE_CROSSED:
        return "CROSSED";
    case MIHFT_LOCAL_STATE_LOCKED:
        return "LOCKED";
    default:
        return "UNKNOWN";
    }
}

static int mihft_is_header(const char *line)
{
    char buf[256];
    char *cols[6];

    if (strlen(line) >= sizeof(buf)) {
        return 0;
    }

    strcpy(buf, line);
    if (mihft_split_csv(buf, cols, 6U) != 0) {
        return 0;
    }

    return strcmp(cols[0], "INSTR-CODE") == 0 &&
           strcmp(cols[1], "BID-AMT") == 0 &&
           strcmp(cols[2], "ASK-AMT") == 0 &&
           strcmp(cols[3], "LAST-AMT") == 0 &&
           strcmp(cols[4], "VOL-QTY") == 0 &&
           strcmp(cols[5], "TICK-TS") == 0;
}

int main(void)
{
    char line[512];
    unsigned long long lineno;
    long long now_ns;

    if (mihft_now_ns(&now_ns) != 0) {
        fprintf(stderr, "時刻取得失敗\n");
        return MIHFT_LOCAL_DECISION_IO_ERROR;
    }

    lineno = 0ULL;
    printf("INSTR-CODE,MARKET-STATE,AGE-NS,BID-AMT,ASK-AMT,LAST-AMT,VOL-QTY,TICK-TS\n");

    while (fgets(line, sizeof(line), stdin) != NULL) {
        struct mihft_local_record rec;
        enum mihft_local_state st;
        long long age_ns;

        ++lineno;

        if (strchr(line, '\n') == NULL && !feof(stdin)) {
            fprintf(stderr, "CSV行長超過:%llu\n", lineno);
            return MIHFT_LOCAL_DECISION_PARSE_ERROR;
        }

        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }

        if (lineno == 1ULL && mihft_is_header(line)) {
            continue;
        }

        if (mihft_parse_record(line, &rec) != 0) {
            fprintf(stderr, "CSV解析失敗:%llu\n", lineno);
            return MIHFT_LOCAL_DECISION_PARSE_ERROR;
        }

        if (mihft_age_ns(now_ns, rec.tick_ts, &age_ns) != 0) {
            fprintf(stderr, "時刻差分失敗:%llu\n", lineno);
            return MIHFT_LOCAL_DECISION_PARSE_ERROR;
        }

        st = mihft_state_of(&rec, age_ns);

        printf("%s,%s,%lld,%lld,%lld,%lld,%lld,%lld\n",
               rec.instr_code,
               mihft_state_name(st),
               age_ns,
               rec.bid_amt,
               rec.ask_amt,
               rec.last_amt,
               rec.vol_qty,
               rec.tick_ts);
    }

    if (ferror(stdin)) {
        fprintf(stderr, "SCMKTD読込失敗\n");
        return MIHFT_LOCAL_DECISION_IO_ERROR;
    }

    return MIHFT_LOCAL_DECISION_OK;
}
