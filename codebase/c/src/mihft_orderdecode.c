/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20200902  篠原 健 (E-203)     初版作成、SCORDFデコードおよびHFDECLOG出力
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define SCORDF_PATH "SCORDF.csv"
#define HFDECLOG_PATH "HFDECLOG.csv"

#define MIHFT_ACCEPT 0
#define MIHFT_REJECT_MARGIN 4
#define MIHFT_REJECT_NOTIONAL 8
#define MIHFT_REJECT_TICK 12
#define MIHFT_IOERR 16

#define LINE_MAX_LEN 1024
#define FIELD_MAX 9
#define ID_MAX_LEN 64
#define CODE_MAX_LEN 32

typedef struct {
    char order_id[ID_MAX_LEN];
    char cif_no[ID_MAX_LEN];
    char instr_code[CODE_MAX_LEN];
    char side_kbn[CODE_MAX_LEN];
    char ord_type[CODE_MAX_LEN];
    char tif_code[CODE_MAX_LEN];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} scordf_record_t;

typedef struct {
    int rate_bp;
    int64_t tick;
} tier_rule_t;

static int trim_field(char *s)
{
    size_t n;
    char *p;

    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n') {
        memmove(s, s + 1, strlen(s));
    }

    n = strlen(s);
    while (n > 0 && (s[n - 1] == ' ' || s[n - 1] == '\t' ||
                     s[n - 1] == '\r' || s[n - 1] == '\n')) {
        s[--n] = '\0';
    }

    if (n >= 2 && s[0] == '"' && s[n - 1] == '"') {
        memmove(s, s + 1, n - 2);
        s[n - 2] = '\0';
        for (p = s; *p != '\0'; ++p) {
            if (*p == '"' && p[1] == '"') {
                memmove(p, p + 1, strlen(p));
            }
        }
    }

    return 0;
}

static int split_csv(char *line, char *field[], size_t cap)
{
    size_t count = 0;
    int quote = 0;
    char *p = line;

    if (cap == 0) {
        return -1;
    }

    field[count++] = p;
    for (; *p != '\0'; ++p) {
        if (*p == '"') {
            quote = !quote;
        } else if (*p == ',' && !quote) {
            if (count >= cap) {
                return -1;
            }
            *p = '\0';
            field[count++] = p + 1;
        }
    }

    if (quote) {
        return -1;
    }

    return (int)count;
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

static int parse_int(const char *s, int *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int copy_code(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dstsz) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_scordf(char *line, scordf_record_t *rec)
{
    char *field[FIELD_MAX];
    int n;

    n = split_csv(line, field, FIELD_MAX);
    if (n != FIELD_MAX) {
        return -1;
    }

    for (n = 0; n < FIELD_MAX; ++n) {
        trim_field(field[n]);
    }

    if (copy_code(rec->order_id, sizeof(rec->order_id), field[0]) != 0 ||
        copy_code(rec->cif_no, sizeof(rec->cif_no), field[1]) != 0 ||
        copy_code(rec->instr_code, sizeof(rec->instr_code), field[2]) != 0 ||
        copy_code(rec->side_kbn, sizeof(rec->side_kbn), field[3]) != 0 ||
        copy_code(rec->ord_type, sizeof(rec->ord_type), field[4]) != 0 ||
        copy_code(rec->tif_code, sizeof(rec->tif_code), field[5]) != 0) {
        return -1;
    }

    if (parse_i64(field[6], &rec->ord_qty) != 0 ||
        parse_i64(field[7], &rec->price_amt) != 0 ||
        parse_int(field[8], &rec->instr_tier) != 0) {
        return -1;
    }

    return 0;
}

static int tier_rule(int tier, tier_rule_t *rule)
{
    switch (tier) {
    case 1:
        rule->rate_bp = 1000;
        rule->tick = 100;
        return 0;
    case 2:
        rule->rate_bp = 2000;
        rule->tick = 500;
        return 0;
    case 3:
        rule->rate_bp = 4000;
        rule->tick = 1000;
        return 0;
    default:
        return -1;
    }
}

static int valid_code_set(const scordf_record_t *rec)
{
    if (!(strcmp(rec->side_kbn, "B") == 0 || strcmp(rec->side_kbn, "S") == 0)) {
        return 0;
    }

    if (!(strcmp(rec->ord_type, "L") == 0 || strcmp(rec->ord_type, "M") == 0)) {
        return 0;
    }

    if (!(strcmp(rec->tif_code, "DAY") == 0 ||
          strcmp(rec->tif_code, "IOC") == 0 ||
          strcmp(rec->tif_code, "FOK") == 0)) {
        return 0;
    }

    return 1;
}

static int checked_notional(const scordf_record_t *rec, int64_t *notional)
{
    if (rec->ord_qty <= 0 || rec->price_amt < 0) {
        return -1;
    }

    if (strcmp(rec->ord_type, "M") == 0) {
        *notional = 0;
        return 0;
    }

    if (rec->price_amt == 0 || rec->ord_qty > INT64_MAX / rec->price_amt) {
        return -1;
    }

    *notional = rec->ord_qty * rec->price_amt;
    return 0;
}

static int decide_order(const scordf_record_t *rec)
{
    tier_rule_t rule;
    int64_t notional;
    int64_t margin;

    if (!valid_code_set(rec) || tier_rule(rec->instr_tier, &rule) != 0) {
        return MIHFT_REJECT_MARGIN;
    }

    if (checked_notional(rec, &notional) != 0) {
        return MIHFT_REJECT_NOTIONAL;
    }

    if (notional > MIHFT_MAX_NOTIONAL) {
        return MIHFT_REJECT_NOTIONAL;
    }

    if (strcmp(rec->ord_type, "L") == 0 && rec->price_amt % rule.tick != 0) {
        return MIHFT_REJECT_TICK;
    }

    if (notional > 0 && notional > INT64_MAX / rule.rate_bp) {
        return MIHFT_REJECT_MARGIN;
    }

    margin = (notional * rule.rate_bp + 9999) / 10000;
    if (margin < 0) {
        return MIHFT_REJECT_MARGIN;
    }

    return MIHFT_ACCEPT;
}

static const char *reason_code(int decision)
{
    switch (decision) {
    case MIHFT_ACCEPT:
        return "ACCEPT";
    case MIHFT_REJECT_MARGIN:
        return "REJECT-MARGIN";
    case MIHFT_REJECT_NOTIONAL:
        return "REJECT-NOTIONAL";
    case MIHFT_REJECT_TICK:
        return "REJECT-TICK";
    default:
        return "PARSE-IO";
    }
}

static int now_ts(char *buf, size_t len)
{
    time_t t;
    struct tm tmv;

    t = time(NULL);
    if (t == (time_t)-1) {
        return -1;
    }

#if defined(_POSIX_VERSION)
    if (localtime_r(&t, &tmv) == NULL) {
        return -1;
    }
#else
    {
        struct tm *tmp = localtime(&t);
        if (tmp == NULL) {
            return -1;
        }
        tmv = *tmp;
    }
#endif

    if (strftime(buf, len, "%Y%m%d%H%M%S", &tmv) == 0) {
        return -1;
    }

    return 0;
}

static int write_hfdeclog(FILE *out, uint64_t decision_id,
                          const scordf_record_t *rec, int decision)
{
    char ts[32];

    if (now_ts(ts, sizeof(ts)) != 0) {
        return -1;
    }

    if (fprintf(out, "%" PRIu64 ",%s,%s,%d,%s,%s\n",
                decision_id,
                rec->order_id,
                rec->instr_code,
                decision,
                reason_code(decision),
                ts) < 0) {
        return -1;
    }

    return 0;
}

static int feed_hotpath(const scordf_record_t *rec)
{
    volatile int64_t qty = rec->ord_qty;
    volatile int64_t px = rec->price_amt;

    return (qty > 0 && px >= 0) ? MIHFT_ACCEPT : MIHFT_REJECT_NOTIONAL;
}

int main(void)
{
    FILE *in;
    FILE *out;
    char line[LINE_MAX_LEN];
    uint64_t decision_id = 1;
    int exit_code = MIHFT_ACCEPT;

    in = fopen(SCORDF_PATH, "r");
    if (in == NULL) {
        return MIHFT_IOERR;
    }

    out = fopen(HFDECLOG_PATH, "w");
    if (out == NULL) {
        fclose(in);
        return MIHFT_IOERR;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        scordf_record_t rec;
        int decision;

        if (strchr(line, '\n') == NULL && !feof(in)) {
            fclose(out);
            fclose(in);
            return MIHFT_IOERR;
        }

        if (strncmp(line, "ORDER-ID,", 9) == 0) {
            continue;
        }

        if (parse_scordf(line, &rec) != 0) {
            fclose(out);
            fclose(in);
            return MIHFT_IOERR;
        }

        decision = decide_order(&rec);
        if (write_hfdeclog(out, decision_id++, &rec, decision) != 0) {
            fclose(out);
            fclose(in);
            return MIHFT_IOERR;
        }

        if (decision == MIHFT_ACCEPT) {
            decision = feed_hotpath(&rec);
        }

        if (decision != MIHFT_ACCEPT) {
            exit_code = decision;
        }
    }

    if (ferror(in) || fflush(out) != 0) {
        fclose(out);
        fclose(in);
        return MIHFT_IOERR;
    }

    if (fclose(out) != 0) {
        fclose(in);
        return MIHFT_IOERR;
    }

    if (fclose(in) != 0) {
        return MIHFT_IOERR;
    }

    return exit_code;
}
