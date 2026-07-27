/*
 * 変更履歴
 * 版数  年月日    担当    概要
 * 1.00  20200310  小林 直樹 (E-252)   初版作成
 * 1.01  20200810  小林 直樹 (E-252)   鮮度判定と場外判定の入力検証を追加
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef MIHFT_RC_NORMAL
#define MIHFT_RC_NORMAL 0
#endif

#ifndef MIHFT_RC_IO_ERROR
#define MIHFT_RC_IO_ERROR 12
#endif

#ifndef MIHFT_RC_PARSE_ERROR
#define MIHFT_RC_PARSE_ERROR 16
#endif

#ifndef MIHFT_MD_STALE_HOTPATH_NS
#define MIHFT_MD_STALE_HOTPATH_NS UINT64_C(5000000)
#endif

#ifndef MIHFT_MAX_CSV_LINE
#define MIHFT_MAX_CSV_LINE 512
#endif

#ifndef MIHFT_MAX_SESSIONS
#define MIHFT_MAX_SESSIONS 32
#endif

#ifndef MIHFT_MAX_INSTR_CODE
#define MIHFT_MAX_INSTR_CODE 32
#endif

#ifndef MIHFT_ALERT_KIND_STALE
#define MIHFT_ALERT_KIND_STALE "MDST"
#endif

#ifndef MIHFT_ALERT_KIND_OUT_SESSION
#define MIHFT_ALERT_KIND_OUT_SESSION "OSES"
#endif

#ifndef MIHFT_SEVERITY_WARN
#define MIHFT_SEVERITY_WARN "W"
#endif

#ifndef MIHFT_SEVERITY_CRIT
#define MIHFT_SEVERITY_CRIT "C"
#endif

typedef struct {
    char sess_dt[9];
    char sess_kbn[8];
    uint64_t open_ns;
    uint64_t close_ns;
} local_session_t;

typedef struct {
    char instr_code[MIHFT_MAX_INSTR_CODE];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    uint64_t vol_qty;
    uint64_t tick_ns;
} local_quote_t;

static void trim_field(char *s)
{
    size_t n;
    char *p = s;

    while (*p != '\0' && isspace((unsigned char)*p)) {
        ++p;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1U);
    }

    n = strlen(s);
    while (n > 0U && isspace((unsigned char)s[n - 1U])) {
        s[--n] = '\0';
    }
}

static int split_csv(char *line, char *field[], size_t expected)
{
    size_t count = 0U;
    char *p = line;

    while (count < expected) {
        field[count++] = p;
        while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
            ++p;
        }
        if (*p == ',') {
            *p++ = '\0';
            continue;
        }
        if (*p == '\n' || *p == '\r') {
            *p = '\0';
        }
        break;
    }

    if (count != expected) {
        return 0;
    }

    for (size_t i = 0U; i < expected; ++i) {
        trim_field(field[i]);
    }
    return 1;
}

static int parse_u64_dec(const char *s, uint64_t *out)
{
    uint64_t v = 0U;

    if (*s == '\0') {
        return 0;
    }
    while (*s != '\0') {
        unsigned d;
        if (!isdigit((unsigned char)*s)) {
            return 0;
        }
        d = (unsigned)(*s - '0');
        if (v > (UINT64_MAX - d) / 10U) {
            return 0;
        }
        v = (v * 10U) + d;
        ++s;
    }
    *out = v;
    return 1;
}

static int parse_i64_dec(const char *s, int64_t *out)
{
    uint64_t mag;
    int neg = 0;

    if (*s == '-') {
        neg = 1;
        ++s;
    } else if (*s == '+') {
        ++s;
    }

    if (!parse_u64_dec(s, &mag)) {
        return 0;
    }
    if (neg) {
        if (mag > ((uint64_t)INT64_MAX + 1U)) {
            return 0;
        }
        *out = (mag == ((uint64_t)INT64_MAX + 1U)) ? INT64_MIN : -(int64_t)mag;
    } else {
        if (mag > (uint64_t)INT64_MAX) {
            return 0;
        }
        *out = (int64_t)mag;
    }
    return 1;
}

static int parse_ymd(const char *s, int *year, int *mon, int *mday)
{
    if (strlen(s) != 8U) {
        return 0;
    }
    for (size_t i = 0U; i < 8U; ++i) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
    }

    *year = (s[0] - '0') * 1000 + (s[1] - '0') * 100 + (s[2] - '0') * 10 + (s[3] - '0');
    *mon = (s[4] - '0') * 10 + (s[5] - '0');
    *mday = (s[6] - '0') * 10 + (s[7] - '0');

    return *year >= 1970 && *mon >= 1 && *mon <= 12 && *mday >= 1 && *mday <= 31;
}

static int parse_hms_frac(const char *s, int *hour, int *min, int *sec, uint32_t *nsec)
{
    uint32_t frac = 0U;
    uint32_t scale = 100000000U;

    if (strlen(s) < 8U || s[2] != ':' || s[5] != ':') {
        return 0;
    }
    if (!isdigit((unsigned char)s[0]) || !isdigit((unsigned char)s[1]) ||
        !isdigit((unsigned char)s[3]) || !isdigit((unsigned char)s[4]) ||
        !isdigit((unsigned char)s[6]) || !isdigit((unsigned char)s[7])) {
        return 0;
    }

    *hour = (s[0] - '0') * 10 + (s[1] - '0');
    *min = (s[3] - '0') * 10 + (s[4] - '0');
    *sec = (s[6] - '0') * 10 + (s[7] - '0');

    if (*hour > 23 || *min > 59 || *sec > 60) {
        return 0;
    }

    s += 8;
    if (*s == '.') {
        ++s;
        if (!isdigit((unsigned char)*s)) {
            return 0;
        }
        while (*s != '\0') {
            if (!isdigit((unsigned char)*s) || scale == 0U) {
                return 0;
            }
            frac += (uint32_t)(*s - '0') * scale;
            scale /= 10U;
            ++s;
        }
    } else if (*s != '\0') {
        return 0;
    }

    *nsec = frac;
    return 1;
}

static int64_t days_from_civil(int y, unsigned m, unsigned d)
{
    int era;
    unsigned yoe;
    unsigned doy;
    unsigned doe;

    y -= (m <= 2U);
    era = (y >= 0 ? y : y - 399) / 400;
    yoe = (unsigned)(y - era * 400);
    doy = (153U * (m + (m > 2U ? UINT_MAX - 2U : 9U)) + 2U) / 5U + d - 1U;
    doe = yoe * 365U + yoe / 4U - yoe / 100U + doy;

    return (int64_t)era * 146097LL + (int64_t)doe - 719468LL;
}

static int build_epoch_ns(const char *dt, const char *ts, uint64_t *out)
{
    int year, mon, mday, hour, min, sec;
    uint32_t nsec;
    int64_t days;
    uint64_t seconds;

    if (!parse_ymd(dt, &year, &mon, &mday) || !parse_hms_frac(ts, &hour, &min, &sec, &nsec)) {
        return 0;
    }

    days = days_from_civil(year, (unsigned)mon, (unsigned)mday);
    if (days < 0) {
        return 0;
    }

    seconds = (uint64_t)days * 86400U + (uint64_t)hour * 3600U + (uint64_t)min * 60U + (uint64_t)sec;
    if (seconds > (UINT64_MAX - nsec) / 1000000000U) {
        return 0;
    }

    *out = seconds * 1000000000U + nsec;
    return 1;
}

static int parse_timestamp_ns(const char *s, const char *fallback_dt, uint64_t *out)
{
    char dt[9];
    char tm[32];

    if (strchr(s, ':') == NULL) {
        return parse_u64_dec(s, out);
    }

    if (strlen(s) >= 17U && s[8] == '-') {
        memcpy(dt, s, 8U);
        dt[8] = '\0';
        if (strlen(s + 9) >= sizeof(tm)) {
            return 0;
        }
        strcpy(tm, s + 9);
        return build_epoch_ns(dt, tm, out);
    }

    if (fallback_dt == NULL || strlen(s) >= sizeof(tm)) {
        return 0;
    }
    strcpy(tm, s);
    return build_epoch_ns(fallback_dt, tm, out);
}

static int read_sessions(const char *path, local_session_t sess[], size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_CSV_LINE];
    size_t n = 0U;
    unsigned long row = 0UL;

    if (fp == NULL) {
        fprintf(stderr, "E001:SCCALFをオープンできません\n");
        return MIHFT_RC_IO_ERROR;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[4];
        ++row;
        if (row == 1UL && strstr(line, "SESS-DT") != NULL) {
            continue;
        }
        if (!split_csv(line, f, 4U)) {
            fprintf(stderr, "E002:SCCALFの項目数が不正です:%lu\n", row);
            fclose(fp);
            return MIHFT_RC_PARSE_ERROR;
        }
        if (n >= MIHFT_MAX_SESSIONS) {
            fprintf(stderr, "E003:SCCALFの件数が上限超過です\n");
            fclose(fp);
            return MIHFT_RC_PARSE_ERROR;
        }
        if (strlen(f[0]) >= sizeof sess[n].sess_dt || strlen(f[1]) >= sizeof sess[n].sess_kbn) {
            fprintf(stderr, "E004:SCCALFのコード長が不正です:%lu\n", row);
            fclose(fp);
            return MIHFT_RC_PARSE_ERROR;
        }

        strcpy(sess[n].sess_dt, f[0]);
        strcpy(sess[n].sess_kbn, f[1]);
        if (!parse_timestamp_ns(f[2], sess[n].sess_dt, &sess[n].open_ns) ||
            !parse_timestamp_ns(f[3], sess[n].sess_dt, &sess[n].close_ns) ||
            sess[n].open_ns >= sess[n].close_ns) {
            fprintf(stderr, "E005:SCCALFの時刻が不正です:%lu\n", row);
            fclose(fp);
            return MIHFT_RC_PARSE_ERROR;
        }
        ++n;
    }

    if (ferror(fp)) {
        fprintf(stderr, "E006:SCCALFの読込に失敗しました\n");
        fclose(fp);
        return MIHFT_RC_IO_ERROR;
    }
    fclose(fp);

    if (n == 0U) {
        fprintf(stderr, "E007:SCCALFが空です\n");
        return MIHFT_RC_PARSE_ERROR;
    }

    *count = n;
    return MIHFT_RC_NORMAL;
}

static int parse_quote_line(char *line, local_quote_t *q, unsigned long row)
{
    char *f[6];

    if (!split_csv(line, f, 6U)) {
        fprintf(stderr, "E101:SCMKTDの項目数が不正です:%lu\n", row);
        return 0;
    }
    if (f[0][0] == '\0' || strlen(f[0]) >= sizeof q->instr_code) {
        fprintf(stderr, "E102:SCMKTDの銘柄コードが不正です:%lu\n", row);
        return 0;
    }

    strcpy(q->instr_code, f[0]);
    if (!parse_i64_dec(f[1], &q->bid_amt) ||
        !parse_i64_dec(f[2], &q->ask_amt) ||
        !parse_i64_dec(f[3], &q->last_amt) ||
        !parse_u64_dec(f[4], &q->vol_qty) ||
        !parse_timestamp_ns(f[5], NULL, &q->tick_ns)) {
        fprintf(stderr, "E103:SCMKTDの数値項目が不正です:%lu\n", row);
        return 0;
    }

    if (q->bid_amt < 0 || q->ask_amt < 0 || q->last_amt < 0 || q->bid_amt > q->ask_amt) {
        fprintf(stderr, "E104:SCMKTDの気配値が不正です:%lu\n", row);
        return 0;
    }

    return 1;
}

static int find_active_session(const local_session_t sess[], size_t count, uint64_t tick_ns)
{
    for (size_t i = 0U; i < count; ++i) {
        if (sess[i].open_ns <= tick_ns && tick_ns <= sess[i].close_ns) {
            return (int)i;
        }
    }
    return -1;
}

static uint64_t latest_close_ns(const local_session_t sess[], size_t count)
{
    uint64_t v = sess[0].close_ns;

    for (size_t i = 1U; i < count; ++i) {
        if (sess[i].close_ns > v) {
            v = sess[i].close_ns;
        }
    }
    return v;
}

static int write_alert(FILE *out, uint64_t *seq, const local_quote_t *q,
                       const char *kind, const char *severity,
                       uint64_t observed, uint64_t limit, uint64_t event_ns)
{
    if (fprintf(out, "A%012" PRIu64 ",%s,%s,%s,%" PRIu64 ",%" PRIu64 ",%" PRIu64 "\n",
                *seq, q->instr_code, kind, severity, observed, limit, event_ns) < 0) {
        fprintf(stderr, "E201:SCHALTの書込に失敗しました\n");
        return MIHFT_RC_IO_ERROR;
    }

    if (*seq == UINT64_MAX) {
        fprintf(stderr, "E202:SCHALTの採番が上限超過です\n");
        return MIHFT_RC_PARSE_ERROR;
    }
    ++(*seq);
    return MIHFT_RC_NORMAL;
}

int main(void)
{
    local_session_t sessions[MIHFT_MAX_SESSIONS];
    size_t session_count = 0U;
    uint64_t event_ns;
    uint64_t alert_seq = 1U;
    FILE *in;
    FILE *out;
    char line[MIHFT_MAX_CSV_LINE];
    unsigned long row = 0UL;
    int rc;

    rc = read_sessions("SCCALF.csv", sessions, &session_count);
    if (rc != MIHFT_RC_NORMAL) {
        return rc;
    }

    event_ns = latest_close_ns(sessions, session_count);

    in = fopen("SCMKTD.csv", "r");
    if (in == NULL) {
        fprintf(stderr, "E301:SCMKTDをオープンできません\n");
        return MIHFT_RC_IO_ERROR;
    }

    out = fopen("SCHALT.dat", "w");
    if (out == NULL) {
        fprintf(stderr, "E302:SCHALTをオープンできません\n");
        fclose(in);
        return MIHFT_RC_IO_ERROR;
    }

    while (fgets(line, sizeof line, in) != NULL) {
        local_quote_t quote;
        int active;
        uint64_t age_ns;

        ++row;
        if (row == 1UL && strstr(line, "INSTR-CODE") != NULL) {
            continue;
        }

        if (!parse_quote_line(line, &quote, row)) {
            fclose(out);
            fclose(in);
            return MIHFT_RC_PARSE_ERROR;
        }

        if (quote.tick_ns > event_ns) {
            event_ns = quote.tick_ns;
        }

        active = find_active_session(sessions, session_count, quote.tick_ns);
        if (active < 0) {
            rc = write_alert(out, &alert_seq, &quote, MIHFT_ALERT_KIND_OUT_SESSION,
                             MIHFT_SEVERITY_CRIT, 0U, 0U, event_ns);
            if (rc != MIHFT_RC_NORMAL) {
                fclose(out);
                fclose(in);
                return rc;
            }
            continue;
        }

        age_ns = event_ns - quote.tick_ns;
        if (age_ns > MIHFT_MD_STALE_HOTPATH_NS) {
            rc = write_alert(out, &alert_seq, &quote, MIHFT_ALERT_KIND_STALE,
                             MIHFT_SEVERITY_WARN, age_ns,
                             MIHFT_MD_STALE_HOTPATH_NS, event_ns);
            if (rc != MIHFT_RC_NORMAL) {
                fclose(out);
                fclose(in);
                return rc;
            }
        }
    }

    if (ferror(in)) {
        fprintf(stderr, "E303:SCMKTDの読込に失敗しました\n");
        fclose(out);
        fclose(in);
        return MIHFT_RC_IO_ERROR;
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "E304:SCHALTのクローズに失敗しました\n");
        fclose(in);
        return MIHFT_RC_IO_ERROR;
    }
    if (fclose(in) != 0) {
        fprintf(stderr, "E305:SCMKTDのクローズに失敗しました\n");
        return MIHFT_RC_IO_ERROR;
    }

    return MIHFT_RC_NORMAL;
}
