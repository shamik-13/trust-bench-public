/************************************************************
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20231114  今井 彩 (E-230)  初版作成
 * 1.01  20240414  中川 美和 (E-283)  限度額超過区分と追記出力を追加
 ************************************************************/

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_RC_IOERR  16
#define MIHFT_RC_PARSE 20

#define SCCUST_PATH   "SCCUST.csv"
#define SCEXPF_PATH   "SCEXPF.csv"
#define SCRISKF2_PATH "SCRISKF2.csv"

#define DECISION_ACCEPT          0
#define DECISION_REJECT_MARGIN   4
#define DECISION_REJECT_NOTIONAL 8
#define DECISION_REJECT_TICK     12

#define MAX_LINE_LEN 512
#define MAX_CIF_LEN  32
#define MAX_DATE_LEN 16
#define MAX_INST_LEN 16
#define EVENT_TS_LEN 32

struct cust_rec {
    char cif_no[MAX_CIF_LEN];
    int64_t group_limit;
    int64_t group_used;
    int64_t acct_used;
};

struct exp_rec {
    char cif_no[MAX_CIF_LEN];
    char sess_dt[MAX_DATE_LEN];
    int64_t gross_long;
    int64_t gross_short;
    int64_t net_exposure;
    int32_t limit_util_pct;
};

static void trim_field(char *s)
{
    size_t n;
    char *p;

    while (isspace((unsigned char)*s) != 0) {
        memmove(s, s + 1, strlen(s));
    }

    n = strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1]) != 0) {
        s[--n] = '\0';
    }

    if (s[0] == '"' && n >= 2 && s[n - 1] == '"') {
        memmove(s, s + 1, n - 2);
        s[n - 2] = '\0';
    }

    for (p = s; *p != '\0'; ++p) {
        if ((unsigned char)*p < 0x20U) {
            *p = '\0';
            break;
        }
    }
}

static int split_csv(char *line, char **field, size_t cap)
{
    size_t n = 0;
    int in_quote = 0;
    char *p = line;

    if (cap == 0) {
        return -1;
    }

    field[n++] = p;
    for (; *p != '\0'; ++p) {
        if (*p == '"') {
            in_quote = !in_quote;
        } else if (*p == ',' && in_quote == 0) {
            *p = '\0';
            if (n >= cap) {
                return -1;
            }
            field[n++] = p + 1;
        }
    }

    for (size_t i = 0; i < n; ++i) {
        trim_field(field[i]);
    }

    return (int)n;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *endp;
    int saved_errno;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &endp, 10);
    saved_errno = errno;

    while (isspace((unsigned char)*endp) != 0) {
        ++endp;
    }

    if (saved_errno != 0 || *endp != '\0') {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int parse_i32(const char *s, int32_t *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v < INT32_MIN || v > INT32_MAX) {
        return -1;
    }

    *out = (int32_t)v;
    return 0;
}

static int checked_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int abs_i64_checked(int64_t v, int64_t *out)
{
    if (v == INT64_MIN) {
        return -1;
    }
    *out = (v < 0) ? -v : v;
    return 0;
}

static int next_data_line(FILE *fp, char *buf, size_t len)
{
    while (fgets(buf, (int)len, fp) != NULL) {
        char *p = buf;

        while (isspace((unsigned char)*p) != 0) {
            ++p;
        }

        if (*p == '\0' || *p == '#') {
            continue;
        }

        if (strstr(p, "CIF-NO") != NULL || strstr(p, "CIF_NO") != NULL) {
            continue;
        }

        return 1;
    }

    return ferror(fp) ? -1 : 0;
}

static int read_customer(FILE *fp, struct cust_rec *rec)
{
    char line[MAX_LINE_LEN];
    char *f[4];
    int n;

    n = next_data_line(fp, line, sizeof line);
    if (n <= 0) {
        return n;
    }

    n = split_csv(line, f, 4);
    if (n != 4 || strlen(f[0]) >= sizeof rec->cif_no) {
        return -1;
    }

    strcpy(rec->cif_no, f[0]);

    if (parse_i64(f[1], &rec->group_limit) != 0 ||
        parse_i64(f[2], &rec->group_used) != 0 ||
        parse_i64(f[3], &rec->acct_used) != 0) {
        return -1;
    }

    if (rec->group_limit < 0 || rec->group_used < 0 || rec->acct_used < 0) {
        return -1;
    }

    return 1;
}

static int read_exposure(FILE *fp, struct exp_rec *rec)
{
    char line[MAX_LINE_LEN];
    char *f[6];
    int n;

    n = next_data_line(fp, line, sizeof line);
    if (n <= 0) {
        return n;
    }

    n = split_csv(line, f, 6);
    if (n != 6 || strlen(f[0]) >= sizeof rec->cif_no || strlen(f[1]) >= sizeof rec->sess_dt) {
        return -1;
    }

    strcpy(rec->cif_no, f[0]);
    strcpy(rec->sess_dt, f[1]);

    if (parse_i64(f[2], &rec->gross_long) != 0 ||
        parse_i64(f[3], &rec->gross_short) != 0 ||
        parse_i64(f[4], &rec->net_exposure) != 0 ||
        parse_i32(f[5], &rec->limit_util_pct) != 0) {
        return -1;
    }

    if (rec->gross_long < 0 || rec->gross_short < 0 || rec->limit_util_pct < 0) {
        return -1;
    }

    return 1;
}

static const char *make_instr_code(const struct exp_rec *exp, char *buf, size_t len)
{
    uint32_t h = 2166136261u;
    const unsigned char *p = (const unsigned char *)exp->cif_no;

    while (*p != '\0') {
        h ^= (uint32_t)*p++;
        h *= 16777619u;
    }

    snprintf(buf, len, "JP%04u", (unsigned)(h % 10000u));
    return buf;
}

static int32_t tier_rate_bp(int32_t pct)
{
    if (pct >= 85) {
        return 4000;
    }
    if (pct >= 60) {
        return 2000;
    }
    return 1000;
}

static int64_t tier_tick(int32_t pct)
{
    if (pct >= 85) {
        return 1000;
    }
    if (pct >= 60) {
        return 500;
    }
    return 100;
}

static int calc_margin(int64_t notional, int32_t rate_bp, int64_t *margin)
{
    int64_t q;
    int64_t r;

    if (notional < 0 || rate_bp <= 0 || notional > INT64_MAX / rate_bp) {
        return -1;
    }

    q = (notional * rate_bp) / 10000;
    r = (notional * rate_bp) % 10000;
    if (r != 0 && q == INT64_MAX) {
        return -1;
    }

    *margin = q + ((r != 0) ? 1 : 0);
    return 0;
}

static void make_event_ts(char *buf, size_t len)
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_POSIX_VERSION)
    localtime_r(&now, &tmv);
#else
    struct tm *tmp = localtime(&now);
    if (tmp != NULL) {
        tmv = *tmp;
    } else {
        memset(&tmv, 0, sizeof tmv);
    }
#endif

    strftime(buf, len, "%Y%m%d%H%M%S", &tmv);
}

static int write_risk(FILE *fp,
                      uint64_t event_id,
                      const struct cust_rec *cust,
                      const struct exp_rec *exp,
                      int64_t limit_amt,
                      int64_t used_amt,
                      int decision)
{
    char ts[EVENT_TS_LEN];
    char instr[MAX_INST_LEN];

    make_event_ts(ts, sizeof ts);
    make_instr_code(exp, instr, sizeof instr);

    if (fprintf(fp,
                "%" PRIu64 ",%s,%s,%s,%" PRId64 ",%" PRId64 ",%d\n",
                event_id,
                cust->cif_no,
                instr,
                ts,
                limit_amt,
                used_amt,
                decision) < 0) {
        return -1;
    }

    return 0;
}

static int same_cif(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

int main(void)
{
    FILE *fcust;
    FILE *fexp;
    FILE *frisk;
    struct cust_rec cust;
    struct exp_rec exp;
    uint64_t event_id = 1;
    int worst_decision = DECISION_ACCEPT;
    int rc_cust;
    int rc_exp;

    fcust = fopen(SCCUST_PATH, "r");
    if (fcust == NULL) {
        fprintf(stderr, "SCCUSTオープン失敗\n");
        return MIHFT_RC_IOERR;
    }

    fexp = fopen(SCEXPF_PATH, "r");
    if (fexp == NULL) {
        fclose(fcust);
        fprintf(stderr, "SCEXPFオープン失敗\n");
        return MIHFT_RC_IOERR;
    }

    frisk = fopen(SCRISKF2_PATH, "a");
    if (frisk == NULL) {
        fclose(fexp);
        fclose(fcust);
        fprintf(stderr, "SCRISKF2オープン失敗\n");
        return MIHFT_RC_IOERR;
    }

    for (;;) {
        int64_t abs_net;
        int64_t trade_notional;
        int64_t margin_amt;
        int64_t acct_projected;
        int64_t group_projected;
        int64_t tick;
        int32_t rate_bp;
        int decision;
        int64_t limit_amt;
        int64_t used_amt;

        rc_cust = read_customer(fcust, &cust);
        rc_exp = read_exposure(fexp, &exp);

        if (rc_cust == 0 && rc_exp == 0) {
            break;
        }

        if (rc_cust <= 0 || rc_exp <= 0) {
            fprintf(stderr, "入力件数不一致\n");
            fclose(frisk);
            fclose(fexp);
            fclose(fcust);
            return (rc_cust < 0 || rc_exp < 0) ? MIHFT_RC_PARSE : MIHFT_RC_IOERR;
        }

        if (!same_cif(cust.cif_no, exp.cif_no)) {
            fprintf(stderr, "CIF照合失敗\n");
            fclose(frisk);
            fclose(fexp);
            fclose(fcust);
            return MIHFT_RC_PARSE;
        }

        if (abs_i64_checked(exp.net_exposure, &abs_net) != 0 ||
            checked_add_i64(exp.gross_long, exp.gross_short, &trade_notional) != 0) {
            fprintf(stderr, "金額桁あふれ\n");
            fclose(frisk);
            fclose(fexp);
            fclose(fcust);
            return MIHFT_RC_PARSE;
        }

        if (trade_notional < abs_net) {
            trade_notional = abs_net;
        }

        rate_bp = tier_rate_bp(exp.limit_util_pct);
        tick = tier_tick(exp.limit_util_pct);

        if (calc_margin(trade_notional, rate_bp, &margin_amt) != 0 ||
            checked_add_i64(cust.acct_used, margin_amt, &acct_projected) != 0 ||
            checked_add_i64(cust.group_used, margin_amt, &group_projected) != 0) {
            fprintf(stderr, "証拠金計算失敗\n");
            fclose(frisk);
            fclose(fexp);
            fclose(fcust);
            return MIHFT_RC_PARSE;
        }

        if (trade_notional == 0 || (trade_notional % tick) != 0) {
            decision = DECISION_REJECT_TICK;
            limit_amt = tick;
            used_amt = trade_notional;
        } else if (trade_notional > MIHFT_MAX_NOTIONAL) {
            decision = DECISION_REJECT_NOTIONAL;
            limit_amt = MIHFT_MAX_NOTIONAL;
            used_amt = trade_notional;
        } else if (acct_projected > cust.group_limit) {
            decision = DECISION_REJECT_MARGIN;
            limit_amt = cust.group_limit;
            used_amt = acct_projected;
        } else if (group_projected > cust.group_limit) {
            decision = DECISION_REJECT_NOTIONAL;
            limit_amt = cust.group_limit;
            used_amt = group_projected;
        } else if (exp.limit_util_pct >= 90) {
            decision = DECISION_REJECT_MARGIN;
            limit_amt = cust.group_limit;
            used_amt = group_projected;
        } else {
            decision = DECISION_ACCEPT;
            limit_amt = cust.group_limit;
            used_amt = group_projected;
        }

        if (write_risk(frisk, event_id++, &cust, &exp, limit_amt, used_amt, decision) != 0) {
            fprintf(stderr, "SCRISKF2書込失敗\n");
            fclose(frisk);
            fclose(fexp);
            fclose(fcust);
            return MIHFT_RC_IOERR;
        }

        if (decision > worst_decision) {
            worst_decision = decision;
        }
    }

    if (fclose(frisk) != 0) {
        fclose(fexp);
        fclose(fcust);
        fprintf(stderr, "SCRISKF2クローズ失敗\n");
        return MIHFT_RC_IOERR;
    }

    fclose(fexp);
    fclose(fcust);

    return worst_decision;
}
