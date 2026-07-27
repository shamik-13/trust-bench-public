/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220906  大野 修 (E-225)  注文元本ガード初版
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <errno.h>
#include <time.h>
#include <limits.h>

#include "mihft_types.h"

#define DEC_ACCEPT          0
#define DEC_REJECT_MARGIN   4
#define DEC_REJECT_NOTIONAL 8
#define DEC_REJECT_TICK     12

#define LINE_MAX_LEN        1024
#define FIELD_MAX_LEN       128
#define INSTR_MAX_LEN       32
#define NAME_MAX_LEN        96
#define ID_MAX_LEN          64
#define SIDE_MAX_LEN        4
#define TYPE_MAX_LEN        4
#define TIF_MAX_LEN         8
#define BOARD_MAX_LEN       8
#define TS_MAX_LEN          32
#define TABLE_MAX_REC       4096

typedef struct {
    char order_id[ID_MAX_LEN];
    char cif_no[ID_MAX_LEN];
    char instr_code[INSTR_MAX_LEN];
    char side_kbn[SIDE_MAX_LEN];
    char ord_type[TYPE_MAX_LEN];
    char tif_code[TIF_MAX_LEN];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} scordf_rec_t;

typedef struct {
    char instr_code[INSTR_MAX_LEN];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    char tick_ts[TS_MAX_LEN];
} scmktd_rec_t;

typedef struct {
    char instr_code[INSTR_MAX_LEN];
    char instr_name[NAME_MAX_LEN];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[BOARD_MAX_LEN];
} scinstf_rec_t;

typedef struct {
    scmktd_rec_t rows[TABLE_MAX_REC];
    size_t used;
} scmktd_table_t;

typedef struct {
    scinstf_rec_t rows[TABLE_MAX_REC];
    size_t used;
} scinstf_table_t;

static int trim_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
    return 0;
}

static int next_field(char **cur, char *out, size_t outsz)
{
    char *p = *cur;
    char *q;
    size_t len;

    if (p == NULL || outsz == 0U) {
        return -1;
    }

    q = strchr(p, ',');
    if (q != NULL) {
        len = (size_t)(q - p);
        *cur = q + 1;
    } else {
        len = strlen(p);
        *cur = NULL;
    }

    if (len >= outsz) {
        return -1;
    }

    memcpy(out, p, len);
    out[len] = '\0';
    return 0;
}

static int parse_i64(const char *s, int64_t *v)
{
    char *endp;
    long long tmp;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    tmp = strtoll(s, &endp, 10);
    if (errno != 0 || *endp != '\0') {
        return -1;
    }

    *v = (int64_t)tmp;
    return 0;
}

static int parse_int(const char *s, int *v)
{
    int64_t tmp;

    if (parse_i64(s, &tmp) != 0 || tmp < INT_MIN || tmp > INT_MAX) {
        return -1;
    }

    *v = (int)tmp;
    return 0;
}

static int parse_scordf_line(char *line, scordf_rec_t *r)
{
    char *cur = line;
    char f[FIELD_MAX_LEN];

    trim_eol(line);

    if (next_field(&cur, r->order_id, sizeof(r->order_id)) != 0) return -1;
    if (next_field(&cur, r->cif_no, sizeof(r->cif_no)) != 0) return -1;
    if (next_field(&cur, r->instr_code, sizeof(r->instr_code)) != 0) return -1;
    if (next_field(&cur, r->side_kbn, sizeof(r->side_kbn)) != 0) return -1;
    if (next_field(&cur, r->ord_type, sizeof(r->ord_type)) != 0) return -1;
    if (next_field(&cur, r->tif_code, sizeof(r->tif_code)) != 0) return -1;
    if (next_field(&cur, f, sizeof(f)) != 0 || parse_i64(f, &r->ord_qty) != 0) return -1;
    if (next_field(&cur, f, sizeof(f)) != 0 || parse_i64(f, &r->price_amt) != 0) return -1;
    if (next_field(&cur, f, sizeof(f)) != 0 || parse_int(f, &r->instr_tier) != 0) return -1;

    return cur == NULL ? 0 : -1;
}

static int parse_scmktd_line(char *line, scmktd_rec_t *r)
{
    char *cur = line;
    char f[FIELD_MAX_LEN];

    trim_eol(line);

    if (next_field(&cur, r->instr_code, sizeof(r->instr_code)) != 0) return -1;
    if (next_field(&cur, f, sizeof(f)) != 0 || parse_i64(f, &r->bid_amt) != 0) return -1;
    if (next_field(&cur, f, sizeof(f)) != 0 || parse_i64(f, &r->ask_amt) != 0) return -1;
    if (next_field(&cur, f, sizeof(f)) != 0 || parse_i64(f, &r->last_amt) != 0) return -1;
    if (next_field(&cur, f, sizeof(f)) != 0 || parse_i64(f, &r->vol_qty) != 0) return -1;
    if (next_field(&cur, r->tick_ts, sizeof(r->tick_ts)) != 0) return -1;

    return cur == NULL ? 0 : -1;
}

static int parse_scinstf_line(char *line, scinstf_rec_t *r)
{
    char *cur = line;
    char f[FIELD_MAX_LEN];

    trim_eol(line);

    if (next_field(&cur, r->instr_code, sizeof(r->instr_code)) != 0) return -1;
    if (next_field(&cur, r->instr_name, sizeof(r->instr_name)) != 0) return -1;
    if (next_field(&cur, f, sizeof(f)) != 0 || parse_int(f, &r->instr_tier) != 0) return -1;
    if (next_field(&cur, f, sizeof(f)) != 0 || parse_i64(f, &r->tick_amt) != 0) return -1;
    if (next_field(&cur, f, sizeof(f)) != 0 || parse_i64(f, &r->lot_qty) != 0) return -1;
    if (next_field(&cur, r->board_code, sizeof(r->board_code)) != 0) return -1;

    return cur == NULL ? 0 : -1;
}

static int load_scmktd(const char *path, scmktd_table_t *t)
{
    FILE *fp;
    char line[LINE_MAX_LEN];

    fp = fopen(path, "r");
    if (fp == NULL) {
        return -1;
    }

    t->used = 0U;
    while (fgets(line, sizeof(line), fp) != NULL) {
        if (t->used >= TABLE_MAX_REC) {
            fclose(fp);
            return -1;
        }
        if (parse_scmktd_line(line, &t->rows[t->used]) != 0) {
            fclose(fp);
            return -1;
        }
        t->used++;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int load_scinstf(const char *path, scinstf_table_t *t)
{
    FILE *fp;
    char line[LINE_MAX_LEN];

    fp = fopen(path, "r");
    if (fp == NULL) {
        return -1;
    }

    t->used = 0U;
    while (fgets(line, sizeof(line), fp) != NULL) {
        if (t->used >= TABLE_MAX_REC) {
            fclose(fp);
            return -1;
        }
        if (parse_scinstf_line(line, &t->rows[t->used]) != 0) {
            fclose(fp);
            return -1;
        }
        t->used++;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static const scmktd_rec_t *find_mkt(const scmktd_table_t *t, const char *instr_code)
{
    size_t i;

    for (i = 0U; i < t->used; i++) {
        if (strcmp(t->rows[i].instr_code, instr_code) == 0) {
            return &t->rows[i];
        }
    }

    return NULL;
}

static const scinstf_rec_t *find_inst(const scinstf_table_t *t, const char *instr_code)
{
    size_t i;

    for (i = 0U; i < t->used; i++) {
        if (strcmp(t->rows[i].instr_code, instr_code) == 0) {
            return &t->rows[i];
        }
    }

    return NULL;
}

static int valid_code_set(const scordf_rec_t *o, const scinstf_rec_t *i)
{
    if ((strcmp(o->side_kbn, "B") != 0) && (strcmp(o->side_kbn, "S") != 0)) {
        return 0;
    }
    if ((strcmp(o->ord_type, "L") != 0) && (strcmp(o->ord_type, "M") != 0)) {
        return 0;
    }
    if ((strcmp(o->tif_code, "DAY") != 0) &&
        (strcmp(o->tif_code, "IOC") != 0) &&
        (strcmp(o->tif_code, "FOK") != 0)) {
        return 0;
    }
    if ((strcmp(i->board_code, "T1") != 0) &&
        (strcmp(i->board_code, "ST") != 0) &&
        (strcmp(i->board_code, "ETF") != 0)) {
        return 0;
    }
    if (i->instr_tier < 1 || i->instr_tier > 3) {
        return 0;
    }
    return 1;
}

static int64_t tier_limit(int tier)
{
    if (tier == 1) {
        return MIHFT_MAX_NOTIONAL;
    }
    if (tier == 2) {
        return MIHFT_MAX_NOTIONAL / 2;
    }
    if (tier == 3) {
        return MIHFT_MAX_NOTIONAL / 4;
    }
    return 0;
}

static int64_t tier_margin_bp(int tier)
{
    if (tier == 1) {
        return 1000;
    }
    if (tier == 2) {
        return 2000;
    }
    if (tier == 3) {
        return 4000;
    }
    return 0;
}

static int checked_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a < 0 || b < 0) {
        return -1;
    }
    if (a != 0 && b > INT64_MAX / a) {
        return -1;
    }
    *out = a * b;
    return 0;
}

static int checked_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if (b > 0 && a > INT64_MAX - b) {
        return -1;
    }
    if (b < 0 && a < INT64_MIN - b) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int eval_price(const scordf_rec_t *o, const scmktd_rec_t *m, int64_t *price)
{
    if (strcmp(o->ord_type, "M") == 0 || o->price_amt == 0) {
        if (strcmp(o->side_kbn, "B") == 0) {
            *price = m->ask_amt;
        } else {
            *price = m->bid_amt;
        }
    } else {
        *price = o->price_amt;
    }

    return *price > 0 ? 0 : -1;
}

static int tick_ok(int64_t price, const scinstf_rec_t *i)
{
    if (i->tick_amt <= 0) {
        return 0;
    }
    return (price % i->tick_amt) == 0;
}

static int lot_ok(int64_t qty, const scinstf_rec_t *i)
{
    if (i->lot_qty <= 0) {
        return 0;
    }
    return qty > 0 && (qty % i->lot_qty) == 0;
}

static int now_ts(char *buf, size_t bufsz)
{
    time_t now;
    struct tm tmv;

    now = time(NULL);
    if (now == (time_t)-1) {
        return -1;
    }
#if defined(_POSIX_VERSION)
    if (localtime_r(&now, &tmv) == NULL) {
        return -1;
    }
#else
    {
        struct tm *p = localtime(&now);
        if (p == NULL) {
            return -1;
        }
        tmv = *p;
    }
#endif
    return strftime(buf, bufsz, "%Y%m%d%H%M%S", &tmv) > 0U ? 0 : -1;
}

static int write_decision(FILE *fp, long seq, const scordf_rec_t *o,
                          int decision, const char *reason,
                          int64_t notional, int64_t limit_used,
                          const char *ts)
{
    if (fprintf(fp, "D%012ld,%s,%s,%s,%d,%s,%lld,%lld,%s\n",
                seq,
                o->order_id,
                o->cif_no,
                o->instr_code,
                decision,
                reason,
                (long long)notional,
                (long long)limit_used,
                ts) < 0) {
        return -1;
    }
    return 0;
}

static int write_reject(FILE *fp, long seq, const scordf_rec_t *o,
                        const char *reject_cd, const char *detail_cd,
                        const char *ts)
{
    if (fprintf(fp, "R%012ld,%s,%s,%s,%s,%s,%s\n",
                seq,
                o->order_id,
                o->cif_no,
                o->instr_code,
                reject_cd,
                detail_cd,
                ts) < 0) {
        return -1;
    }
    return 0;
}

static int lightweight_margin_probe(int64_t notional, int tier)
{
    int64_t bp;
    int64_t margin;

    bp = tier_margin_bp(tier);
    if (bp <= 0) {
        return DEC_REJECT_MARGIN;
    }
    if (checked_mul_i64(notional, bp, &margin) != 0) {
        return DEC_REJECT_MARGIN;
    }
    margin /= 10000;

    return margin <= MIHFT_MAX_NOTIONAL ? DEC_ACCEPT : DEC_REJECT_MARGIN;
}

int main(void)
{
    scmktd_table_t mkt;
    scinstf_table_t inst;
    FILE *in;
    FILE *hfdec;
    FILE *hfrjct;
    char line[LINE_MAX_LEN];
    char ts[TS_MAX_LEN];
    long dec_seq = 1;
    long rjct_seq = 1;
    int final_rc = DEC_ACCEPT;

    if (load_scmktd("SCMKTD.csv", &mkt) != 0) {
        return 91;
    }
    if (load_scinstf("SCINSTF.csv", &inst) != 0) {
        return 92;
    }

    in = fopen("SCORDF.csv", "r");
    if (in == NULL) {
        return 93;
    }

    hfdec = fopen("HFDEC.dat", "a");
    if (hfdec == NULL) {
        fclose(in);
        return 94;
    }

    hfrjct = fopen("HFRJCT.dat", "a");
    if (hfrjct == NULL) {
        fclose(hfdec);
        fclose(in);
        return 95;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        scordf_rec_t ord;
        const scmktd_rec_t *m;
        const scinstf_rec_t *s;
        int64_t px = 0;
        int64_t notional = 0;
        int64_t used = 0;
        int64_t lim = 0;
        int decision = DEC_ACCEPT;
        const char *reason = "OK";
        order_t risk_order;
        cust_t risk_cust;

        if (parse_scordf_line(line, &ord) != 0) {
            final_rc = 96;
            break;
        }
        if (now_ts(ts, sizeof(ts)) != 0) {
            final_rc = 97;
            break;
        }

        m = find_mkt(&mkt, ord.instr_code);
        s = find_inst(&inst, ord.instr_code);

        memset(&risk_order, 0, sizeof(risk_order));
        memset(&risk_cust, 0, sizeof(risk_cust));

        if (m == NULL || s == NULL) {
            decision = DEC_REJECT_NOTIONAL;
            reason = "MASTER";
        } else if (!valid_code_set(&ord, s)) {
            decision = DEC_REJECT_TICK;
            reason = "CODE";
        } else if (eval_price(&ord, m, &px) != 0) {
            decision = DEC_REJECT_NOTIONAL;
            reason = "PRICE";
        } else if (!lot_ok(ord.ord_qty, s)) {
            decision = DEC_REJECT_TICK;
            reason = "LOT";
        } else if (!tick_ok(px, s)) {
            decision = DEC_REJECT_TICK;
            reason = "TICK";
        } else if (checked_mul_i64(px, ord.ord_qty, &notional) != 0 ||
                   checked_mul_i64(notional, 100, &notional) != 0) {
            decision = DEC_REJECT_NOTIONAL;
            reason = "OVF";
        } else {
            lim = tier_limit(s->instr_tier);
            if (lim <= 0 || checked_add_i64(used, notional, &used) != 0 || used > lim) {
                decision = DEC_REJECT_NOTIONAL;
                reason = "LIMIT";
            } else {
                int mr = lightweight_margin_probe(notional, s->instr_tier);
                if (mr != DEC_ACCEPT) {
                    decision = DEC_REJECT_MARGIN;
                    reason = "MARGIN";
                } else {
                    int rr = mihft_risk_eval(&risk_order, &risk_cust);
                    if (rr != DEC_ACCEPT) {
                        decision = rr;
                        reason = "RISK";
                    }
                }
            }
        }

        if (write_decision(hfdec, dec_seq++, &ord, decision, reason,
                           notional, used, ts) != 0) {
            final_rc = 98;
            break;
        }

        if (decision != DEC_ACCEPT) {
            const char *rej = decision == DEC_REJECT_MARGIN ? "RJ-MARGIN" :
                              decision == DEC_REJECT_TICK ? "RJ-TICK" : "RJ-NOTIONAL";
            if (write_reject(hfrjct, rjct_seq++, &ord, rej, reason, ts) != 0) {
                final_rc = 99;
                break;
            }
            if (final_rc == DEC_ACCEPT) {
                final_rc = decision;
            }
        }
    }

    if (ferror(in) && final_rc == DEC_ACCEPT) {
        final_rc = 96;
    }

    if (fclose(hfrjct) != 0 && final_rc == DEC_ACCEPT) {
        final_rc = 99;
    }
    if (fclose(hfdec) != 0 && final_rc == DEC_ACCEPT) {
        final_rc = 98;
    }
    if (fclose(in) != 0 && final_rc == DEC_ACCEPT) {
        final_rc = 93;
    }

    return final_rc;
}
