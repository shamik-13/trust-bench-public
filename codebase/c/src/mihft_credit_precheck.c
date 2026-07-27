/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20250603  渡辺 隆 (E-260)  初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_DECISION_ACCEPT 0
#define MIHFT_DECISION_REJECT_MARGIN 4
#define MIHFT_DECISION_REJECT_NOTIONAL 8
#define MIHFT_DECISION_REJECT_TICK 12

#define MIHFT_ERR_IO 20
#define MIHFT_ERR_PARSE 21
#define MIHFT_MAX_REC 4096
#define MIHFT_LINE_MAX 1024
#define MIHFT_ID_MAX 64
#define MIHFT_CODE_MAX 32
#define MIHFT_TS_MAX 32

typedef struct {
    char cif_no[MIHFT_ID_MAX];
    long long group_limit;
    long long group_used_amt;
    long long acct_used_amt;
} LocalCustomer;

typedef struct {
    char cif_no[MIHFT_ID_MAX];
    int instr_tier;
    long long max_notional_amt;
    long long max_order_qty;
    long long max_rate_cnt;
    char updated_ts[MIHFT_TS_MAX];
} LocalLimit;

typedef struct {
    char order_id[MIHFT_ID_MAX];
    char cif_no[MIHFT_ID_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    long long ord_qty;
    long long price_amt;
    int instr_tier;
} LocalOrder;

static void trim_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t n;

    if (dst_sz == 0 || src == NULL) {
        return 0;
    }
    n = strlen(src);
    if (n >= dst_sz) {
        return 0;
    }
    memcpy(dst, src, n + 1);
    return 1;
}

static int parse_ll(const char *s, long long *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
        return 0;
    }
    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0' || v < 0) {
        return 0;
    }
    *out = v;
    return 1;
}

static int parse_int(const char *s, int *out)
{
    long long v;

    if (!parse_ll(s, &v) || v > INT_MAX) {
        return 0;
    }
    *out = (int)v;
    return 1;
}

static int next_token(char **save, char **out)
{
    char *p = strtok_r(NULL, ",", save);

    if (p == NULL || *p == '\0') {
        return 0;
    }
    *out = p;
    return 1;
}

static int split_first(char *line, char **save, char **out)
{
    char *p = strtok_r(line, ",", save);

    if (p == NULL || *p == '\0') {
        return 0;
    }
    *out = p;
    return 1;
}

static int read_customers(LocalCustomer *rows, size_t *count)
{
    FILE *fp = fopen("SCCUST.csv", "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "SCCUST入力を開けません\n");
        return MIHFT_ERR_IO;
    }

    *count = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *save = NULL;
        char *f = NULL;
        LocalCustomer r;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (*count >= MIHFT_MAX_REC) {
            fclose(fp);
            fprintf(stderr, "SCCUST件数が上限を超過しました\n");
            return MIHFT_ERR_PARSE;
        }

        if (!split_first(line, &save, &f) || !copy_field(r.cif_no, sizeof(r.cif_no), f)) {
            fclose(fp);
            fprintf(stderr, "SCCUSTのCIF-NOが不正です\n");
            return MIHFT_ERR_PARSE;
        }
        if (!next_token(&save, &f) || !parse_ll(f, &r.group_limit) ||
            !next_token(&save, &f) || !parse_ll(f, &r.group_used_amt) ||
            !next_token(&save, &f) || !parse_ll(f, &r.acct_used_amt)) {
            fclose(fp);
            fprintf(stderr, "SCCUSTの金額項目が不正です\n");
            return MIHFT_ERR_PARSE;
        }
        rows[(*count)++] = r;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCCUST読込で障害が発生しました\n");
        return MIHFT_ERR_IO;
    }
    fclose(fp);
    return 0;
}

static int read_limits(LocalLimit *rows, size_t *count)
{
    FILE *fp = fopen("SCLMTF.csv", "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "SCLMTF入力を開けません\n");
        return MIHFT_ERR_IO;
    }

    *count = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *save = NULL;
        char *f = NULL;
        LocalLimit r;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (*count >= MIHFT_MAX_REC) {
            fclose(fp);
            fprintf(stderr, "SCLMTF件数が上限を超過しました\n");
            return MIHFT_ERR_PARSE;
        }

        if (!split_first(line, &save, &f) || !copy_field(r.cif_no, sizeof(r.cif_no), f) ||
            !next_token(&save, &f) || !parse_int(f, &r.instr_tier) ||
            !next_token(&save, &f) || !parse_ll(f, &r.max_notional_amt) ||
            !next_token(&save, &f) || !parse_ll(f, &r.max_order_qty) ||
            !next_token(&save, &f) || !parse_ll(f, &r.max_rate_cnt) ||
            !next_token(&save, &f) || !copy_field(r.updated_ts, sizeof(r.updated_ts), f)) {
            fclose(fp);
            fprintf(stderr, "SCLMTF項目が不正です\n");
            return MIHFT_ERR_PARSE;
        }
        rows[(*count)++] = r;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCLMTF読込で障害が発生しました\n");
        return MIHFT_ERR_IO;
    }
    fclose(fp);
    return 0;
}

static int parse_order_line(char *line, LocalOrder *r)
{
    char *save = NULL;
    char *f = NULL;

    if (!split_first(line, &save, &f) || !copy_field(r->order_id, sizeof(r->order_id), f) ||
        !next_token(&save, &f) || !copy_field(r->cif_no, sizeof(r->cif_no), f) ||
        !next_token(&save, &f) || !copy_field(r->instr_code, sizeof(r->instr_code), f) ||
        !next_token(&save, &f) || strlen(f) != 1 ||
        (f[0] != 'B' && f[0] != 'S')) {
        return 0;
    }
    r->side_kbn = f[0];

    if (!next_token(&save, &f) || strlen(f) != 1 ||
        (f[0] != 'L' && f[0] != 'M')) {
        return 0;
    }
    r->ord_type = f[0];

    if (!next_token(&save, &f) || !copy_field(r->tif_code, sizeof(r->tif_code), f) ||
        (strcmp(r->tif_code, "DAY") != 0 && strcmp(r->tif_code, "IOC") != 0 &&
         strcmp(r->tif_code, "FOK") != 0) ||
        !next_token(&save, &f) || !parse_ll(f, &r->ord_qty) ||
        !next_token(&save, &f) || !parse_ll(f, &r->price_amt) ||
        !next_token(&save, &f) || !parse_int(f, &r->instr_tier)) {
        return 0;
    }

    return r->ord_qty > 0 && r->price_amt > 0;
}

static const LocalCustomer *find_customer(const LocalCustomer *rows, size_t count, const char *cif_no)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(rows[i].cif_no, cif_no) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static const LocalLimit *find_limit(const LocalLimit *rows, size_t count, const char *cif_no, int tier)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (rows[i].instr_tier == tier && strcmp(rows[i].cif_no, cif_no) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static int tier_margin_bp(int tier)
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

static long long tier_tick(int tier)
{
    if (tier == 1) {
        return 100;
    }
    if (tier == 2) {
        return 500;
    }
    if (tier == 3) {
        return 1000;
    }
    return 0;
}

static int checked_add_ll(long long a, long long b, long long *out)
{
    if (b > LLONG_MAX - a) {
        return 0;
    }
    *out = a + b;
    return 1;
}

static int calc_notional(long long qty, long long price, long long *out)
{
    if (qty <= 0 || price <= 0 || qty > LLONG_MAX / price) {
        return 0;
    }
    *out = qty * price;
    return 1;
}

static int calc_margin(long long notional, int bp, long long *out)
{
    long long q;
    long long r;

    if (bp <= 0 || notional < 0) {
        return 0;
    }
    q = notional / 10000;
    r = notional % 10000;
    if (q > LLONG_MAX / bp) {
        return 0;
    }
    q *= bp;
    if (r > (LLONG_MAX - 9999) / bp) {
        return 0;
    }
    r = (r * bp + 9999) / 10000;
    return checked_add_ll(q, r, out);
}

static void make_ts(char *buf, size_t sz)
{
    time_t now = time(NULL);
    struct tm tmv;

    if (now == (time_t)-1 || localtime_r(&now, &tmv) == NULL) {
        snprintf(buf, sz, "00000000000000");
        return;
    }
    strftime(buf, sz, "%Y%m%d%H%M%S", &tmv);
}

static int write_reject(FILE *fp, long long id, const LocalOrder *o, int reject_cd,
                        const char *detail_cd, const char *ts)
{
    if (fprintf(fp, "RJ%012lld,%s,%s,%s,%d,%s,%s\n",
                id, o->order_id, o->cif_no, o->instr_code, reject_cd, detail_cd, ts) < 0) {
        fprintf(stderr, "HFRJCT書込で障害が発生しました\n");
        return MIHFT_ERR_IO;
    }
    return 0;
}

static int write_decision(FILE *fp, long long id, const LocalOrder *o, int decision_cd,
                          const char *reason_cd, long long notional, long long used_amt,
                          const char *ts)
{
    if (fprintf(fp, "DC%012lld,%s,%s,%s,%d,%s,%lld,%lld,%s\n",
                id, o->order_id, o->cif_no, o->instr_code, decision_cd, reason_cd,
                notional, used_amt, ts) < 0) {
        fprintf(stderr, "HFDEC書込で障害が発生しました\n");
        return MIHFT_ERR_IO;
    }
    return 0;
}

int main(void)
{
    LocalCustomer customers[MIHFT_MAX_REC];
    LocalLimit limits[MIHFT_MAX_REC];
    size_t customer_count = 0;
    size_t limit_count = 0;
    FILE *ord_fp = NULL;
    FILE *dec_fp = NULL;
    FILE *rj_fp = NULL;
    char line[MIHFT_LINE_MAX];
    long long decision_seq = 1;
    long long reject_seq = 1;
    int final_rc = MIHFT_DECISION_ACCEPT;
    int rc;

    rc = read_customers(customers, &customer_count);
    if (rc != 0) {
        return rc;
    }
    rc = read_limits(limits, &limit_count);
    if (rc != 0) {
        return rc;
    }

    ord_fp = fopen("SCORDF.csv", "r");
    if (ord_fp == NULL) {
        fprintf(stderr, "SCORDF入力を開けません\n");
        return MIHFT_ERR_IO;
    }
    dec_fp = fopen("HFDEC.csv", "w");
    if (dec_fp == NULL) {
        fclose(ord_fp);
        fprintf(stderr, "HFDEC出力を開けません\n");
        return MIHFT_ERR_IO;
    }
    rj_fp = fopen("HFRJCT.csv", "w");
    if (rj_fp == NULL) {
        fclose(dec_fp);
        fclose(ord_fp);
        fprintf(stderr, "HFRJCT出力を開けません\n");
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), ord_fp) != NULL) {
        LocalOrder o;
        const LocalCustomer *cust;
        const LocalLimit *lim;
        char ts[MIHFT_TS_MAX];
        const char *reason = "OK";
        long long notional = 0;
        long long margin = 0;
        long long post_group = 0;
        long long post_acct = 0;
        int decision_cd = MIHFT_DECISION_ACCEPT;
        int bp;
        long long tick;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (!parse_order_line(line, &o)) {
            fprintf(stderr, "SCORDF項目が不正です\n");
            fclose(rj_fp);
            fclose(dec_fp);
            fclose(ord_fp);
            return MIHFT_ERR_PARSE;
        }

        make_ts(ts, sizeof(ts));
        cust = find_customer(customers, customer_count, o.cif_no);
        lim = find_limit(limits, limit_count, o.cif_no, o.instr_tier);
        bp = tier_margin_bp(o.instr_tier);
        tick = tier_tick(o.instr_tier);

        if (cust == NULL || lim == NULL || bp == 0 || tick == 0) {
            decision_cd = MIHFT_DECISION_REJECT_MARGIN;
            reason = "MASTER";
        } else if (!calc_notional(o.ord_qty, o.price_amt, &notional)) {
            decision_cd = MIHFT_DECISION_REJECT_NOTIONAL;
            reason = "AMTOVF";
        } else if (o.price_amt % tick != 0) {
            decision_cd = MIHFT_DECISION_REJECT_TICK;
            reason = "TICK";
        } else if (notional > MIHFT_MAX_NOTIONAL || notional > lim->max_notional_amt ||
                   o.ord_qty > lim->max_order_qty) {
            decision_cd = MIHFT_DECISION_REJECT_NOTIONAL;
            reason = "TIER";
        } else if (!calc_margin(notional, bp, &margin) ||
                   !checked_add_ll(cust->group_used_amt, margin, &post_group) ||
                   !checked_add_ll(cust->acct_used_amt, margin, &post_acct)) {
            decision_cd = MIHFT_DECISION_REJECT_MARGIN;
            reason = "AMTOVF";
        } else if (post_group > cust->group_limit) {
            decision_cd = MIHFT_DECISION_REJECT_MARGIN;
            reason = "GROUP";
        } else if (post_acct > cust->group_limit) {
            decision_cd = MIHFT_DECISION_REJECT_MARGIN;
            reason = "ACCT";
        } else {
            reason = "ACCEPT";
        }

        rc = write_decision(dec_fp, decision_seq++, &o, decision_cd, reason,
                            notional, margin, ts);
        if (rc != 0) {
            fclose(rj_fp);
            fclose(dec_fp);
            fclose(ord_fp);
            return rc;
        }

        if (decision_cd != MIHFT_DECISION_ACCEPT) {
            rc = write_reject(rj_fp, reject_seq++, &o, decision_cd, reason, ts);
            if (rc != 0) {
                fclose(rj_fp);
                fclose(dec_fp);
                fclose(ord_fp);
                return rc;
            }
            final_rc = decision_cd;
        }
    }

    if (ferror(ord_fp)) {
        fclose(rj_fp);
        fclose(dec_fp);
        fclose(ord_fp);
        fprintf(stderr, "SCORDF読込で障害が発生しました\n");
        return MIHFT_ERR_IO;
    }
    if (fclose(rj_fp) != 0 || fclose(dec_fp) != 0 || fclose(ord_fp) != 0) {
        fprintf(stderr, "入出力終了処理で障害が発生しました\n");
        return MIHFT_ERR_IO;
    }

    return final_rc;
}
