/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220222  大野 修 (E-225)  ポジション報告フィード初版作成
 * 1.01  20220722  三宅 拓也 (E-241)  CSV境界検査と金額桁あふれ検査を追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_RET_ACCEPT          0
#define MIHFT_RET_REJECT_MARGIN   4
#define MIHFT_RET_REJECT_NOTIONAL 8
#define MIHFT_RET_REJECT_TICK     12
#define MIHFT_RET_IOERR           16
#define MIHFT_RET_PARSEERR        20

#define MIHFT_MAX_LINE 512
#define MIHFT_MAX_REC  4096
#define MIHFT_KEY_LEN  64
#define MIHFT_DATE_LEN 16
#define MIHFT_TS_LEN   32

typedef struct {
    char cif_no[MIHFT_KEY_LEN];
    char instr_code[MIHFT_KEY_LEN];
    long long net_qty;
    long long avg_amt;
    long long rlzd_amt;
} scposf_rec;

typedef struct {
    char cif_no[MIHFT_KEY_LEN];
    char instr_code[MIHFT_KEY_LEN];
    char sess_dt[MIHFT_DATE_LEN];
    long long net_qty;
    long long mark_amt;
    long long mark_notional_amt;
    long long unrlzd_amt;
} scm2mf_rec;

typedef struct {
    char cif_no[MIHFT_KEY_LEN];
    char instr_code[MIHFT_KEY_LEN];
    char sess_dt[MIHFT_DATE_LEN];
    long long rlzd_amt;
    long long unrlzd_amt;
    long long fee_amt;
    char calc_ts[MIHFT_TS_LEN];
} scpnlf_rec;

typedef struct {
    scposf_rec pos[MIHFT_MAX_REC];
    scm2mf_rec mark[MIHFT_MAX_REC];
    scpnlf_rec pnl[MIHFT_MAX_REC];
    size_t pos_cnt;
    size_t mark_cnt;
    size_t pnl_cnt;
} feed_store;

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dst_len) {
        return MIHFT_RET_PARSEERR;
    }
    memcpy(dst, src, n + 1);
    return MIHFT_RET_ACCEPT;
}

static int parse_i64(const char *s, long long *out)
{
    char *endp;
    long long v;

    if (*s == '\0') {
        return MIHFT_RET_PARSEERR;
    }
    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno == ERANGE || *endp != '\0') {
        return MIHFT_RET_PARSEERR;
    }
    *out = v;
    return MIHFT_RET_ACCEPT;
}

static int split_csv(char *line, char **col, size_t need)
{
    size_t i = 0;
    char *p = line;

    while (i < need) {
        char *comma = strchr(p, ',');
        if (comma != NULL) {
            *comma = '\0';
        } else if (i + 1 != need) {
            return MIHFT_RET_PARSEERR;
        }

        col[i++] = p;
        if (comma == NULL) {
            break;
        }
        p = comma + 1;
    }

    if (i != need || strchr(col[need - 1], ',') != NULL) {
        return MIHFT_RET_PARSEERR;
    }

    return MIHFT_RET_ACCEPT;
}

static void trim_eol(char *line)
{
    size_t n = strlen(line);

    while (n > 0 && (line[n - 1] == '\n' || line[n - 1] == '\r')) {
        line[--n] = '\0';
    }
}

static int same_key2(const char *cif_no, const char *instr_code,
                     const char *cif_no2, const char *instr_code2)
{
    return strcmp(cif_no, cif_no2) == 0 && strcmp(instr_code, instr_code2) == 0;
}

static int read_scposf(feed_store *st)
{
    FILE *fp = fopen("SCPOSF.csv", "r");
    char line[MIHFT_MAX_LINE];

    if (fp == NULL) {
        fprintf(stderr, "SCPOSF読込不能\n");
        return MIHFT_RET_IOERR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *col[5];
        scposf_rec *r;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (st->pos_cnt >= MIHFT_MAX_REC) {
            fclose(fp);
            fprintf(stderr, "SCPOSF件数超過\n");
            return MIHFT_RET_PARSEERR;
        }
        if (split_csv(line, col, 5) != MIHFT_RET_ACCEPT) {
            fclose(fp);
            fprintf(stderr, "SCPOSF項目不正\n");
            return MIHFT_RET_PARSEERR;
        }

        r = &st->pos[st->pos_cnt];
        if (copy_field(r->cif_no, sizeof(r->cif_no), col[0]) != MIHFT_RET_ACCEPT ||
            copy_field(r->instr_code, sizeof(r->instr_code), col[1]) != MIHFT_RET_ACCEPT ||
            parse_i64(col[2], &r->net_qty) != MIHFT_RET_ACCEPT ||
            parse_i64(col[3], &r->avg_amt) != MIHFT_RET_ACCEPT ||
            parse_i64(col[4], &r->rlzd_amt) != MIHFT_RET_ACCEPT) {
            fclose(fp);
            fprintf(stderr, "SCPOSF数値不正\n");
            return MIHFT_RET_PARSEERR;
        }
        st->pos_cnt++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCPOSF読込中断\n");
        return MIHFT_RET_IOERR;
    }
    fclose(fp);
    return MIHFT_RET_ACCEPT;
}

static int read_scm2mf(feed_store *st)
{
    FILE *fp = fopen("SCM2MF.csv", "r");
    char line[MIHFT_MAX_LINE];

    if (fp == NULL) {
        fprintf(stderr, "SCM2MF読込不能\n");
        return MIHFT_RET_IOERR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *col[7];
        scm2mf_rec *r;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (st->mark_cnt >= MIHFT_MAX_REC) {
            fclose(fp);
            fprintf(stderr, "SCM2MF件数超過\n");
            return MIHFT_RET_PARSEERR;
        }
        if (split_csv(line, col, 7) != MIHFT_RET_ACCEPT) {
            fclose(fp);
            fprintf(stderr, "SCM2MF項目不正\n");
            return MIHFT_RET_PARSEERR;
        }

        r = &st->mark[st->mark_cnt];
        if (copy_field(r->cif_no, sizeof(r->cif_no), col[0]) != MIHFT_RET_ACCEPT ||
            copy_field(r->instr_code, sizeof(r->instr_code), col[1]) != MIHFT_RET_ACCEPT ||
            copy_field(r->sess_dt, sizeof(r->sess_dt), col[2]) != MIHFT_RET_ACCEPT ||
            parse_i64(col[3], &r->net_qty) != MIHFT_RET_ACCEPT ||
            parse_i64(col[4], &r->mark_amt) != MIHFT_RET_ACCEPT ||
            parse_i64(col[5], &r->mark_notional_amt) != MIHFT_RET_ACCEPT ||
            parse_i64(col[6], &r->unrlzd_amt) != MIHFT_RET_ACCEPT) {
            fclose(fp);
            fprintf(stderr, "SCM2MF数値不正\n");
            return MIHFT_RET_PARSEERR;
        }
        st->mark_cnt++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCM2MF読込中断\n");
        return MIHFT_RET_IOERR;
    }
    fclose(fp);
    return MIHFT_RET_ACCEPT;
}

static int read_scpnlf(feed_store *st)
{
    FILE *fp = fopen("SCPNLF.csv", "r");
    char line[MIHFT_MAX_LINE];

    if (fp == NULL) {
        fprintf(stderr, "SCPNLF読込不能\n");
        return MIHFT_RET_IOERR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *col[7];
        scpnlf_rec *r;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (st->pnl_cnt >= MIHFT_MAX_REC) {
            fclose(fp);
            fprintf(stderr, "SCPNLF件数超過\n");
            return MIHFT_RET_PARSEERR;
        }
        if (split_csv(line, col, 7) != MIHFT_RET_ACCEPT) {
            fclose(fp);
            fprintf(stderr, "SCPNLF項目不正\n");
            return MIHFT_RET_PARSEERR;
        }

        r = &st->pnl[st->pnl_cnt];
        if (copy_field(r->cif_no, sizeof(r->cif_no), col[0]) != MIHFT_RET_ACCEPT ||
            copy_field(r->instr_code, sizeof(r->instr_code), col[1]) != MIHFT_RET_ACCEPT ||
            copy_field(r->sess_dt, sizeof(r->sess_dt), col[2]) != MIHFT_RET_ACCEPT ||
            parse_i64(col[3], &r->rlzd_amt) != MIHFT_RET_ACCEPT ||
            parse_i64(col[4], &r->unrlzd_amt) != MIHFT_RET_ACCEPT ||
            parse_i64(col[5], &r->fee_amt) != MIHFT_RET_ACCEPT ||
            copy_field(r->calc_ts, sizeof(r->calc_ts), col[6]) != MIHFT_RET_ACCEPT) {
            fclose(fp);
            fprintf(stderr, "SCPNLF数値不正\n");
            return MIHFT_RET_PARSEERR;
        }
        st->pnl_cnt++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCPNLF読込中断\n");
        return MIHFT_RET_IOERR;
    }
    fclose(fp);
    return MIHFT_RET_ACCEPT;
}

static int instr_tier(const char *instr_code)
{
    unsigned char c = (unsigned char)instr_code[0];

    if (c == '1') {
        return 1;
    }
    if (c == '2' || c == '3') {
        return 2;
    }
    return 3;
}

static long long tier_margin_bp(int tier)
{
    if (tier == 1) {
        return 1000;
    }
    if (tier == 2) {
        return 2000;
    }
    return 4000;
}

static long long tier_tick(int tier)
{
    if (tier == 1) {
        return 100;
    }
    if (tier == 2) {
        return 500;
    }
    return 1000;
}

static const scm2mf_rec *find_latest_mark(const feed_store *st, const scposf_rec *pos)
{
    const scm2mf_rec *best = NULL;
    size_t i;

    for (i = 0; i < st->mark_cnt; i++) {
        const scm2mf_rec *r = &st->mark[i];

        if (same_key2(pos->cif_no, pos->instr_code, r->cif_no, r->instr_code) &&
            (best == NULL || strcmp(best->sess_dt, r->sess_dt) <= 0)) {
            best = r;
        }
    }
    return best;
}

static const scpnlf_rec *find_latest_pnl(const feed_store *st, const scposf_rec *pos)
{
    const scpnlf_rec *best = NULL;
    size_t i;

    for (i = 0; i < st->pnl_cnt; i++) {
        const scpnlf_rec *r = &st->pnl[i];

        if (same_key2(pos->cif_no, pos->instr_code, r->cif_no, r->instr_code) &&
            (best == NULL ||
             strcmp(best->sess_dt, r->sess_dt) < 0 ||
             (strcmp(best->sess_dt, r->sess_dt) == 0 && strcmp(best->calc_ts, r->calc_ts) <= 0))) {
            best = r;
        }
    }
    return best;
}

static int abs_i64(long long v, long long *out)
{
    if (v == LLONG_MIN) {
        return MIHFT_RET_PARSEERR;
    }
    *out = v < 0 ? -v : v;
    return MIHFT_RET_ACCEPT;
}

static int add_i64(long long a, long long b, long long *out)
{
    if ((b > 0 && a > LLONG_MAX - b) || (b < 0 && a < LLONG_MIN - b)) {
        return MIHFT_RET_PARSEERR;
    }
    *out = a + b;
    return MIHFT_RET_ACCEPT;
}

static int evaluate_feed(const feed_store *st)
{
    int decision = MIHFT_RET_ACCEPT;
    size_t i;

    for (i = 0; i < st->pos_cnt; i++) {
        const scposf_rec *pos = &st->pos[i];
        const scm2mf_rec *mark = find_latest_mark(st, pos);
        const scpnlf_rec *pnl = find_latest_pnl(st, pos);
        long long notional_abs;
        long long net_pnl;
        long long need_margin;
        int tier;

        if (mark == NULL || pnl == NULL) {
            fprintf(stderr, "完全な報告セットなし\n");
            return MIHFT_RET_PARSEERR;
        }

        tier = instr_tier(pos->instr_code);

        if (mark->mark_amt <= 0 || mark->mark_amt % tier_tick(tier) != 0) {
            decision = MIHFT_RET_REJECT_TICK;
        }

        if (abs_i64(mark->mark_notional_amt, &notional_abs) != MIHFT_RET_ACCEPT) {
            fprintf(stderr, "時価想定元本あふれ\n");
            return MIHFT_RET_PARSEERR;
        }

        if (notional_abs > MIHFT_MAX_NOTIONAL) {
            decision = MIHFT_RET_REJECT_NOTIONAL;
        }

        if (add_i64(pnl->rlzd_amt, pnl->unrlzd_amt, &net_pnl) != MIHFT_RET_ACCEPT ||
            add_i64(net_pnl, -pnl->fee_amt, &net_pnl) != MIHFT_RET_ACCEPT) {
            fprintf(stderr, "損益合算あふれ\n");
            return MIHFT_RET_PARSEERR;
        }

        need_margin = (notional_abs / 10000) * tier_margin_bp(tier);
        if (net_pnl < 0 && -net_pnl > need_margin) {
            decision = MIHFT_RET_REJECT_MARGIN;
        }

        printf("%s,%s,%s,%lld,%lld,%lld,%lld,%lld,%s\n",
               pos->cif_no,
               pos->instr_code,
               mark->sess_dt,
               mark->net_qty,
               mark->mark_amt,
               mark->mark_notional_amt,
               pnl->rlzd_amt,
               pnl->unrlzd_amt,
               pnl->calc_ts);
    }

    return decision;
}

int main(void)
{
    feed_store st;
    int rc;

    memset(&st, 0, sizeof(st));

    rc = read_scposf(&st);
    if (rc != MIHFT_RET_ACCEPT) {
        return rc;
    }

    rc = read_scm2mf(&st);
    if (rc != MIHFT_RET_ACCEPT) {
        return rc;
    }

    rc = read_scpnlf(&st);
    if (rc != MIHFT_RET_ACCEPT) {
        return rc;
    }

    return evaluate_feed(&st);
}
