/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20190416  篠原 健 (E-203)     初版作成
 * 1.01  20190916  藤田 和也 (E-271)     CSV検査および桁あふれ検査を追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_RC_ACCEPT          0
#define MIHFT_RC_REJECT_MARGIN   4
#define MIHFT_RC_REJECT_NOTIONAL 8
#define MIHFT_RC_REJECT_TICK     12
#define MIHFT_RC_IOERR           16
#define MIHFT_RC_PARSEERR        20

#define MIHFT_MAX_CUST    4096
#define MIHFT_MAX_POS     16384
#define MIHFT_MAX_INST    4096
#define MIHFT_LINE_MAX    1024
#define MIHFT_KEY_MAX     32
#define MIHFT_NAME_MAX    96

typedef struct {
    char cif_no[MIHFT_KEY_MAX];
    int64_t group_limit;
    int64_t group_used_amt;
    int64_t acct_used_amt;
} MihftCustRec;

typedef struct {
    char cif_no[MIHFT_KEY_MAX];
    char instr_code[MIHFT_KEY_MAX];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
} MihftPosRec;

typedef struct {
    char instr_code[MIHFT_KEY_MAX];
    char instr_name[MIHFT_NAME_MAX];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[MIHFT_KEY_MAX];
} MihftInstRec;

typedef struct {
    char order_id[MIHFT_KEY_MAX];
    char cif_no[MIHFT_KEY_MAX];
    char instr_code[MIHFT_KEY_MAX];
    char side_kbn;
    int64_t ord_qty;
    int64_t price_amt;
    char ord_type;
    char tif_code[MIHFT_KEY_MAX];
} MihftOrderRec;

static MihftCustRec g_cust[MIHFT_MAX_CUST];
static MihftPosRec g_pos[MIHFT_MAX_POS];
static MihftInstRec g_inst[MIHFT_MAX_INST];
static size_t g_cust_cnt;
static size_t g_pos_cnt;
static size_t g_inst_cnt;

static void strip_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);
    if (n == 0 || n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
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

static int split_csv(char *line, char **field, size_t need)
{
    size_t i = 0;
    char *p = line;

    while (i < need) {
        field[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return i == need && strchr(field[need - 1], ',') == NULL ? 0 : -1;
}

static int mul_i64_checked(int64_t a, int64_t b, int64_t *out)
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

static int add_i64_checked(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int margin_rate_bp(int tier, int64_t *rate_bp)
{
    if (tier == 1) {
        *rate_bp = 1000;
        return 0;
    }
    if (tier == 2) {
        *rate_bp = 2000;
        return 0;
    }
    if (tier == 3) {
        *rate_bp = 4000;
        return 0;
    }
    return -1;
}

static MihftCustRec *find_cust(const char *cif_no)
{
    size_t i;
    for (i = 0; i < g_cust_cnt; i++) {
        if (strcmp(g_cust[i].cif_no, cif_no) == 0) {
            return &g_cust[i];
        }
    }
    return NULL;
}

static MihftInstRec *find_inst(const char *instr_code)
{
    size_t i;
    for (i = 0; i < g_inst_cnt; i++) {
        if (strcmp(g_inst[i].instr_code, instr_code) == 0) {
            return &g_inst[i];
        }
    }
    return NULL;
}

static MihftPosRec *find_pos(const char *cif_no, const char *instr_code)
{
    size_t i;
    for (i = 0; i < g_pos_cnt; i++) {
        if (strcmp(g_pos[i].cif_no, cif_no) == 0 &&
            strcmp(g_pos[i].instr_code, instr_code) == 0) {
            return &g_pos[i];
        }
    }
    return NULL;
}

static int read_sccust(const char *path)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        return -1;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[4];
        MihftCustRec r;

        strip_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (g_cust_cnt >= MIHFT_MAX_CUST || split_csv(line, f, 4) != 0) {
            fclose(fp);
            return -1;
        }
        if (copy_field(r.cif_no, sizeof(r.cif_no), f[0]) != 0 ||
            parse_i64(f[1], &r.group_limit) != 0 ||
            parse_i64(f[2], &r.group_used_amt) != 0 ||
            parse_i64(f[3], &r.acct_used_amt) != 0 ||
            r.group_limit < 0 || r.group_used_amt < 0 || r.acct_used_amt < 0) {
            fclose(fp);
            return -1;
        }
        g_cust[g_cust_cnt++] = r;
    }
    return ferror(fp) ? (fclose(fp), -1) : fclose(fp);
}

static int read_scposf(const char *path)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        return -1;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[5];
        MihftPosRec r;

        strip_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (g_pos_cnt >= MIHFT_MAX_POS || split_csv(line, f, 5) != 0) {
            fclose(fp);
            return -1;
        }
        if (copy_field(r.cif_no, sizeof(r.cif_no), f[0]) != 0 ||
            copy_field(r.instr_code, sizeof(r.instr_code), f[1]) != 0 ||
            parse_i64(f[2], &r.net_qty) != 0 ||
            parse_i64(f[3], &r.avg_amt) != 0 ||
            parse_i64(f[4], &r.rlzd_amt) != 0 ||
            r.avg_amt < 0) {
            fclose(fp);
            return -1;
        }
        g_pos[g_pos_cnt++] = r;
    }
    return ferror(fp) ? (fclose(fp), -1) : fclose(fp);
}

static int read_scinstf(const char *path)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        return -1;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];
        MihftInstRec r;
        int64_t tier64;

        strip_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (g_inst_cnt >= MIHFT_MAX_INST || split_csv(line, f, 6) != 0) {
            fclose(fp);
            return -1;
        }
        if (copy_field(r.instr_code, sizeof(r.instr_code), f[0]) != 0 ||
            copy_field(r.instr_name, sizeof(r.instr_name), f[1]) != 0 ||
            parse_i64(f[2], &tier64) != 0 ||
            parse_i64(f[3], &r.tick_amt) != 0 ||
            parse_i64(f[4], &r.lot_qty) != 0 ||
            copy_field(r.board_code, sizeof(r.board_code), f[5]) != 0 ||
            tier64 < 1 || tier64 > 3 || r.tick_amt <= 0 || r.lot_qty <= 0) {
            fclose(fp);
            return -1;
        }
        r.instr_tier = (int)tier64;
        g_inst[g_inst_cnt++] = r;
    }
    return ferror(fp) ? (fclose(fp), -1) : fclose(fp);
}

static int parse_order(char *line, MihftOrderRec *o)
{
    char *f[8];

    strip_eol(line);
    if (split_csv(line, f, 8) != 0) {
        return -1;
    }
    if (copy_field(o->order_id, sizeof(o->order_id), f[0]) != 0 ||
        copy_field(o->cif_no, sizeof(o->cif_no), f[1]) != 0 ||
        copy_field(o->instr_code, sizeof(o->instr_code), f[2]) != 0 ||
        strlen(f[3]) != 1 ||
        parse_i64(f[4], &o->ord_qty) != 0 ||
        parse_i64(f[5], &o->price_amt) != 0 ||
        strlen(f[6]) != 1 ||
        copy_field(o->tif_code, sizeof(o->tif_code), f[7]) != 0) {
        return -1;
    }
    o->side_kbn = f[3][0];
    o->ord_type = f[6][0];
    if ((o->side_kbn != 'B' && o->side_kbn != 'S') ||
        (o->ord_type != 'L' && o->ord_type != 'M') ||
        o->ord_qty <= 0 || o->price_amt <= 0) {
        return -1;
    }
    return 0;
}

static int calc_credit_exposure(const MihftOrderRec *o, const MihftPosRec *p,
                                int64_t projected_notional, int64_t *exposure)
{
    int64_t cover_qty = 0;
    int64_t covered_notional = 0;

    if (o->side_kbn == 'S' && p != NULL && p->net_qty > 0) {
        cover_qty = p->net_qty < o->ord_qty ? p->net_qty : o->ord_qty;
    } else if (o->side_kbn == 'B' && p != NULL && p->net_qty < 0) {
        int64_t short_qty = p->net_qty == INT64_MIN ? INT64_MAX : -p->net_qty;
        cover_qty = short_qty < o->ord_qty ? short_qty : o->ord_qty;
    }
    if (mul_i64_checked(cover_qty, o->price_amt, &covered_notional) != 0) {
        return -1;
    }
    *exposure = projected_notional - covered_notional;
    return *exposure < 0 ? -1 : 0;
}

static int reject_code(const MihftOrderRec *o, const MihftCustRec *c,
                       const MihftInstRec *i, int64_t *reject_amt)
{
    MihftPosRec *p = find_pos(o->cif_no, o->instr_code);
    int64_t projected_notional;
    int64_t exposure;
    int64_t rate_bp;
    int64_t margin_amt;
    int64_t group_after;
    int64_t acct_after;

    *reject_amt = 0;
    if (mul_i64_checked(o->ord_qty, o->price_amt, &projected_notional) != 0) {
        return MIHFT_RC_REJECT_NOTIONAL;
    }
    if (projected_notional > MIHFT_MAX_NOTIONAL) {
        *reject_amt = projected_notional;
        return MIHFT_RC_REJECT_NOTIONAL;
    }
    if (o->ord_type == 'L' && o->price_amt % i->tick_amt != 0) {
        *reject_amt = o->price_amt;
        return MIHFT_RC_REJECT_TICK;
    }
    if (o->ord_qty % i->lot_qty != 0) {
        *reject_amt = o->ord_qty;
        return MIHFT_RC_REJECT_TICK;
    }
    if (margin_rate_bp(i->instr_tier, &rate_bp) != 0 ||
        calc_credit_exposure(o, p, projected_notional, &exposure) != 0 ||
        mul_i64_checked(exposure, rate_bp, &margin_amt) != 0) {
        return MIHFT_RC_REJECT_MARGIN;
    }
    margin_amt = (margin_amt + 9999) / 10000;
    if (add_i64_checked(c->group_used_amt, margin_amt, &group_after) != 0 ||
        add_i64_checked(c->acct_used_amt, margin_amt, &acct_after) != 0) {
        return MIHFT_RC_REJECT_MARGIN;
    }
    if (group_after > c->group_limit || acct_after > c->group_limit) {
        *reject_amt = margin_amt;
        return MIHFT_RC_REJECT_MARGIN;
    }
    return MIHFT_RC_ACCEPT;
}

static int write_reject(FILE *fp, int64_t reject_id, const MihftOrderRec *o, int code)
{
    time_t now = time(NULL);
    struct tm *tmv = localtime(&now);
    char ts[32];

    if (tmv == NULL) {
        return -1;
    }
    if (strftime(ts, sizeof(ts), "%Y%m%d%H%M%S", tmv) == 0) {
        return -1;
    }
    if (fprintf(fp, "%lld,%s,%s,%s,%d,%s\n",
                (long long)reject_id, o->order_id, o->cif_no,
                o->instr_code, code, ts) < 0) {
        return -1;
    }
    return 0;
}

int main(void)
{
    FILE *rej;
    char line[MIHFT_LINE_MAX];
    int64_t reject_id = 1;
    int last_decision = MIHFT_RC_ACCEPT;

    if (read_sccust("SCCUST.csv") != 0 ||
        read_scposf("SCPOSF.csv") != 0 ||
        read_scinstf("SCINSTF.csv") != 0) {
        return MIHFT_RC_IOERR;
    }

    rej = fopen("SCREJ", "w");
    if (rej == NULL) {
        return MIHFT_RC_IOERR;
    }

    while (fgets(line, sizeof(line), stdin) != NULL) {
        MihftOrderRec order;
        MihftCustRec *cust;
        MihftInstRec *inst;
        int64_t reject_amt;
        int code;

        if (line[0] == '\n' || line[0] == '\r') {
            continue;
        }
        if (parse_order(line, &order) != 0) {
            fclose(rej);
            return MIHFT_RC_PARSEERR;
        }

        cust = find_cust(order.cif_no);
        inst = find_inst(order.instr_code);
        if (cust == NULL || inst == NULL) {
            code = MIHFT_RC_REJECT_MARGIN;
        } else {
            code = reject_code(&order, cust, inst, &reject_amt);
        }

        if (code != MIHFT_RC_ACCEPT) {
            if (write_reject(rej, reject_id++, &order, code) != 0) {
                fclose(rej);
                return MIHFT_RC_IOERR;
            }
            last_decision = code;
        }
    }

    if (ferror(stdin) || fclose(rej) != 0) {
        return MIHFT_RC_IOERR;
    }
    return last_decision;
}
