/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20200310  市場基盤部  訂正ホットパス初版
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_ERR_IO      64
#define MIHFT_ERR_PARSE   65
#define MIHFT_ERR_NOTFOUND 66

#define MIHFT_LINE_MAX 512
#define MIHFT_ID_MAX 32
#define MIHFT_NAME_MAX 96
#define MIHFT_BOARD_MAX 8
#define MIHFT_STATE_MAX 8
#define MIHFT_PATH_MAX 64

typedef struct {
    char order_id[MIHFT_ID_MAX];
    char cif_no[MIHFT_ID_MAX];
    char instr_code[MIHFT_ID_MAX];
    char state_kbn[MIHFT_STATE_MAX];
    int64_t leaves_qty;
    int64_t cum_qty;
    int64_t avg_fill_amt;
    int64_t last_upd_ts;
} scords_rec_t;

typedef struct {
    char instr_code[MIHFT_ID_MAX];
    char instr_name[MIHFT_NAME_MAX];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[MIHFT_BOARD_MAX];
} scinstf_rec_t;

typedef struct {
    char order_id[MIHFT_ID_MAX];
    int64_t amend_price;
    int64_t amend_qty;
} amend_req_t;

static int copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t n;

    if (dst_sz == 0) {
        return -1;
    }
    n = strlen(src);
    if (n >= dst_sz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static char *trim_eol(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
    return s;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *endp;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }
    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || *endp != '\0') {
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

static int split_csv(char *line, char **cols, size_t want)
{
    size_t i = 0;
    char *p = line;

    while (i < want) {
        cols[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return i == want && strchr(cols[want - 1], ',') == NULL ? 0 : -1;
}

static int parse_order_line(char *line, scords_rec_t *rec)
{
    char *c[8];

    trim_eol(line);
    if (split_csv(line, c, 8) != 0) {
        return -1;
    }
    if (copy_field(rec->order_id, sizeof rec->order_id, c[0]) != 0 ||
        copy_field(rec->cif_no, sizeof rec->cif_no, c[1]) != 0 ||
        copy_field(rec->instr_code, sizeof rec->instr_code, c[2]) != 0 ||
        copy_field(rec->state_kbn, sizeof rec->state_kbn, c[3]) != 0 ||
        parse_i64(c[4], &rec->leaves_qty) != 0 ||
        parse_i64(c[5], &rec->cum_qty) != 0 ||
        parse_i64(c[6], &rec->avg_fill_amt) != 0 ||
        parse_i64(c[7], &rec->last_upd_ts) != 0) {
        return -1;
    }
    return 0;
}

static int parse_inst_line(char *line, scinstf_rec_t *rec)
{
    char *c[6];

    trim_eol(line);
    if (split_csv(line, c, 6) != 0) {
        return -1;
    }
    if (copy_field(rec->instr_code, sizeof rec->instr_code, c[0]) != 0 ||
        copy_field(rec->instr_name, sizeof rec->instr_name, c[1]) != 0 ||
        parse_int(c[2], &rec->instr_tier) != 0 ||
        parse_i64(c[3], &rec->tick_amt) != 0 ||
        parse_i64(c[4], &rec->lot_qty) != 0 ||
        copy_field(rec->board_code, sizeof rec->board_code, c[5]) != 0) {
        return -1;
    }
    return 0;
}

static int read_env_i64(const char *name, int64_t *out)
{
    const char *v = getenv(name);

    if (v == NULL) {
        return -1;
    }
    return parse_i64(v, out);
}

static int load_amend(amend_req_t *req)
{
    const char *id = getenv("MIHFT_AMEND_ORDER_ID");

    if (id == NULL || copy_field(req->order_id, sizeof req->order_id, id) != 0) {
        return -1;
    }
    if (read_env_i64("MIHFT_AMEND_PRICE", &req->amend_price) != 0 ||
        read_env_i64("MIHFT_AMEND_QTY", &req->amend_qty) != 0) {
        return -1;
    }
    return 0;
}

static int read_order(const char *path, const char *order_id, scords_rec_t *hit)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        return MIHFT_ERR_IO;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        scords_rec_t rec;
        if (parse_order_line(line, &rec) != 0) {
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (strcmp(rec.order_id, order_id) == 0) {
            *hit = rec;
            fclose(fp);
            return 0;
        }
    }
    if (ferror(fp)) {
        fclose(fp);
        return MIHFT_ERR_IO;
    }
    fclose(fp);
    return MIHFT_ERR_NOTFOUND;
}

static int read_inst(const char *path, const char *instr_code, scinstf_rec_t *hit)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        return MIHFT_ERR_IO;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        scinstf_rec_t rec;
        if (parse_inst_line(line, &rec) != 0) {
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (strcmp(rec.instr_code, instr_code) == 0) {
            *hit = rec;
            fclose(fp);
            return 0;
        }
    }
    if (ferror(fp)) {
        fclose(fp);
        return MIHFT_ERR_IO;
    }
    fclose(fp);
    return MIHFT_ERR_NOTFOUND;
}

static int mul_over_i64(int64_t a, int64_t b, int64_t *out)
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

static int decide_amend(const scords_rec_t *ord, const scinstf_rec_t *inst, const amend_req_t *req)
{
    int64_t notional;

    if (req->amend_price <= 0 || inst->tick_amt <= 0 || req->amend_price % inst->tick_amt != 0) {
        return 12;
    }
    if (req->amend_qty <= 0 || inst->lot_qty <= 0 || req->amend_qty % inst->lot_qty != 0) {
        return 12;
    }
    if (req->amend_qty > ord->leaves_qty + ord->cum_qty) {
        return 8;
    }
    if (mul_over_i64(req->amend_price, req->amend_qty, &notional) != 0) {
        return 8;
    }
    if (notional > MIHFT_MAX_NOTIONAL) {
        return 8;
    }
    if (ord->state_kbn[0] != 'O' && ord->state_kbn[0] != 'P') {
        return 4;
    }
    return 0;
}

static uint64_t fnv1a_mix(uint64_t h, const void *buf, size_t n)
{
    const unsigned char *p = (const unsigned char *)buf;
    size_t i;

    for (i = 0; i < n; i++) {
        h ^= (uint64_t)p[i];
        h *= UINT64_C(1099511628211);
    }
    return h;
}

static uint64_t payload_hash(const scords_rec_t *ord, const scinstf_rec_t *inst, const amend_req_t *req)
{
    uint64_t h = UINT64_C(1469598103934665603);

    h = fnv1a_mix(h, ord->order_id, strlen(ord->order_id));
    h = fnv1a_mix(h, ord->instr_code, strlen(ord->instr_code));
    h = fnv1a_mix(h, inst->board_code, strlen(inst->board_code));
    h = fnv1a_mix(h, &ord->leaves_qty, sizeof ord->leaves_qty);
    h = fnv1a_mix(h, &req->amend_price, sizeof req->amend_price);
    h = fnv1a_mix(h, &req->amend_qty, sizeof req->amend_qty);
    return h;
}

static int append_journal(const char *path, const scords_rec_t *ord, const scinstf_rec_t *inst, const amend_req_t *req)
{
    FILE *fp = fopen(path, "a");
    int64_t ts = (int64_t)time(NULL);
    uint64_t seq = (uint64_t)ts * UINT64_C(1000003) ^ payload_hash(ord, inst, req);

    if (fp == NULL) {
        return -1;
    }
    if (fprintf(fp, "%llu,%lld,AMD,%s,%s,%016llX\n",
                (unsigned long long)seq,
                (long long)ts,
                ord->order_id,
                ord->instr_code,
                (unsigned long long)payload_hash(ord, inst, req)) < 0) {
        fclose(fp);
        return -1;
    }
    return fclose(fp) == 0 ? 0 : -1;
}

static int rewrite_orders(const char *path, const scords_rec_t *newrec)
{
    FILE *in = fopen(path, "r");
    FILE *out = fopen("SCORDS.tmp", "w");
    char line[MIHFT_LINE_MAX];
    int replaced = 0;

    if (in == NULL || out == NULL) {
        if (in != NULL) {
            fclose(in);
        }
        if (out != NULL) {
            fclose(out);
        }
        return -1;
    }

    while (fgets(line, sizeof line, in) != NULL) {
        scords_rec_t rec;
        if (parse_order_line(line, &rec) != 0) {
            fclose(in);
            fclose(out);
            remove("SCORDS.tmp");
            return -1;
        }
        if (strcmp(rec.order_id, newrec->order_id) == 0) {
            rec = *newrec;
            replaced = 1;
        }
        if (fprintf(out, "%s,%s,%s,%s,%lld,%lld,%lld,%lld\n",
                    rec.order_id,
                    rec.cif_no,
                    rec.instr_code,
                    rec.state_kbn,
                    (long long)rec.leaves_qty,
                    (long long)rec.cum_qty,
                    (long long)rec.avg_fill_amt,
                    (long long)rec.last_upd_ts) < 0) {
            fclose(in);
            fclose(out);
            remove("SCORDS.tmp");
            return -1;
        }
    }

    if (ferror(in) || fclose(in) != 0 || fclose(out) != 0 || !replaced) {
        remove("SCORDS.tmp");
        return -1;
    }
    if (rename("SCORDS.tmp", path) != 0) {
        remove("SCORDS.tmp");
        return -1;
    }
    return 0;
}

int main(void)
{
    amend_req_t req;
    scords_rec_t ord;
    scinstf_rec_t inst;
    int rc;
    int decision;

    if (load_amend(&req) != 0) {
        fprintf(stderr, "訂正入力不正\n");
        return MIHFT_ERR_PARSE;
    }

    rc = read_order("SCORDS.csv", req.order_id, &ord);
    if (rc != 0) {
        fprintf(stderr, "注文読込失敗\n");
        return rc;
    }

    rc = read_inst("SCINSTF.csv", ord.instr_code, &inst);
    if (rc != 0) {
        fprintf(stderr, "銘柄読込失敗\n");
        return rc;
    }

    decision = decide_amend(&ord, &inst, &req);
    if (decision != 0) {
        return decision;
    }

    if (append_journal("SCJRNF.csv", &ord, &inst, &req) != 0) {
        fprintf(stderr, "訂正記録失敗\n");
        return MIHFT_ERR_IO;
    }

    ord.leaves_qty = req.amend_qty - ord.cum_qty;
    ord.last_upd_ts = (int64_t)time(NULL);

    if (ord.leaves_qty < 0 || rewrite_orders("SCORDS.csv", &ord) != 0) {
        fprintf(stderr, "注文更新失敗\n");
        return MIHFT_ERR_IO;
    }

    return decision;
}
