/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20250121  渡辺 隆 (E-260)    注文ゲートウェイ初版
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_OK 0
#define MIHFT_ERR_IO 91
#define MIHFT_ERR_PARSE 92
#define MIHFT_ERR_RANGE 93

#define MIHFT_REJ_CLOSED 4
#define MIHFT_REJ_LOT 8
#define MIHFT_REJ_PRICE 12
#define MIHFT_REJ_INSTR 16

#define MIHFT_MAX_LINE 1024
#define MIHFT_MAX_INST 4096
#define MIHFT_FIELD_MAX 16

typedef struct {
    char code[32];
    int tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board[8];
} gate_inst_t;

typedef struct {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn[4];
    char ord_type[4];
    char tif_code[8];
    int64_t ord_qty;
    int64_t price_amt;
    int tier;
} gate_order_t;

typedef struct {
    char sess_dt[16];
    char sess_kbn[8];
    char open_ts[32];
    char close_ts[32];
} gate_calendar_t;

static void trim_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int split_csv(char *line, char *field[], size_t need)
{
    size_t n = 0U;
    char *p = line;

    while (n < need) {
        field[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return n == need && strchr(field[need - 1U], ',') == NULL;
}

static int copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);
    if (n == 0U || n >= dstsz) {
        return 0;
    }
    memcpy(dst, src, n + 1U);
    return 1;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return 0;
    }
    *out = (int64_t)v;
    return 1;
}

static int parse_int(const char *s, int *out)
{
    int64_t v;
    if (!parse_i64(s, &v) || v < INT_MIN || v > INT_MAX) {
        return 0;
    }
    *out = (int)v;
    return 1;
}

static uint64_t fnv1a_update(uint64_t h, const char *s)
{
    while (*s != '\0') {
        h ^= (unsigned char)*s++;
        h *= UINT64_C(1099511628211);
    }
    return h;
}

static uint64_t order_hash(const gate_order_t *o)
{
    uint64_t h = UINT64_C(1469598103934665603);
    char buf[64];

    h = fnv1a_update(h, o->order_id);
    h = fnv1a_update(h, o->cif_no);
    h = fnv1a_update(h, o->instr_code);
    h = fnv1a_update(h, o->side_kbn);
    h = fnv1a_update(h, o->ord_type);
    h = fnv1a_update(h, o->tif_code);
    snprintf(buf, sizeof(buf), "%" PRId64 ":%" PRId64 ":%d", o->ord_qty, o->price_amt, o->tier);
    h = fnv1a_update(h, buf);
    return h;
}

static int load_calendar(gate_calendar_t *cal)
{
    FILE *fp = fopen("SCCALF.csv", "r");
    char line[MIHFT_MAX_LINE];
    char *f[4];

    if (fp == NULL) {
        return MIHFT_ERR_IO;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        trim_eol(line);
        if (split_csv(line, f, 4U) &&
            copy_field(cal->sess_dt, sizeof(cal->sess_dt), f[0]) &&
            copy_field(cal->sess_kbn, sizeof(cal->sess_kbn), f[1]) &&
            copy_field(cal->open_ts, sizeof(cal->open_ts), f[2]) &&
            copy_field(cal->close_ts, sizeof(cal->close_ts), f[3])) {
            fclose(fp);
            return MIHFT_OK;
        }
    }
    fclose(fp);
    return MIHFT_ERR_PARSE;
}

static int load_instruments(gate_inst_t inst[], size_t *count)
{
    FILE *fp = fopen("SCINSTF.csv", "r");
    char line[MIHFT_MAX_LINE];
    char *f[6];
    size_t n = 0U;

    if (fp == NULL) {
        return MIHFT_ERR_IO;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        gate_inst_t x;
        trim_eol(line);
        if (!split_csv(line, f, 6U)) {
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (!copy_field(x.code, sizeof(x.code), f[0]) ||
            !parse_int(f[2], &x.tier) ||
            !parse_i64(f[3], &x.tick_amt) ||
            !parse_i64(f[4], &x.lot_qty) ||
            !copy_field(x.board, sizeof(x.board), f[5])) {
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (x.lot_qty <= 0 || x.tick_amt <= 0 || n >= MIHFT_MAX_INST) {
            fclose(fp);
            return MIHFT_ERR_RANGE;
        }
        inst[n++] = x;
    }
    fclose(fp);
    *count = n;
    return MIHFT_OK;
}

static const gate_inst_t *find_inst(const gate_inst_t inst[], size_t count, const char *code)
{
    size_t i;
    for (i = 0U; i < count; i++) {
        if (strcmp(inst[i].code, code) == 0) {
            return &inst[i];
        }
    }
    return NULL;
}

static int parse_order(char *line, gate_order_t *o)
{
    char *f[9];

    trim_eol(line);
    if (!split_csv(line, f, 9U)) {
        return 0;
    }
    return copy_field(o->order_id, sizeof(o->order_id), f[0]) &&
           copy_field(o->cif_no, sizeof(o->cif_no), f[1]) &&
           copy_field(o->instr_code, sizeof(o->instr_code), f[2]) &&
           copy_field(o->side_kbn, sizeof(o->side_kbn), f[3]) &&
           copy_field(o->ord_type, sizeof(o->ord_type), f[4]) &&
           copy_field(o->tif_code, sizeof(o->tif_code), f[5]) &&
           parse_i64(f[6], &o->ord_qty) &&
           parse_i64(f[7], &o->price_amt) &&
           parse_int(f[8], &o->tier);
}

static int session_open(const gate_calendar_t *cal)
{
    return strcmp(cal->sess_kbn, "O") == 0 &&
           cal->open_ts[0] != '\0' &&
           cal->close_ts[0] != '\0' &&
           strcmp(cal->open_ts, cal->close_ts) < 0;
}

static int validate_order(const gate_order_t *o, const gate_inst_t *inst, const gate_calendar_t *cal)
{
    if (!session_open(cal)) {
        return MIHFT_REJ_CLOSED;
    }
    if (inst == NULL || inst->tier != o->tier) {
        return MIHFT_REJ_INSTR;
    }
    if (o->ord_qty <= 0 || o->ord_qty % inst->lot_qty != 0) {
        return MIHFT_REJ_LOT;
    }
    if (o->price_amt <= 0) {
        return MIHFT_REJ_PRICE;
    }
    if (o->price_amt > INT64_MAX / o->ord_qty) {
        return MIHFT_REJ_PRICE;
    }
    if ((o->price_amt * o->ord_qty) / 100 > MIHFT_MAX_NOTIONAL) {
        return MIHFT_REJ_PRICE;
    }
    return MIHFT_OK;
}

static int write_reject(FILE *rej, uint64_t seq, const gate_order_t *o, int reject_cd)
{
    if (fprintf(rej, "R%012" PRIu64 ",%s,%s,%s,%d,202501150900000000\n",
                seq, o->order_id, o->cif_no, o->instr_code, reject_cd) < 0) {
        return 0;
    }
    return 1;
}

static int write_journal(FILE *jrn, uint64_t seq, const gate_order_t *o)
{
    if (fprintf(jrn, "%" PRIu64 ",202501150900000000,ORDGATE,%s,%s,%016" PRIx64 "\n",
                seq, o->order_id, o->instr_code, order_hash(o)) < 0) {
        return 0;
    }
    return 1;
}

int main(void)
{
    gate_inst_t inst[MIHFT_MAX_INST];
    gate_calendar_t cal;
    size_t inst_count = 0U;
    FILE *ord = NULL;
    FILE *jrn = NULL;
    FILE *rej = NULL;
    char line[MIHFT_MAX_LINE];
    uint64_t seq = 1U;
    int rc;
    int final_code = MIHFT_OK;

    rc = load_calendar(&cal);
    if (rc != MIHFT_OK) {
        return rc;
    }
    rc = load_instruments(inst, &inst_count);
    if (rc != MIHFT_OK) {
        return rc;
    }

    ord = fopen("SCORDF.csv", "r");
    jrn = fopen("SCJRNF.dat", "w");
    rej = fopen("SCREJ.dat", "w");
    if (ord == NULL || jrn == NULL || rej == NULL) {
        if (ord != NULL) fclose(ord);
        if (jrn != NULL) fclose(jrn);
        if (rej != NULL) fclose(rej);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), ord) != NULL) {
        gate_order_t o;
        const gate_inst_t *mi;
        int decision;

        if (!parse_order(line, &o)) {
            fclose(ord);
            fclose(jrn);
            fclose(rej);
            return MIHFT_ERR_PARSE;
        }

        mi = find_inst(inst, inst_count, o.instr_code);
        decision = validate_order(&o, mi, &cal);
        if (decision == MIHFT_OK) {
            if (!write_journal(jrn, seq, &o)) {
                fclose(ord);
                fclose(jrn);
                fclose(rej);
                return MIHFT_ERR_IO;
            }
        } else {
            final_code = decision;
            if (!write_reject(rej, seq, &o, decision)) {
                fclose(ord);
                fclose(jrn);
                fclose(rej);
                return MIHFT_ERR_IO;
            }
        }
        seq++;
    }

    if (ferror(ord) || fclose(ord) != 0 || fclose(jrn) != 0 || fclose(rej) != 0) {
        return MIHFT_ERR_IO;
    }
    return final_code;
}
