/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20250603  藤田 和也 (E-271)  初版作成
 * 1.01  20251103  福田 亮太 (E-211)  CSV検査と未突合約定の判定返却を追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_RC_ACCEPT 0
#define MIHFT_RC_REJECT_MARGIN 4
#define MIHFT_RC_REJECT_NOTIONAL 8
#define MIHFT_RC_REJECT_TICK 12

#define MIHFT_IO_ERROR 16
#define MIHFT_PARSE_ERROR 20

#define MIHFT_MAX_LINE 1024
#define MIHFT_MAX_EXEC 200000
#define MIHFT_MAX_ORDER 200000
#define MIHFT_MAX_ID 32
#define MIHFT_MAX_CODE 32
#define MIHFT_MAX_CIF 32
#define MIHFT_MAX_SIDE 2
#define MIHFT_MAX_ORDTYPE 2
#define MIHFT_MAX_TIF 4
#define MIHFT_EXEC_FILE "SCEXEC.csv"
#define MIHFT_ORDER_FILE "SCORDF.csv"
#define MIHFT_DROP_FILE "HFDROPQ.csv"

typedef struct {
    char exec_id[MIHFT_MAX_ID];
    char order_id[MIHFT_MAX_ID];
    char instr_code[MIHFT_MAX_CODE];
    char side_kbn[MIHFT_MAX_SIDE];
    int64_t fill_qty;
    int64_t fill_amt;
    int64_t exec_ts;
} MihftExecRec;

typedef struct {
    char order_id[MIHFT_MAX_ID];
    char cif_no[MIHFT_MAX_CIF];
    char instr_code[MIHFT_MAX_CODE];
    char side_kbn[MIHFT_MAX_SIDE];
    char ord_type[MIHFT_MAX_ORDTYPE];
    char tif_code[MIHFT_MAX_TIF];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} MihftOrderRec;

typedef struct {
    MihftExecRec *data;
    size_t used;
    size_t cap;
} MihftExecVec;

typedef struct {
    MihftOrderRec *data;
    size_t used;
    size_t cap;
} MihftOrderVec;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n;

    if (dstsz == 0 || src == NULL) {
        return -1;
    }

    n = strlen(src);
    if (n == 0 || n >= dstsz) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static int mihft_parse_i64(const char *s, int64_t *out)
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

static int mihft_parse_int(const char *s, int *out)
{
    int64_t v;

    if (mihft_parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int mihft_split_csv(char *line, char **field, size_t expect)
{
    size_t i = 0;
    char *p = line;

    while (i < expect) {
        field[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    return (i == expect && strchr(field[expect - 1], ',') == NULL) ? 0 : -1;
}

static int mihft_valid_side(const char *s)
{
    return strcmp(s, "B") == 0 || strcmp(s, "S") == 0;
}

static int mihft_valid_ord_type(const char *s)
{
    return strcmp(s, "L") == 0 || strcmp(s, "M") == 0;
}

static int mihft_valid_tif(const char *s)
{
    return strcmp(s, "DAY") == 0 || strcmp(s, "IOC") == 0 || strcmp(s, "FOK") == 0;
}

static int mihft_tier_rate_bp(int tier)
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
    return -1;
}

static int64_t mihft_tier_tick(int tier)
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
    return -1;
}

static int mihft_checked_mul_i64(int64_t a, int64_t b, int64_t *out)
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

static int mihft_push_exec(MihftExecVec *v, const MihftExecRec *r)
{
    MihftExecRec *p;
    size_t next;

    if (v->used == v->cap) {
        next = v->cap == 0 ? 4096u : v->cap * 2u;
        if (next > MIHFT_MAX_EXEC) {
            next = MIHFT_MAX_EXEC;
        }
        if (next <= v->cap) {
            return -1;
        }
        p = (MihftExecRec *)realloc(v->data, next * sizeof(*p));
        if (p == NULL) {
            return -1;
        }
        v->data = p;
        v->cap = next;
    }

    v->data[v->used++] = *r;
    return 0;
}

static int mihft_push_order(MihftOrderVec *v, const MihftOrderRec *r)
{
    MihftOrderRec *p;
    size_t next;

    if (v->used == v->cap) {
        next = v->cap == 0 ? 4096u : v->cap * 2u;
        if (next > MIHFT_MAX_ORDER) {
            next = MIHFT_MAX_ORDER;
        }
        if (next <= v->cap) {
            return -1;
        }
        p = (MihftOrderRec *)realloc(v->data, next * sizeof(*p));
        if (p == NULL) {
            return -1;
        }
        v->data = p;
        v->cap = next;
    }

    v->data[v->used++] = *r;
    return 0;
}

static int mihft_read_execs(const char *path, MihftExecVec *vec)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    unsigned long row = 0;

    if (fp == NULL) {
        fprintf(stderr, "E%04d: %s を開けません\n", MIHFT_IO_ERROR, path);
        return MIHFT_IO_ERROR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[7];
        MihftExecRec r;

        row++;
        mihft_chomp(line);
        if (row == 1 && strncmp(line, "EXEC-ID,", 8) == 0) {
            continue;
        }
        if (mihft_split_csv(line, f, 7) != 0 ||
            mihft_copy_field(r.exec_id, sizeof(r.exec_id), f[0]) != 0 ||
            mihft_copy_field(r.order_id, sizeof(r.order_id), f[1]) != 0 ||
            mihft_copy_field(r.instr_code, sizeof(r.instr_code), f[2]) != 0 ||
            mihft_copy_field(r.side_kbn, sizeof(r.side_kbn), f[3]) != 0 ||
            mihft_parse_i64(f[4], &r.fill_qty) != 0 ||
            mihft_parse_i64(f[5], &r.fill_amt) != 0 ||
            mihft_parse_i64(f[6], &r.exec_ts) != 0 ||
            !mihft_valid_side(r.side_kbn) ||
            r.fill_qty <= 0 ||
            r.fill_amt <= 0) {
            fprintf(stderr, "E%04d: %s:%lu 約定CSV不正\n", MIHFT_PARSE_ERROR, path, row);
            fclose(fp);
            return MIHFT_PARSE_ERROR;
        }
        if (mihft_push_exec(vec, &r) != 0) {
            fprintf(stderr, "E%04d: 約定領域不足\n", MIHFT_IO_ERROR);
            fclose(fp);
            return MIHFT_IO_ERROR;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E%04d: %s 読込失敗\n", MIHFT_IO_ERROR, path);
        fclose(fp);
        return MIHFT_IO_ERROR;
    }

    fclose(fp);
    return MIHFT_RC_ACCEPT;
}

static int mihft_read_orders(const char *path, MihftOrderVec *vec)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    unsigned long row = 0;

    if (fp == NULL) {
        fprintf(stderr, "E%04d: %s を開けません\n", MIHFT_IO_ERROR, path);
        return MIHFT_IO_ERROR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[9];
        MihftOrderRec r;

        row++;
        mihft_chomp(line);
        if (row == 1 && strncmp(line, "ORDER-ID,", 9) == 0) {
            continue;
        }
        if (mihft_split_csv(line, f, 9) != 0 ||
            mihft_copy_field(r.order_id, sizeof(r.order_id), f[0]) != 0 ||
            mihft_copy_field(r.cif_no, sizeof(r.cif_no), f[1]) != 0 ||
            mihft_copy_field(r.instr_code, sizeof(r.instr_code), f[2]) != 0 ||
            mihft_copy_field(r.side_kbn, sizeof(r.side_kbn), f[3]) != 0 ||
            mihft_copy_field(r.ord_type, sizeof(r.ord_type), f[4]) != 0 ||
            mihft_copy_field(r.tif_code, sizeof(r.tif_code), f[5]) != 0 ||
            mihft_parse_i64(f[6], &r.ord_qty) != 0 ||
            mihft_parse_i64(f[7], &r.price_amt) != 0 ||
            mihft_parse_int(f[8], &r.instr_tier) != 0 ||
            !mihft_valid_side(r.side_kbn) ||
            !mihft_valid_ord_type(r.ord_type) ||
            !mihft_valid_tif(r.tif_code) ||
            r.ord_qty <= 0 ||
            r.price_amt <= 0 ||
            mihft_tier_rate_bp(r.instr_tier) < 0) {
            fprintf(stderr, "E%04d: %s:%lu 注文CSV不正\n", MIHFT_PARSE_ERROR, path, row);
            fclose(fp);
            return MIHFT_PARSE_ERROR;
        }
        if (mihft_push_order(vec, &r) != 0) {
            fprintf(stderr, "E%04d: 注文領域不足\n", MIHFT_IO_ERROR);
            fclose(fp);
            return MIHFT_IO_ERROR;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E%04d: %s 読込失敗\n", MIHFT_IO_ERROR, path);
        fclose(fp);
        return MIHFT_IO_ERROR;
    }

    fclose(fp);
    return MIHFT_RC_ACCEPT;
}

static const MihftOrderRec *mihft_find_order(const MihftOrderVec *orders, const char *order_id)
{
    size_t i;

    for (i = 0; i < orders->used; i++) {
        if (strcmp(orders->data[i].order_id, order_id) == 0) {
            return &orders->data[i];
        }
    }

    return NULL;
}

static int mihft_validate_joined(const MihftExecRec *e, const MihftOrderRec *o)
{
    int64_t notional;
    int64_t margin;
    int64_t tick = mihft_tier_tick(o->instr_tier);
    int rate_bp = mihft_tier_rate_bp(o->instr_tier);

    if (strcmp(e->instr_code, o->instr_code) != 0 || strcmp(e->side_kbn, o->side_kbn) != 0) {
        return MIHFT_PARSE_ERROR;
    }

    if (mihft_checked_mul_i64(e->fill_qty, o->price_amt, &notional) != 0) {
        return MIHFT_PARSE_ERROR;
    }

    if (notional > MIHFT_MAX_NOTIONAL || e->fill_amt > MIHFT_MAX_NOTIONAL) {
        return MIHFT_RC_REJECT_NOTIONAL;
    }

    if (tick <= 0 || o->price_amt % tick != 0) {
        return MIHFT_RC_REJECT_TICK;
    }

    if (mihft_checked_mul_i64(e->fill_amt, (int64_t)rate_bp, &margin) != 0) {
        return MIHFT_PARSE_ERROR;
    }

    if (margin / 10000 > MIHFT_MAX_NOTIONAL / 2) {
        return MIHFT_RC_REJECT_MARGIN;
    }

    return MIHFT_RC_ACCEPT;
}

static int mihft_write_drop(FILE *fp, uint64_t seq, const MihftExecRec *e)
{
    time_t now = time(NULL);

    if (now == (time_t)-1) {
        return -1;
    }

    if (fprintf(fp, "D%012" PRIu64 ",%s,%s,%s,%" PRId64 ",%" PRId64 ",%" PRIdMAX "\n",
                seq,
                e->exec_id,
                e->order_id,
                e->instr_code,
                e->fill_qty,
                e->fill_amt,
                (intmax_t)now) < 0) {
        return -1;
    }

    return 0;
}

int main(void)
{
    MihftExecVec execs = { 0 };
    MihftOrderVec orders = { 0 };
    FILE *out = NULL;
    size_t i;
    uint64_t drop_seq = 1;
    int rc = MIHFT_RC_ACCEPT;

    rc = mihft_read_orders(MIHFT_ORDER_FILE, &orders);
    if (rc != MIHFT_RC_ACCEPT) {
        free(execs.data);
        free(orders.data);
        return rc;
    }

    rc = mihft_read_execs(MIHFT_EXEC_FILE, &execs);
    if (rc != MIHFT_RC_ACCEPT) {
        free(execs.data);
        free(orders.data);
        return rc;
    }

    out = fopen(MIHFT_DROP_FILE, "w");
    if (out == NULL) {
        fprintf(stderr, "E%04d: %s を作成できません\n", MIHFT_IO_ERROR, MIHFT_DROP_FILE);
        free(execs.data);
        free(orders.data);
        return MIHFT_IO_ERROR;
    }

    if (fprintf(out, "DROP-ID,EXEC-ID,ORDER-ID,INSTR-CODE,FILL-QTY,FILL-AMT,CAPTURE-TS\n") < 0) {
        fprintf(stderr, "E%04d: %s 見出し書込失敗\n", MIHFT_IO_ERROR, MIHFT_DROP_FILE);
        fclose(out);
        free(execs.data);
        free(orders.data);
        return MIHFT_IO_ERROR;
    }

    for (i = 0; i < execs.used; i++) {
        const MihftExecRec *e = &execs.data[i];
        const MihftOrderRec *o = mihft_find_order(&orders, e->order_id);
        int decision;

        if (o == NULL) {
            fprintf(stderr, "D%04d: EXEC-ID=%s ORDER-ID=%s 注文未検出\n",
                    MIHFT_RC_REJECT_NOTIONAL, e->exec_id, e->order_id);
            rc = MIHFT_RC_REJECT_NOTIONAL;
            continue;
        }

        decision = mihft_validate_joined(e, o);
        if (decision == MIHFT_PARSE_ERROR) {
            fprintf(stderr, "E%04d: EXEC-ID=%s 注文属性不整合\n", MIHFT_PARSE_ERROR, e->exec_id);
            rc = MIHFT_PARSE_ERROR;
            break;
        }
        if (decision != MIHFT_RC_ACCEPT) {
            fprintf(stderr, "D%04d: EXEC-ID=%s 約定連結拒否\n", decision, e->exec_id);
            if (rc == MIHFT_RC_ACCEPT) {
                rc = decision;
            }
            continue;
        }

        if (mihft_write_drop(out, drop_seq, e) != 0) {
            fprintf(stderr, "E%04d: %s 明細書込失敗\n", MIHFT_IO_ERROR, MIHFT_DROP_FILE);
            rc = MIHFT_IO_ERROR;
            break;
        }
        drop_seq++;
    }

    if (fclose(out) != 0 && rc == MIHFT_RC_ACCEPT) {
        fprintf(stderr, "E%04d: %s 終了書込失敗\n", MIHFT_IO_ERROR, MIHFT_DROP_FILE);
        rc = MIHFT_IO_ERROR;
    }

    free(execs.data);
    free(orders.data);
    return rc;
}
