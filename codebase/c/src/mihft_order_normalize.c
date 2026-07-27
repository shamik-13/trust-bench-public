/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220906  渡辺 隆 (E-260)  注文正規化ホットパス初版
 */
#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_ERR_IO      16
#define MIHFT_ERR_PARSE   20
#define MIHFT_MAX_LINE    512
#define MIHFT_MAX_INST    4096
#define MIHFT_MAX_FIELD   64

typedef struct {
    char instr_code[MIHFT_MAX_FIELD];
    char instr_name[128];
    int instr_tier;
    int64_t tick_amt_x100;
    int64_t lot_qty;
    char board_code[MIHFT_MAX_FIELD];
} inst_rec_t;

typedef struct {
    char order_id[MIHFT_MAX_FIELD];
    char cif_no[MIHFT_MAX_FIELD];
    char instr_code[MIHFT_MAX_FIELD];
    char side_kbn;
    char ord_type;
    char tif_code[MIHFT_MAX_FIELD];
    int64_t ord_qty;
    int64_t price_amt_x100;
    int instr_tier;
} ord_rec_t;

static int split_csv(char *line, char **field, int max_field)
{
    int n = 0;
    char *p = line;

    while (n < max_field) {
        field[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    if (strchr(field[n - 1], '\n') != NULL) {
        *strchr(field[n - 1], '\n') = '\0';
    }
    if (strchr(field[n - 1], '\r') != NULL) {
        *strchr(field[n - 1], '\r') = '\0';
    }

    return n;
}

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t len = strlen(src);

    if (len == 0 || len >= dst_len) {
        return -1;
    }
    memcpy(dst, src, len + 1);
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

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

    if (parse_i64(s, &v) != 0 || v < INT32_MIN || v > INT32_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int parse_amt_x100(const char *s, int64_t *out)
{
    int neg = 0;
    int frac = 0;
    int seen_digit = 0;
    int64_t whole = 0;
    int64_t cents = 0;
    const unsigned char *p = (const unsigned char *)s;

    if (*p == '-') {
        neg = 1;
        p++;
    }

    while (*p != '\0') {
        if (*p == '.') {
            if (frac != 0) {
                return -1;
            }
            frac = 1;
            p++;
            continue;
        }
        if (*p < '0' || *p > '9') {
            return -1;
        }

        seen_digit = 1;
        if (frac == 0) {
            if (whole > (INT64_MAX - 9) / 10) {
                return -1;
            }
            whole = whole * 10 + (int64_t)(*p - '0');
        } else {
            if (frac > 2) {
                return -1;
            }
            cents = cents * 10 + (int64_t)(*p - '0');
            frac++;
        }
        p++;
    }

    if (!seen_digit || whole > (INT64_MAX / 100)) {
        return -1;
    }
    if (frac == 2) {
        cents *= 10;
    }

    *out = whole * 100 + cents;
    if (neg) {
        *out = -*out;
    }
    return 0;
}

static int load_inst(inst_rec_t *inst, size_t *inst_count)
{
    FILE *fp = fopen("SCINSTF.csv", "r");
    char line[MIHFT_MAX_LINE];

    if (fp == NULL) {
        fprintf(stderr, "SCINSTFオープン失敗\n");
        return MIHFT_ERR_IO;
    }

    *inst_count = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];
        inst_rec_t *r;

        if (strncmp(line, "INSTR-CODE,", 11) == 0) {
            continue;
        }
        if (*inst_count >= MIHFT_MAX_INST || split_csv(line, f, 6) != 6) {
            fclose(fp);
            fprintf(stderr, "SCINSTF形式不正\n");
            return MIHFT_ERR_PARSE;
        }

        r = &inst[(*inst_count)++];
        if (copy_field(r->instr_code, sizeof(r->instr_code), f[0]) != 0 ||
            copy_field(r->instr_name, sizeof(r->instr_name), f[1]) != 0 ||
            parse_int(f[2], &r->instr_tier) != 0 ||
            parse_amt_x100(f[3], &r->tick_amt_x100) != 0 ||
            parse_i64(f[4], &r->lot_qty) != 0 ||
            copy_field(r->board_code, sizeof(r->board_code), f[5]) != 0 ||
            r->tick_amt_x100 <= 0 || r->lot_qty <= 0) {
            fclose(fp);
            fprintf(stderr, "SCINSTF項目不正\n");
            return MIHFT_ERR_PARSE;
        }
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCINSTF読込失敗\n");
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    return 0;
}

static const inst_rec_t *find_inst(const inst_rec_t *inst, size_t inst_count, const char *code)
{
    size_t i;

    for (i = 0; i < inst_count; i++) {
        if (strcmp(inst[i].instr_code, code) == 0) {
            return &inst[i];
        }
    }
    return NULL;
}

static int parse_order(char *line, ord_rec_t *o)
{
    char *f[9];

    if (split_csv(line, f, 9) != 9) {
        return -1;
    }

    if (copy_field(o->order_id, sizeof(o->order_id), f[0]) != 0 ||
        copy_field(o->cif_no, sizeof(o->cif_no), f[1]) != 0 ||
        copy_field(o->instr_code, sizeof(o->instr_code), f[2]) != 0 ||
        strlen(f[3]) != 1 ||
        strlen(f[4]) != 1 ||
        copy_field(o->tif_code, sizeof(o->tif_code), f[5]) != 0 ||
        parse_i64(f[6], &o->ord_qty) != 0 ||
        parse_amt_x100(f[7], &o->price_amt_x100) != 0 ||
        parse_int(f[8], &o->instr_tier) != 0) {
        return -1;
    }

    o->side_kbn = f[3][0];
    o->ord_type = f[4][0];
    return 0;
}

static int valid_tif(const char *tif)
{
    return strcmp(tif, "DAY") == 0 || strcmp(tif, "IOC") == 0 || strcmp(tif, "FOK") == 0;
}

static int calc_notional(int64_t qty, int64_t price_x100, int64_t *notional)
{
    if (qty <= 0 || price_x100 < 0) {
        return -1;
    }
    if (price_x100 != 0 && qty > INT64_MAX / price_x100) {
        return -1;
    }
    *notional = (qty * price_x100) / 100;
    return 0;
}

static int write_reject(FILE *fp, uint64_t seq, const ord_rec_t *o, int code)
{
    return fprintf(fp, "RJ%012" PRIu64 ",%s,%s,%s,%d,%" PRIu64 "\n",
                   seq, o->order_id, o->cif_no, o->instr_code, code, seq) < 0 ? -1 : 0;
}

int main(void)
{
    inst_rec_t inst[MIHFT_MAX_INST];
    size_t inst_count = 0;
    FILE *ord_fp;
    FILE *rej_fp;
    char line[MIHFT_MAX_LINE];
    uint64_t reject_seq = 1;
    int final_code = 0;
    int load_rc;

    load_rc = load_inst(inst, &inst_count);
    if (load_rc != 0) {
        return load_rc;
    }

    ord_fp = fopen("SCORDF.csv", "r");
    if (ord_fp == NULL) {
        fprintf(stderr, "SCORDFオープン失敗\n");
        return MIHFT_ERR_IO;
    }

    rej_fp = fopen("SCREJTF.dat", "w");
    if (rej_fp == NULL) {
        fclose(ord_fp);
        fprintf(stderr, "SCREJTFオープン失敗\n");
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), ord_fp) != NULL) {
        ord_rec_t rec;
        const inst_rec_t *ir;
        int reject_code = 0;
        int64_t notional = 0;

        if (strncmp(line, "ORDER-ID,", 9) == 0) {
            continue;
        }

        if (parse_order(line, &rec) != 0) {
            fclose(rej_fp);
            fclose(ord_fp);
            fprintf(stderr, "SCORDF形式不正\n");
            return MIHFT_ERR_PARSE;
        }

        ir = find_inst(inst, inst_count, rec.instr_code);
        if (ir == NULL || rec.instr_tier != ir->instr_tier) {
            reject_code = 12;
        } else if (rec.side_kbn != 'B' && rec.side_kbn != 'S') {
            reject_code = 12;
        } else if (rec.ord_type != 'L' && rec.ord_type != 'M') {
            reject_code = 12;
        } else if (!valid_tif(rec.tif_code)) {
            reject_code = 12;
        } else if (rec.ord_qty <= 0 || rec.ord_qty % ir->lot_qty != 0) {
            reject_code = 12;
        } else if (rec.ord_type == 'L' &&
                   (rec.price_amt_x100 <= 0 || rec.price_amt_x100 % ir->tick_amt_x100 != 0)) {
            reject_code = 12;
        } else if (calc_notional(rec.ord_qty, rec.price_amt_x100, &notional) != 0 ||
                   notional > MIHFT_MAX_NOTIONAL) {
            reject_code = 8;
        } else {
            order_t normalized_order;
            cust_t customer_stub;
            int risk_code;

            memset(&normalized_order, 0, sizeof(normalized_order));
            memset(&customer_stub, 0, sizeof(customer_stub));
            risk_code = mihft_risk_eval(&normalized_order, &customer_stub);
            if (risk_code != 0) {
                reject_code = risk_code;
            }
        }

        if (reject_code != 0) {
            if (write_reject(rej_fp, reject_seq++, &rec, reject_code) != 0) {
                fclose(rej_fp);
                fclose(ord_fp);
                fprintf(stderr, "SCREJTF書込失敗\n");
                return MIHFT_ERR_IO;
            }
            final_code = reject_code;
        }
    }

    if (ferror(ord_fp) || fclose(ord_fp) != 0) {
        fclose(rej_fp);
        fprintf(stderr, "SCORDF読込失敗\n");
        return MIHFT_ERR_IO;
    }
    if (fclose(rej_fp) != 0) {
        fprintf(stderr, "SCREJTFクローズ失敗\n");
        return MIHFT_ERR_IO;
    }

    return final_code;
}
