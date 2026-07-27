/*
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  20250121  小林 直樹 (E-252)  スマートオーダールータの事前判定版を作成
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_DEC_ACCEPT           0
#define MIHFT_DEC_REJECT_MARGIN    4
#define MIHFT_DEC_REJECT_NOTIONAL  8
#define MIHFT_DEC_REJECT_TICK      12

#define MIHFT_MAX_ORDERS           4096
#define MIHFT_MAX_INSTRUMENTS      4096
#define MIHFT_MAX_FIELDS           16
#define MIHFT_LINE_SIZE            1024
#define MIHFT_ID_SIZE              32
#define MIHFT_NAME_SIZE            96
#define MIHFT_CODE_SIZE            16
#define MIHFT_SIDE_SIZE            4
#define MIHFT_TYPE_SIZE            4
#define MIHFT_TIF_SIZE             8
#define MIHFT_BOARD_SIZE           8

typedef struct {
    char order_id[MIHFT_ID_SIZE];
    char cif_no[MIHFT_ID_SIZE];
    char instr_code[MIHFT_CODE_SIZE];
    char side_kbn[MIHFT_SIDE_SIZE];
    char ord_type[MIHFT_TYPE_SIZE];
    char tif_code[MIHFT_TIF_SIZE];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} StagedOrder;

typedef struct {
    char instr_code[MIHFT_CODE_SIZE];
    char instr_name[MIHFT_NAME_SIZE];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[MIHFT_BOARD_SIZE];
} StagedInstrument;

static void trim_field(char *s)
{
    size_t len;
    char *p;

    while (isspace((unsigned char)*s)) {
        memmove(s, s + 1, strlen(s));
    }

    len = strlen(s);
    while (len > 0U && isspace((unsigned char)s[len - 1U])) {
        s[--len] = '\0';
    }

    if (len >= 2U && s[0] == '"' && s[len - 1U] == '"') {
        memmove(s, s + 1, len - 2U);
        s[len - 2U] = '\0';
        for (p = s; *p != '\0'; ++p) {
            if (p[0] == '"' && p[1] == '"') {
                memmove(p, p + 1, strlen(p));
            }
        }
    }
}

static int split_csv(char *line, char **fields, size_t max_fields)
{
    size_t n = 0U;
    int in_quote = 0;
    char *start = line;
    char *p;

    for (p = line; *p != '\0'; ++p) {
        if (*p == '"') {
            in_quote = !in_quote;
        } else if (*p == ',' && !in_quote) {
            if (n >= max_fields) {
                return -1;
            }
            *p = '\0';
            fields[n++] = start;
            start = p + 1;
        }
    }

    if (in_quote || n >= max_fields) {
        return -1;
    }

    fields[n++] = start;
    for (size_t i = 0U; i < n; ++i) {
        trim_field(fields[i]);
    }
    return (int)n;
}

static int copy_token(char *dst, size_t dst_size, const char *src)
{
    size_t len = strlen(src);

    if (dst_size == 0U || len >= dst_size) {
        return -1;
    }
    memcpy(dst, src, len + 1U);
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (*s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0' || v < 0) {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int parse_int_range(const char *s, int min_v, int max_v, int *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v < min_v || v > max_v) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int is_header_line(const char *first)
{
    return strcmp(first, "ORDER-ID") == 0 || strcmp(first, "INSTR-CODE") == 0;
}

static FILE *open_staged_file(const char *base_name)
{
    char path[64];
    FILE *fp = fopen(base_name, "r");

    if (fp != NULL) {
        return fp;
    }

    if (snprintf(path, sizeof(path), "%s.csv", base_name) >= (int)sizeof(path)) {
        return NULL;
    }
    return fopen(path, "r");
}

static int read_orders(StagedOrder *orders, size_t *count)
{
    FILE *fp = open_staged_file("SCORDF");
    char line[MIHFT_LINE_SIZE];
    size_t n = 0U;
    unsigned long row = 0UL;

    if (fp == NULL) {
        fprintf(stderr, "SCORDFを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *fields[MIHFT_MAX_FIELDS];
        int nf;

        ++row;
        line[strcspn(line, "\r\n")] = '\0';
        if (line[0] == '\0') {
            continue;
        }

        nf = split_csv(line, fields, MIHFT_MAX_FIELDS);
        if (nf < 0 || nf != 9) {
            fprintf(stderr, "SCORDFの%lu行目が不正です\n", row);
            fclose(fp);
            return -1;
        }

        if (row == 1UL && is_header_line(fields[0])) {
            continue;
        }

        if (n >= MIHFT_MAX_ORDERS) {
            fprintf(stderr, "SCORDFの件数が上限を超過しました\n");
            fclose(fp);
            return -1;
        }

        if (copy_token(orders[n].order_id, sizeof(orders[n].order_id), fields[0]) != 0 ||
            copy_token(orders[n].cif_no, sizeof(orders[n].cif_no), fields[1]) != 0 ||
            copy_token(orders[n].instr_code, sizeof(orders[n].instr_code), fields[2]) != 0 ||
            copy_token(orders[n].side_kbn, sizeof(orders[n].side_kbn), fields[3]) != 0 ||
            copy_token(orders[n].ord_type, sizeof(orders[n].ord_type), fields[4]) != 0 ||
            copy_token(orders[n].tif_code, sizeof(orders[n].tif_code), fields[5]) != 0 ||
            parse_i64(fields[6], &orders[n].ord_qty) != 0 ||
            parse_i64(fields[7], &orders[n].price_amt) != 0 ||
            parse_int_range(fields[8], 1, 3, &orders[n].instr_tier) != 0) {
            fprintf(stderr, "SCORDFの%lu行目の項目値が不正です\n", row);
            fclose(fp);
            return -1;
        }

        ++n;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCORDFの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int read_instruments(StagedInstrument *insts, size_t *count)
{
    FILE *fp = open_staged_file("SCINSTF");
    char line[MIHFT_LINE_SIZE];
    size_t n = 0U;
    unsigned long row = 0UL;

    if (fp == NULL) {
        fprintf(stderr, "SCINSTFを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *fields[MIHFT_MAX_FIELDS];
        int nf;

        ++row;
        line[strcspn(line, "\r\n")] = '\0';
        if (line[0] == '\0') {
            continue;
        }

        nf = split_csv(line, fields, MIHFT_MAX_FIELDS);
        if (nf < 0 || nf != 6) {
            fprintf(stderr, "SCINSTFの%lu行目が不正です\n", row);
            fclose(fp);
            return -1;
        }

        if (row == 1UL && is_header_line(fields[0])) {
            continue;
        }

        if (n >= MIHFT_MAX_INSTRUMENTS) {
            fprintf(stderr, "SCINSTFの件数が上限を超過しました\n");
            fclose(fp);
            return -1;
        }

        if (copy_token(insts[n].instr_code, sizeof(insts[n].instr_code), fields[0]) != 0 ||
            copy_token(insts[n].instr_name, sizeof(insts[n].instr_name), fields[1]) != 0 ||
            parse_int_range(fields[2], 1, 3, &insts[n].instr_tier) != 0 ||
            parse_i64(fields[3], &insts[n].tick_amt) != 0 ||
            parse_i64(fields[4], &insts[n].lot_qty) != 0 ||
            copy_token(insts[n].board_code, sizeof(insts[n].board_code), fields[5]) != 0) {
            fprintf(stderr, "SCINSTFの%lu行目の項目値が不正です\n", row);
            fclose(fp);
            return -1;
        }

        if (insts[n].tick_amt <= 0 || insts[n].lot_qty <= 0) {
            fprintf(stderr, "SCINSTFの%lu行目の刻みまたは売買単位が不正です\n", row);
            fclose(fp);
            return -1;
        }

        ++n;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCINSTFの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static const StagedInstrument *find_instrument(const StagedInstrument *insts, size_t count, const char *code)
{
    for (size_t i = 0U; i < count; ++i) {
        if (strcmp(insts[i].instr_code, code) == 0) {
            return &insts[i];
        }
    }
    return NULL;
}

static int canonical_tick(int tier, int64_t *tick)
{
    if (tier == 1) {
        *tick = 100;
        return 0;
    }
    if (tier == 2) {
        *tick = 500;
        return 0;
    }
    if (tier == 3) {
        *tick = 1000;
        return 0;
    }
    return -1;
}

static int validate_code_values(const StagedOrder *ord, const StagedInstrument *inst)
{
    if ((strcmp(ord->side_kbn, "B") != 0 && strcmp(ord->side_kbn, "S") != 0) ||
        (strcmp(ord->ord_type, "L") != 0 && strcmp(ord->ord_type, "M") != 0) ||
        (strcmp(ord->tif_code, "DAY") != 0 && strcmp(ord->tif_code, "IOC") != 0 && strcmp(ord->tif_code, "FOK") != 0) ||
        (strcmp(inst->board_code, "T1") != 0 && strcmp(inst->board_code, "ST") != 0 && strcmp(inst->board_code, "ETF") != 0)) {
        return -1;
    }
    return 0;
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

static const char *select_board(const StagedOrder *ord, const StagedInstrument *inst)
{
    if (strcmp(inst->board_code, "ETF") == 0) {
        return "ETF";
    }
    if (strcmp(ord->tif_code, "IOC") == 0 || strcmp(ord->tif_code, "FOK") == 0) {
        return inst->instr_tier == 3 ? "ST" : "T1";
    }
    return inst->board_code;
}

static int route_order(const StagedOrder *ord, const StagedInstrument *inst, const char **board)
{
    int64_t notional;
    int64_t canonical;

    *board = select_board(ord, inst);

    if (validate_code_values(ord, inst) != 0 || ord->instr_tier != inst->instr_tier) {
        return MIHFT_DEC_REJECT_MARGIN;
    }

    if (ord->ord_qty <= 0 || ord->ord_qty % inst->lot_qty != 0) {
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    if (strcmp(ord->ord_type, "M") == 0) {
        if (ord->price_amt != 0) {
            return MIHFT_DEC_REJECT_TICK;
        }
        return MIHFT_DEC_ACCEPT;
    }

    if (ord->price_amt <= 0) {
        return MIHFT_DEC_REJECT_TICK;
    }

    if (canonical_tick(inst->instr_tier, &canonical) != 0 || inst->tick_amt != canonical) {
        return MIHFT_DEC_REJECT_TICK;
    }

    if (ord->price_amt % inst->tick_amt != 0) {
        return MIHFT_DEC_REJECT_TICK;
    }

    if (mul_i64_checked(ord->ord_qty, ord->price_amt, &notional) != 0 || notional > MIHFT_MAX_NOTIONAL) {
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    return MIHFT_DEC_ACCEPT;
}

int main(void)
{
    StagedOrder orders[MIHFT_MAX_ORDERS];
    StagedInstrument insts[MIHFT_MAX_INSTRUMENTS];
    size_t order_count = 0U;
    size_t inst_count = 0U;
    int final_decision = MIHFT_DEC_ACCEPT;

    if (read_orders(orders, &order_count) != 0 || read_instruments(insts, &inst_count) != 0) {
        return 2;
    }

    for (size_t i = 0U; i < order_count; ++i) {
        const StagedInstrument *inst = find_instrument(insts, inst_count, orders[i].instr_code);
        const char *board = "";
        int decision;

        if (inst == NULL) {
            decision = MIHFT_DEC_REJECT_MARGIN;
        } else {
            decision = route_order(&orders[i], inst, &board);
        }

        printf("%s,%s,%s,%d\n", orders[i].order_id, orders[i].instr_code, board, decision);

        if (final_decision == MIHFT_DEC_ACCEPT && decision != MIHFT_DEC_ACCEPT) {
            final_decision = decision;
        }
    }

    return final_decision;
}
