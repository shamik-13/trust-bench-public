/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20210715  市場基盤部  呼値検証ホットパス初版
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_OK 0
#define MIHFT_ERR_IO 2
#define MIHFT_ERR_PARSE 3
#define MIHFT_MAX_LINE 512
#define MIHFT_MAX_INST 4096
#define MIHFT_MAX_ORD 8192
#define MIHFT_DEC_ACCEPT 0
#define MIHFT_DEC_REJECT_NOTIONAL 8
#define MIHFT_DEC_REJECT_TICK 12

struct inst_row {
    char instr_code[32];
    char instr_name[96];
    int instr_tier;
    int64_t tick_amt_x100;
    int64_t lot_qty;
    char board_code[8];
};

struct order_row {
    char order_id[32];
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    int64_t price_x100;
    int64_t qty;
};

static int trim_eol(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
    return n > 0;
}

static int next_field(char **cur, char *out, size_t out_sz)
{
    char *p = *cur;
    char *q;
    size_t len;

    if (p == NULL || out_sz == 0) {
        return 0;
    }

    q = strchr(p, ',');
    if (q != NULL) {
        len = (size_t)(q - p);
        *cur = q + 1;
    } else {
        len = strlen(p);
        *cur = NULL;
    }

    if (len >= out_sz) {
        return 0;
    }

    memcpy(out, p, len);
    out[len] = '\0';
    return 1;
}

static int parse_i64(const char *s, int64_t *v)
{
    char *endp;
    long long x;

    if (s == NULL || *s == '\0') {
        return 0;
    }

    errno = 0;
    x = strtoll(s, &endp, 10);
    if (errno != 0 || *endp != '\0') {
        return 0;
    }

    *v = (int64_t)x;
    return 1;
}

static int parse_i32(const char *s, int *v)
{
    int64_t x;

    if (!parse_i64(s, &x) || x < INT32_MIN || x > INT32_MAX) {
        return 0;
    }

    *v = (int)x;
    return 1;
}

static int is_header_line(const char *line, const char *first_name)
{
    return strncmp(line, first_name, strlen(first_name)) == 0;
}

static int load_instruments(const char *path, struct inst_row *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "入力ファイルを開けません:%s\n", path);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *cur;
        char tier_s[16];
        char tick_s[32];
        char lot_s[32];

        if (!trim_eol(line) || is_header_line(line, "INSTR-CODE")) {
            continue;
        }
        if (n >= cap) {
            fclose(fp);
            fprintf(stderr, "銘柄件数が上限を超過しました\n");
            return MIHFT_ERR_PARSE;
        }

        cur = line;
        if (!next_field(&cur, rows[n].instr_code, sizeof rows[n].instr_code) ||
            !next_field(&cur, rows[n].instr_name, sizeof rows[n].instr_name) ||
            !next_field(&cur, tier_s, sizeof tier_s) ||
            !next_field(&cur, tick_s, sizeof tick_s) ||
            !next_field(&cur, lot_s, sizeof lot_s) ||
            !next_field(&cur, rows[n].board_code, sizeof rows[n].board_code) ||
            !parse_i32(tier_s, &rows[n].instr_tier) ||
            !parse_i64(tick_s, &rows[n].tick_amt_x100) ||
            !parse_i64(lot_s, &rows[n].lot_qty) ||
            rows[n].tick_amt_x100 <= 0 ||
            rows[n].lot_qty <= 0) {
            fclose(fp);
            fprintf(stderr, "銘柄CSVの形式が不正です\n");
            return MIHFT_ERR_PARSE;
        }

        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "銘柄CSVの読込に失敗しました\n");
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    *count = n;
    return MIHFT_OK;
}

static int load_orders(const char *path, struct order_row *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "入力ファイルを開けません:%s\n", path);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *cur;
        char side_s[4];
        char type_s[4];
        char price_s[32];
        char qty_s[32];

        if (!trim_eol(line) || is_header_line(line, "ORDER-ID")) {
            continue;
        }
        if (n >= cap) {
            fclose(fp);
            fprintf(stderr, "注文件数が上限を超過しました\n");
            return MIHFT_ERR_PARSE;
        }

        cur = line;
        if (!next_field(&cur, rows[n].order_id, sizeof rows[n].order_id) ||
            !next_field(&cur, rows[n].instr_code, sizeof rows[n].instr_code) ||
            !next_field(&cur, side_s, sizeof side_s) ||
            !next_field(&cur, type_s, sizeof type_s) ||
            !next_field(&cur, rows[n].tif_code, sizeof rows[n].tif_code) ||
            !next_field(&cur, price_s, sizeof price_s) ||
            !next_field(&cur, qty_s, sizeof qty_s) ||
            !parse_i64(price_s, &rows[n].price_x100) ||
            !parse_i64(qty_s, &rows[n].qty) ||
            (side_s[0] != 'B' && side_s[0] != 'S') ||
            side_s[1] != '\0' ||
            (type_s[0] != 'L' && type_s[0] != 'M') ||
            type_s[1] != '\0' ||
            rows[n].price_x100 < 0 ||
            rows[n].qty <= 0) {
            fclose(fp);
            fprintf(stderr, "注文CSVの形式が不正です\n");
            return MIHFT_ERR_PARSE;
        }

        rows[n].side_kbn = side_s[0];
        rows[n].ord_type = type_s[0];
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "注文CSVの読込に失敗しました\n");
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    *count = n;
    return MIHFT_OK;
}

static const struct inst_row *find_inst(const struct inst_row *rows, size_t n, const char *code)
{
    size_t i;

    for (i = 0; i < n; i++) {
        if (strcmp(rows[i].instr_code, code) == 0) {
            return &rows[i];
        }
    }

    return NULL;
}

static int notional_over_limit(int64_t price_x100, int64_t qty)
{
    int64_t limit_x100 = (int64_t)MIHFT_MAX_NOTIONAL * 100;

    if (price_x100 == 0) {
        return 0;
    }
    if (qty > INT64_MAX / price_x100) {
        return 1;
    }

    return price_x100 * qty > limit_x100;
}

static int decide_order(const struct order_row *ord, const struct inst_row *inst)
{
    if (inst == NULL) {
        return MIHFT_DEC_REJECT_TICK;
    }
    if (ord->ord_type == 'L' && ord->price_x100 % inst->tick_amt_x100 != 0) {
        return MIHFT_DEC_REJECT_TICK;
    }
    if (ord->qty % inst->lot_qty != 0) {
        return MIHFT_DEC_REJECT_TICK;
    }
    if (notional_over_limit(ord->price_x100, ord->qty)) {
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    return MIHFT_DEC_ACCEPT;
}

static void make_ts(char *out, size_t out_sz)
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

    strftime(out, out_sz, "%Y%m%d%H%M%S", &tmv);
}

int main(void)
{
    struct inst_row insts[MIHFT_MAX_INST];
    struct order_row orders[MIHFT_MAX_ORD];
    size_t inst_count = 0;
    size_t order_count = 0;
    FILE *out;
    size_t i;
    int rc;
    int final_code = MIHFT_DEC_ACCEPT;

    rc = load_instruments("SCINSTF.csv", insts, MIHFT_MAX_INST, &inst_count);
    if (rc != MIHFT_OK) {
        return rc;
    }

    rc = load_orders("HFORDER.csv", orders, MIHFT_MAX_ORD, &order_count);
    if (rc != MIHFT_OK) {
        return rc;
    }

    out = fopen("HFDECLOG.csv", "w");
    if (out == NULL) {
        fprintf(stderr, "判定ログを開けません\n");
        return MIHFT_ERR_IO;
    }

    if (fprintf(out, "DECISION-ID,ORDER-ID,INSTR-CODE,ACTION-CODE,REASON-CODE,DECISION-TS\n") < 0) {
        fclose(out);
        fprintf(stderr, "判定ログの書込に失敗しました\n");
        return MIHFT_ERR_IO;
    }

    for (i = 0; i < order_count; i++) {
        const struct inst_row *inst = find_inst(insts, inst_count, orders[i].instr_code);
        int decision = decide_order(&orders[i], inst);
        char ts[16];

        make_ts(ts, sizeof ts);
        if (fprintf(out, "D%010zu,%s,%s,%d,%d,%s\n",
                    i + 1,
                    orders[i].order_id,
                    orders[i].instr_code,
                    decision == MIHFT_DEC_ACCEPT ? 0 : 1,
                    decision,
                    ts) < 0) {
            fclose(out);
            fprintf(stderr, "判定ログの書込に失敗しました\n");
            return MIHFT_ERR_IO;
        }

        if (final_code == MIHFT_DEC_ACCEPT && decision != MIHFT_DEC_ACCEPT) {
            final_code = decision;
        }
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "判定ログのクローズに失敗しました\n");
        return MIHFT_ERR_IO;
    }

    return final_code;
}
