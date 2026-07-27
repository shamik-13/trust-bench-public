/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20191022  小林 直樹 (E-252)  初版作成
 * 1.01  20200322  西村 亮 (E-204)  価格帯境界のtick丸めを追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_DECISION_ACCEPT 0
#define MIHFT_DECISION_REJECT_NOTIONAL 8
#define MIHFT_DECISION_REJECT_TICK 12
#define MIHFT_IO_ERROR 2
#define MIHFT_PARSE_ERROR 3

#define MIHFT_LINE_MAX 1024
#define MIHFT_CODE_MAX 32
#define MIHFT_NAME_MAX 128
#define MIHFT_BOARD_MAX 8
#define MIHFT_ID_MAX 40
#define MIHFT_SIDE_MAX 4
#define MIHFT_TYPE_MAX 4
#define MIHFT_TIF_MAX 8
#define MIHFT_MKT_MAX 4096
#define MIHFT_INST_MAX 4096

typedef struct {
    char instr_code[MIHFT_CODE_MAX];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    char tick_ts[MIHFT_ID_MAX];
} MihftMarketRow;

typedef struct {
    char instr_code[MIHFT_CODE_MAX];
    char instr_name[MIHFT_NAME_MAX];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[MIHFT_BOARD_MAX];
} MihftInstRow;

typedef struct {
    char order_id[MIHFT_ID_MAX];
    char cif_no[MIHFT_ID_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn[MIHFT_SIDE_MAX];
    char ord_type[MIHFT_TYPE_MAX];
    char tif_code[MIHFT_TIF_MAX];
    int64_t ord_amt;
    int64_t ord_qty;
} MihftOrderRow;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);
    if (dstsz == 0U || n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1U);
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
    if (mihft_parse_i64(s, &v) != 0 || v < 1 || v > 3) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int mihft_split_csv(char *line, char **field, size_t max_field)
{
    size_t n = 0U;
    char *p = line;

    while (n < max_field) {
        field[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    return (int)n;
}

static int mihft_tier_rate_bp(int tier, int64_t *rate_bp)
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

static int64_t mihft_floor_tick(int64_t value, int64_t tick)
{
    if (value >= 0) {
        return (value / tick) * tick;
    }
    return -(((-value + tick - 1) / tick) * tick);
}

static int64_t mihft_ceil_tick(int64_t value, int64_t tick)
{
    if (value >= 0) {
        return ((value + tick - 1) / tick) * tick;
    }
    return -(((-value) / tick) * tick);
}

static int mihft_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a != 0 && b > INT64_MAX / a) {
        return -1;
    }
    *out = a * b;
    return 0;
}

static int mihft_read_market(MihftMarketRow *rows, size_t *count)
{
    FILE *fp = fopen("SCMKTD.csv", "r");
    char line[MIHFT_LINE_MAX];
    size_t n = 0U;

    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];

        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (n >= MIHFT_MKT_MAX || mihft_split_csv(line, f, 6U) != 6) {
            fclose(fp);
            return -1;
        }
        if (strcmp(f[0], "INSTR-CODE") == 0) {
            continue;
        }
        if (mihft_copy_field(rows[n].instr_code, sizeof(rows[n].instr_code), f[0]) != 0 ||
            mihft_parse_i64(f[1], &rows[n].bid_amt) != 0 ||
            mihft_parse_i64(f[2], &rows[n].ask_amt) != 0 ||
            mihft_parse_i64(f[3], &rows[n].last_amt) != 0 ||
            mihft_parse_i64(f[4], &rows[n].vol_qty) != 0 ||
            mihft_copy_field(rows[n].tick_ts, sizeof(rows[n].tick_ts), f[5]) != 0) {
            fclose(fp);
            return -1;
        }
        n++;
    }

    if (ferror(fp) != 0) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int mihft_read_inst(MihftInstRow *rows, size_t *count)
{
    FILE *fp = fopen("SCINSTF.csv", "r");
    char line[MIHFT_LINE_MAX];
    size_t n = 0U;

    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];

        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (n >= MIHFT_INST_MAX || mihft_split_csv(line, f, 6U) != 6) {
            fclose(fp);
            return -1;
        }
        if (strcmp(f[0], "INSTR-CODE") == 0) {
            continue;
        }
        if (mihft_copy_field(rows[n].instr_code, sizeof(rows[n].instr_code), f[0]) != 0 ||
            mihft_copy_field(rows[n].instr_name, sizeof(rows[n].instr_name), f[1]) != 0 ||
            mihft_parse_int(f[2], &rows[n].instr_tier) != 0 ||
            mihft_parse_i64(f[3], &rows[n].tick_amt) != 0 ||
            mihft_parse_i64(f[4], &rows[n].lot_qty) != 0 ||
            mihft_copy_field(rows[n].board_code, sizeof(rows[n].board_code), f[5]) != 0) {
            fclose(fp);
            return -1;
        }
        n++;
    }

    if (ferror(fp) != 0) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static const MihftMarketRow *mihft_find_market(const MihftMarketRow *rows, size_t count, const char *code)
{
    size_t i;
    for (i = 0U; i < count; i++) {
        if (strcmp(rows[i].instr_code, code) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static const MihftInstRow *mihft_find_inst(const MihftInstRow *rows, size_t count, const char *code)
{
    size_t i;
    for (i = 0U; i < count; i++) {
        if (strcmp(rows[i].instr_code, code) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static int mihft_parse_order(char *line, MihftOrderRow *row)
{
    char *f[8];

    mihft_chomp(line);
    if (mihft_split_csv(line, f, 8U) != 8) {
        return -1;
    }
    if (mihft_copy_field(row->order_id, sizeof(row->order_id), f[0]) != 0 ||
        mihft_copy_field(row->cif_no, sizeof(row->cif_no), f[1]) != 0 ||
        mihft_copy_field(row->instr_code, sizeof(row->instr_code), f[2]) != 0 ||
        mihft_copy_field(row->side_kbn, sizeof(row->side_kbn), f[3]) != 0 ||
        mihft_copy_field(row->ord_type, sizeof(row->ord_type), f[4]) != 0 ||
        mihft_copy_field(row->tif_code, sizeof(row->tif_code), f[5]) != 0 ||
        mihft_parse_i64(f[6], &row->ord_amt) != 0 ||
        mihft_parse_i64(f[7], &row->ord_qty) != 0) {
        return -1;
    }
    return 0;
}

static int mihft_judge_order(const MihftOrderRow *ord,
                             const MihftMarketRow *mkt,
                             const MihftInstRow *inst)
{
    int64_t rate_bp;
    int64_t width;
    int64_t lower;
    int64_t upper;
    int64_t notional;

    if (mkt == NULL || inst == NULL || inst->tick_amt <= 0 || mkt->last_amt <= 0 ||
        ord->ord_qty <= 0 || ord->ord_amt <= 0) {
        return MIHFT_DECISION_REJECT_TICK;
    }

    if (mihft_tier_rate_bp(inst->instr_tier, &rate_bp) != 0) {
        return MIHFT_DECISION_REJECT_TICK;
    }

    if (mihft_mul_i64(mkt->last_amt, rate_bp, &width) != 0) {
        return MIHFT_DECISION_REJECT_NOTIONAL;
    }
    width /= 10000;

    lower = mihft_ceil_tick(mkt->last_amt - width, inst->tick_amt);
    upper = mihft_floor_tick(mkt->last_amt + width, inst->tick_amt);

    if (strcmp(ord->ord_type, "M") == 0) {
        return MIHFT_DECISION_ACCEPT;
    }

    if (strcmp(ord->ord_type, "L") != 0 ||
        strcmp(ord->side_kbn, "B") != 0 && strcmp(ord->side_kbn, "S") != 0) {
        return MIHFT_DECISION_REJECT_TICK;
    }

    if ((ord->ord_amt % inst->tick_amt) != 0) {
        return MIHFT_DECISION_REJECT_TICK;
    }

    if (ord->ord_amt < lower || ord->ord_amt > upper) {
        return MIHFT_DECISION_REJECT_TICK;
    }

    if (mihft_mul_i64(ord->ord_amt, ord->ord_qty, &notional) != 0 ||
        notional > (int64_t)MIHFT_MAX_NOTIONAL) {
        return MIHFT_DECISION_REJECT_NOTIONAL;
    }

    return MIHFT_DECISION_ACCEPT;
}

static int mihft_write_reject(FILE *out,
                              uint64_t reject_seq,
                              const MihftOrderRow *ord,
                              int reject_code,
                              const char *reject_ts)
{
    if (fprintf(out, "RJ%012" PRIu64 ",%s,%s,%s,%d,%s\n",
                reject_seq,
                ord->order_id,
                ord->cif_no,
                ord->instr_code,
                reject_code,
                reject_ts) < 0) {
        return -1;
    }
    return 0;
}

int main(void)
{
    MihftMarketRow markets[MIHFT_MKT_MAX];
    MihftInstRow insts[MIHFT_INST_MAX];
    size_t market_count = 0U;
    size_t inst_count = 0U;
    FILE *orders;
    FILE *rejects;
    char line[MIHFT_LINE_MAX];
    uint64_t reject_seq = 1U;
    int final_decision = MIHFT_DECISION_ACCEPT;

    if (mihft_read_market(markets, &market_count) != 0) {
        return MIHFT_IO_ERROR;
    }
    if (mihft_read_inst(insts, &inst_count) != 0) {
        return MIHFT_IO_ERROR;
    }

    orders = fopen("SCORDR.csv", "r");
    if (orders == NULL) {
        return MIHFT_IO_ERROR;
    }

    rejects = fopen("SCREJTF.dat", "w");
    if (rejects == NULL) {
        fclose(orders);
        return MIHFT_IO_ERROR;
    }

    while (fgets(line, sizeof(line), orders) != NULL) {
        MihftOrderRow ord;
        const MihftMarketRow *mkt;
        const MihftInstRow *inst;
        int decision;

        if (line[0] == '\n' || line[0] == '\r') {
            continue;
        }
        if (strncmp(line, "ORDER-ID,", 9U) == 0) {
            continue;
        }

        if (mihft_parse_order(line, &ord) != 0) {
            fclose(rejects);
            fclose(orders);
            return MIHFT_PARSE_ERROR;
        }

        mkt = mihft_find_market(markets, market_count, ord.instr_code);
        inst = mihft_find_inst(insts, inst_count, ord.instr_code);
        decision = mihft_judge_order(&ord, mkt, inst);

        if (decision != MIHFT_DECISION_ACCEPT) {
            const char *ts = (mkt != NULL) ? mkt->tick_ts : "00000000000000";
            if (mihft_write_reject(rejects, reject_seq++, &ord, decision, ts) != 0) {
                fclose(rejects);
                fclose(orders);
                return MIHFT_IO_ERROR;
            }
            final_decision = decision;
        }
    }

    if (ferror(orders) != 0 || fclose(rejects) != 0) {
        fclose(orders);
        return MIHFT_IO_ERROR;
    }

    fclose(orders);
    return final_decision;
}
