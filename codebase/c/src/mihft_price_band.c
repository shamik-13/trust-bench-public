/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20230418  渡辺 隆 (E-260)  価格帯・呼値検証の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define SCORDF_PATH  "SCORDF.csv"
#define SCMKTD_PATH  "SCMKTD.csv"
#define SCINSTF_PATH "SCINSTF.csv"
#define HFRJCT_PATH  "HFRJCT.csv"

#define DECISION_ACCEPT          0
#define DECISION_REJECT_MARGIN   4
#define DECISION_REJECT_NOTIONAL 8
#define DECISION_REJECT_TICK     12

#define DETAIL_PARSE       9001
#define DETAIL_IO          9002
#define DETAIL_MASTER      9101
#define DETAIL_BOARD       9102
#define DETAIL_TICK_UNIT   1201
#define DETAIL_PRICE_BAND  1202
#define DETAIL_NO_QUOTE    1203
#define DETAIL_NOTIONAL    8001

#define MAX_LINE_LEN 1024
#define MAX_ORDERS   4096
#define MAX_MARKETS  4096
#define MAX_INSTRS   4096

typedef struct {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[8];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} OrderRec;

typedef struct {
    char instr_code[32];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    char tick_ts[32];
} MarketRec;

typedef struct {
    char instr_code[32];
    char instr_name[96];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[8];
} InstrRec;

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t n;

    if (dst_len == 0) {
        return -1;
    }

    n = strlen(src);
    while (n > 0 && (src[n - 1] == '\n' || src[n - 1] == '\r')) {
        n--;
    }
    while (n > 0 && src[0] == ' ') {
        src++;
        n--;
    }
    while (n > 0 && src[n - 1] == ' ') {
        n--;
    }

    if (n >= dst_len) {
        return -1;
    }

    memcpy(dst, src, n);
    dst[n] = '\0';
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *endp;
    long long v;

    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || endp == s) {
        return -1;
    }
    while (*endp == ' ' || *endp == '\r' || *endp == '\n') {
        endp++;
    }
    if (*endp != '\0') {
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
    size_t n = 0;
    char *p = line;

    while (n < want) {
        cols[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p = '\0';
        p++;
    }

    return n == want && strchr(cols[want - 1], ',') == NULL ? 0 : -1;
}

static int same_header(const char *line, const char *first_name)
{
    return strncmp(line, first_name, strlen(first_name)) == 0;
}

static int read_orders(OrderRec *orders, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MAX_LINE_LEN];
    size_t n = 0;

    fp = fopen(SCORDF_PATH, "r");
    if (fp == NULL) {
        fprintf(stderr, "注文ファイルを開けません: %s\n", SCORDF_PATH);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[9];

        if (n == 0 && same_header(line, "ORDER-ID")) {
            continue;
        }
        if (n >= cap || split_csv(line, cols, 9) != 0) {
            fclose(fp);
            return -1;
        }

        if (copy_field(orders[n].order_id, sizeof(orders[n].order_id), cols[0]) != 0 ||
            copy_field(orders[n].cif_no, sizeof(orders[n].cif_no), cols[1]) != 0 ||
            copy_field(orders[n].instr_code, sizeof(orders[n].instr_code), cols[2]) != 0 ||
            copy_field(orders[n].tif_code, sizeof(orders[n].tif_code), cols[5]) != 0 ||
            parse_i64(cols[6], &orders[n].ord_qty) != 0 ||
            parse_i64(cols[7], &orders[n].price_amt) != 0 ||
            parse_int(cols[8], &orders[n].instr_tier) != 0) {
            fclose(fp);
            return -1;
        }

        orders[n].side_kbn = cols[3][0];
        orders[n].ord_type = cols[4][0];
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int read_markets(MarketRec *markets, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MAX_LINE_LEN];
    size_t n = 0;

    fp = fopen(SCMKTD_PATH, "r");
    if (fp == NULL) {
        fprintf(stderr, "気配ファイルを開けません: %s\n", SCMKTD_PATH);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[6];

        if (n == 0 && same_header(line, "INSTR-CODE")) {
            continue;
        }
        if (n >= cap || split_csv(line, cols, 6) != 0) {
            fclose(fp);
            return -1;
        }

        if (copy_field(markets[n].instr_code, sizeof(markets[n].instr_code), cols[0]) != 0 ||
            parse_i64(cols[1], &markets[n].bid_amt) != 0 ||
            parse_i64(cols[2], &markets[n].ask_amt) != 0 ||
            parse_i64(cols[3], &markets[n].last_amt) != 0 ||
            parse_i64(cols[4], &markets[n].vol_qty) != 0 ||
            copy_field(markets[n].tick_ts, sizeof(markets[n].tick_ts), cols[5]) != 0) {
            fclose(fp);
            return -1;
        }

        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int read_instrs(InstrRec *instrs, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MAX_LINE_LEN];
    size_t n = 0;

    fp = fopen(SCINSTF_PATH, "r");
    if (fp == NULL) {
        fprintf(stderr, "銘柄ファイルを開けません: %s\n", SCINSTF_PATH);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[6];

        if (n == 0 && same_header(line, "INSTR-CODE")) {
            continue;
        }
        if (n >= cap || split_csv(line, cols, 6) != 0) {
            fclose(fp);
            return -1;
        }

        if (copy_field(instrs[n].instr_code, sizeof(instrs[n].instr_code), cols[0]) != 0 ||
            copy_field(instrs[n].instr_name, sizeof(instrs[n].instr_name), cols[1]) != 0 ||
            parse_int(cols[2], &instrs[n].instr_tier) != 0 ||
            parse_i64(cols[3], &instrs[n].tick_amt) != 0 ||
            parse_i64(cols[4], &instrs[n].lot_qty) != 0 ||
            copy_field(instrs[n].board_code, sizeof(instrs[n].board_code), cols[5]) != 0) {
            fclose(fp);
            return -1;
        }

        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static const MarketRec *find_market(const MarketRec *markets, size_t count, const char *code)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(markets[i].instr_code, code) == 0) {
            return &markets[i];
        }
    }
    return NULL;
}

static const InstrRec *find_instr(const InstrRec *instrs, size_t count, const char *code)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(instrs[i].instr_code, code) == 0) {
            return &instrs[i];
        }
    }
    return NULL;
}

static int tier_rate_bp(int tier)
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

static int board_code_ok(const char *board_code)
{
    return strcmp(board_code, "T1") == 0 ||
           strcmp(board_code, "ST") == 0 ||
           strcmp(board_code, "ETF") == 0;
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

static int band_for_order(const MarketRec *mkt, int rate_bp, int64_t *lo, int64_t *hi)
{
    int64_t ref;
    int64_t width;

    if (mkt->last_amt > 0) {
        ref = mkt->last_amt;
    } else if (mkt->bid_amt > 0 && mkt->ask_amt > 0 && mkt->bid_amt <= mkt->ask_amt) {
        if (mkt->bid_amt > INT64_MAX - mkt->ask_amt) {
            return -1;
        }
        ref = (mkt->bid_amt + mkt->ask_amt) / 2;
    } else {
        return -1;
    }

    if (mul_over_i64(ref, (int64_t)rate_bp, &width) != 0) {
        return -1;
    }
    width /= 10000;
    if (width <= 0) {
        width = 1;
    }

    *lo = ref > width ? ref - width : 0;
    if (ref > INT64_MAX - width) {
        return -1;
    }
    *hi = ref + width;
    return 0;
}

static void reject_ts(char *buf, size_t len)
{
    time_t now;
    struct tm *tmv;

    now = time(NULL);
    tmv = localtime(&now);
    if (tmv == NULL || strftime(buf, len, "%Y%m%d%H%M%S", tmv) == 0) {
        copy_field(buf, len, "00000000000000");
    }
}

static int write_reject(FILE *out, uint64_t reject_id, const OrderRec *ord, int reject_cd, int detail_cd)
{
    char ts[20];

    reject_ts(ts, sizeof(ts));
    if (fprintf(out, "RJ%012" PRIu64 ",%s,%s,%s,%d,%d,%s\n",
                reject_id,
                ord->order_id,
                ord->cif_no,
                ord->instr_code,
                reject_cd,
                detail_cd,
                ts) < 0) {
        return -1;
    }
    return 0;
}

static int validate_order(const OrderRec *ord,
                          const InstrRec *instrs,
                          size_t instr_count,
                          const MarketRec *markets,
                          size_t market_count,
                          int *detail_cd)
{
    const InstrRec *inst;
    const MarketRec *mkt;
    int rate_bp;
    int64_t band_lo;
    int64_t band_hi;
    int64_t notional;

    inst = find_instr(instrs, instr_count, ord->instr_code);
    mkt = find_market(markets, market_count, ord->instr_code);
    if (inst == NULL || mkt == NULL) {
        *detail_cd = DETAIL_MASTER;
        return DECISION_REJECT_TICK;
    }

    if (inst->instr_tier != ord->instr_tier || inst->tick_amt <= 0 ||
        inst->lot_qty <= 0 || !board_code_ok(inst->board_code)) {
        *detail_cd = DETAIL_BOARD;
        return DECISION_REJECT_TICK;
    }

    rate_bp = tier_rate_bp(inst->instr_tier);
    if (rate_bp == 0) {
        *detail_cd = DETAIL_BOARD;
        return DECISION_REJECT_MARGIN;
    }

    if (ord->ord_qty <= 0 || ord->ord_qty % inst->lot_qty != 0) {
        *detail_cd = DETAIL_NOTIONAL;
        return DECISION_REJECT_NOTIONAL;
    }

    if (ord->ord_type == 'M') {
        if (mkt->bid_amt <= 0 || mkt->ask_amt <= 0 || mkt->bid_amt > mkt->ask_amt) {
            *detail_cd = DETAIL_NO_QUOTE;
            return DECISION_REJECT_TICK;
        }
        *detail_cd = 0;
        return DECISION_ACCEPT;
    }

    if (ord->ord_type != 'L' || ord->price_amt <= 0) {
        *detail_cd = DETAIL_TICK_UNIT;
        return DECISION_REJECT_TICK;
    }

    if (ord->price_amt % inst->tick_amt != 0) {
        *detail_cd = DETAIL_TICK_UNIT;
        return DECISION_REJECT_TICK;
    }

    if (band_for_order(mkt, rate_bp, &band_lo, &band_hi) != 0 ||
        ord->price_amt < band_lo || ord->price_amt > band_hi) {
        *detail_cd = DETAIL_PRICE_BAND;
        return DECISION_REJECT_TICK;
    }

    if (mul_over_i64(ord->ord_qty, ord->price_amt, &notional) != 0 ||
        notional > MIHFT_MAX_NOTIONAL) {
        *detail_cd = DETAIL_NOTIONAL;
        return DECISION_REJECT_NOTIONAL;
    }

    *detail_cd = 0;
    return DECISION_ACCEPT;
}

int main(void)
{
    OrderRec orders[MAX_ORDERS];
    MarketRec markets[MAX_MARKETS];
    InstrRec instrs[MAX_INSTRS];
    size_t order_count = 0;
    size_t market_count = 0;
    size_t instr_count = 0;
    size_t i;
    FILE *reject_fp;
    uint64_t reject_id = 1;
    int final_decision = DECISION_ACCEPT;

    if (read_orders(orders, MAX_ORDERS, &order_count) != 0 ||
        read_markets(markets, MAX_MARKETS, &market_count) != 0 ||
        read_instrs(instrs, MAX_INSTRS, &instr_count) != 0) {
        fprintf(stderr, "入力解析に失敗しました\n");
        return DETAIL_PARSE;
    }

    reject_fp = fopen(HFRJCT_PATH, "w");
    if (reject_fp == NULL) {
        fprintf(stderr, "拒否ファイルを作成できません: %s\n", HFRJCT_PATH);
        return DETAIL_IO;
    }

    if (fprintf(reject_fp, "REJECT-ID,ORDER-ID,CIF-NO,INSTR-CODE,REJECT-CD,DETAIL-CD,REJECT-TS\n") < 0) {
        fclose(reject_fp);
        fprintf(stderr, "拒否ファイルの書込に失敗しました\n");
        return DETAIL_IO;
    }

    for (i = 0; i < order_count; i++) {
        int detail_cd;
        int decision;

        decision = validate_order(&orders[i], instrs, instr_count, markets, market_count, &detail_cd);
        if (decision != DECISION_ACCEPT) {
            if (write_reject(reject_fp, reject_id++, &orders[i], decision, detail_cd) != 0) {
                fclose(reject_fp);
                fprintf(stderr, "拒否レコードの書込に失敗しました\n");
                return DETAIL_IO;
            }
            if (final_decision == DECISION_ACCEPT || decision > final_decision) {
                final_decision = decision;
            }
        }
    }

    if (fclose(reject_fp) != 0) {
        fprintf(stderr, "拒否ファイルのクローズに失敗しました\n");
        return DETAIL_IO;
    }

    return final_decision;
}
