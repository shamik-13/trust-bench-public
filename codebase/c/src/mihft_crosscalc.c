/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20190416  福田 亮太 (E-211)     寄付クロス価格計算の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_MAX_LEVELS 128
#define MIHFT_LINE_SIZE 1024
#define MIHFT_CODE_SIZE 32
#define MIHFT_NAME_SIZE 96
#define MIHFT_TIER_1 1
#define MIHFT_TIER_2 2
#define MIHFT_TIER_3 3

typedef struct {
    char instr_code[MIHFT_CODE_SIZE];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int order_cnt;
    int64_t entry_ts;
} MihftBookRec;

typedef struct {
    char instr_code[MIHFT_CODE_SIZE];
    char instr_name[MIHFT_NAME_SIZE];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[8];
} MihftInstRec;

typedef struct {
    int64_t price_amt;
    int64_t bid_qty;
    int64_t ask_qty;
} MihftPricePoint;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_split_csv(char *line, char **cols, size_t max_cols)
{
    size_t n = 0;
    char *p = line;

    while (n < max_cols) {
        cols[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return (int)n;
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

static int mihft_copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n;

    if (dstsz == 0 || src == NULL) {
        return -1;
    }
    n = strlen(src);
    if (n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int mihft_board_ok(const char *board_code)
{
    return strcmp(board_code, "T1") == 0 ||
           strcmp(board_code, "ST") == 0 ||
           strcmp(board_code, "ETF") == 0;
}

static int mihft_read_inst(MihftInstRec *inst)
{
    FILE *fp = fopen("SCINSTF.csv", "r");
    char line[MIHFT_LINE_SIZE];
    char *cols[6];

    if (fp == NULL) {
        fprintf(stderr, "SCINSTF入力オープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        mihft_chomp(line);
        if (line[0] == '\0' || strncmp(line, "INSTR-CODE,", 11) == 0) {
            continue;
        }
        if (mihft_split_csv(line, cols, 6) != 6 ||
            mihft_copy_field(inst->instr_code, sizeof(inst->instr_code), cols[0]) != 0 ||
            mihft_copy_field(inst->instr_name, sizeof(inst->instr_name), cols[1]) != 0 ||
            mihft_parse_int(cols[2], &inst->instr_tier) != 0 ||
            mihft_parse_i64(cols[3], &inst->tick_amt) != 0 ||
            mihft_parse_i64(cols[4], &inst->lot_qty) != 0 ||
            mihft_copy_field(inst->board_code, sizeof(inst->board_code), cols[5]) != 0) {
            fclose(fp);
            fprintf(stderr, "SCINSTF解析失敗\n");
            return -1;
        }
        fclose(fp);
        return 0;
    }

    fclose(fp);
    fprintf(stderr, "SCINSTF対象なし\n");
    return -1;
}

static int mihft_add_point(MihftPricePoint *points, size_t *count, int64_t price, char side, int64_t qty)
{
    size_t i;

    for (i = 0; i < *count; i++) {
        if (points[i].price_amt == price) {
            if (side == 'B') {
                if (points[i].bid_qty > INT64_MAX - qty) {
                    return -1;
                }
                points[i].bid_qty += qty;
            } else {
                if (points[i].ask_qty > INT64_MAX - qty) {
                    return -1;
                }
                points[i].ask_qty += qty;
            }
            return 0;
        }
    }

    if (*count >= MIHFT_MAX_LEVELS) {
        return -1;
    }
    points[*count].price_amt = price;
    points[*count].bid_qty = side == 'B' ? qty : 0;
    points[*count].ask_qty = side == 'S' ? qty : 0;
    (*count)++;
    return 0;
}

static int mihft_read_book(const char *target_code, MihftPricePoint *points, size_t *point_count)
{
    FILE *fp = fopen("SCBOOK.csv", "r");
    char line[MIHFT_LINE_SIZE];
    char *cols[7];

    if (fp == NULL) {
        fprintf(stderr, "SCBOOK入力オープン失敗\n");
        return -1;
    }

    *point_count = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        MihftBookRec rec;

        mihft_chomp(line);
        if (line[0] == '\0' || strncmp(line, "INSTR-CODE,", 11) == 0) {
            continue;
        }
        if (mihft_split_csv(line, cols, 7) != 7 ||
            mihft_copy_field(rec.instr_code, sizeof(rec.instr_code), cols[0]) != 0 ||
            strlen(cols[1]) != 1 ||
            mihft_parse_int(cols[2], &rec.level_cnt) != 0 ||
            mihft_parse_i64(cols[3], &rec.price_amt) != 0 ||
            mihft_parse_i64(cols[4], &rec.book_qty) != 0 ||
            mihft_parse_int(cols[5], &rec.order_cnt) != 0 ||
            mihft_parse_i64(cols[6], &rec.entry_ts) != 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOK解析失敗\n");
            return -1;
        }

        rec.side_kbn = cols[1][0];
        if (strcmp(rec.instr_code, target_code) != 0) {
            continue;
        }
        if ((rec.side_kbn != 'B' && rec.side_kbn != 'S') ||
            rec.level_cnt < 1 || rec.level_cnt > MIHFT_MAX_LEVELS ||
            rec.price_amt <= 0 || rec.book_qty < 0 || rec.order_cnt < 0 ||
            rec.entry_ts <= 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOK値域不正\n");
            return -1;
        }
        if (rec.book_qty > 0 &&
            mihft_add_point(points, point_count, rec.price_amt, rec.side_kbn, rec.book_qty) != 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOK気配集約失敗\n");
            return -1;
        }
    }

    fclose(fp);
    if (*point_count == 0) {
        fprintf(stderr, "SCBOOK対象気配なし\n");
        return -1;
    }
    return 0;
}

static int mihft_cmp_price(const void *a, const void *b)
{
    const MihftPricePoint *pa = (const MihftPricePoint *)a;
    const MihftPricePoint *pb = (const MihftPricePoint *)b;

    if (pa->price_amt < pb->price_amt) {
        return -1;
    }
    if (pa->price_amt > pb->price_amt) {
        return 1;
    }
    return 0;
}

static int mihft_tier_tick_ok(int tier, int64_t tick_amt)
{
    if (tier == MIHFT_TIER_1) {
        return tick_amt == 100;
    }
    if (tier == MIHFT_TIER_2) {
        return tick_amt == 500;
    }
    if (tier == MIHFT_TIER_3) {
        return tick_amt == 1000;
    }
    return 0;
}

static int mihft_select_cross(const MihftInstRec *inst,
                              MihftPricePoint *points,
                              size_t point_count,
                              int64_t *cross_amt,
                              int64_t *match_qty,
                              int64_t *imbal_qty)
{
    int found = 0;
    size_t i;

    qsort(points, point_count, sizeof(points[0]), mihft_cmp_price);

    for (i = 0; i < point_count; i++) {
        size_t j;
        int64_t buy_cum = 0;
        int64_t sell_cum = 0;
        int64_t exec_qty;
        int64_t resid;

        for (j = 0; j < point_count; j++) {
            if (points[j].price_amt >= points[i].price_amt) {
                if (buy_cum > INT64_MAX - points[j].bid_qty) {
                    return 8;
                }
                buy_cum += points[j].bid_qty;
            }
            if (points[j].price_amt <= points[i].price_amt) {
                if (sell_cum > INT64_MAX - points[j].ask_qty) {
                    return 8;
                }
                sell_cum += points[j].ask_qty;
            }
        }

        exec_qty = buy_cum < sell_cum ? buy_cum : sell_cum;
        resid = buy_cum >= sell_cum ? buy_cum - sell_cum : sell_cum - buy_cum;

        if (exec_qty <= 0) {
            continue;
        }
        if (points[i].price_amt % inst->tick_amt != 0) {
            return 12;
        }
        if (exec_qty > INT64_MAX / points[i].price_amt ||
            points[i].price_amt * exec_qty > MIHFT_MAX_NOTIONAL) {
            return 8;
        }
        if (exec_qty % inst->lot_qty != 0) {
            return 12;
        }

        if (!found ||
            exec_qty > *match_qty ||
            (exec_qty == *match_qty && resid < *imbal_qty) ||
            (exec_qty == *match_qty && resid == *imbal_qty && points[i].price_amt < *cross_amt)) {
            *cross_amt = points[i].price_amt;
            *match_qty = exec_qty;
            *imbal_qty = resid;
            found = 1;
        }
    }

    return found ? 0 : 8;
}

static int mihft_write_auction(const MihftInstRec *inst,
                               int64_t cross_amt,
                               int64_t match_qty,
                               int64_t imbal_qty)
{
    FILE *fp = fopen("SCAUCT.csv", "w");
    time_t now = time(NULL);

    if (fp == NULL || now == (time_t)-1) {
        if (fp != NULL) {
            fclose(fp);
        }
        fprintf(stderr, "SCAUCT出力開始失敗\n");
        return -1;
    }

    if (fprintf(fp, "INSTR-CODE,AUCTION-KBN,CROSS-AMT,IMBAL-QTY,MATCH-QTY,CALC-TS\n") < 0 ||
        fprintf(fp, "%s,O,%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
                inst->instr_code, cross_amt, imbal_qty, match_qty, (int64_t)now) < 0) {
        fclose(fp);
        fprintf(stderr, "SCAUCT出力書込失敗\n");
        return -1;
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCAUCT出力クローズ失敗\n");
        return -1;
    }
    return 0;
}

int main(void)
{
    MihftInstRec inst;
    MihftPricePoint points[MIHFT_MAX_LEVELS];
    size_t point_count = 0;
    int64_t cross_amt = 0;
    int64_t match_qty = 0;
    int64_t imbal_qty = 0;
    int rc;

    if (mihft_read_inst(&inst) != 0) {
        return 16;
    }

    if (!mihft_tier_tick_ok(inst.instr_tier, inst.tick_amt) ||
        inst.lot_qty <= 0 ||
        !mihft_board_ok(inst.board_code)) {
        fprintf(stderr, "SCINSTF銘柄属性不正\n");
        return 12;
    }

    if (mihft_read_book(inst.instr_code, points, &point_count) != 0) {
        return 16;
    }

    rc = mihft_select_cross(&inst, points, point_count, &cross_amt, &match_qty, &imbal_qty);
    if (rc != 0) {
        return rc;
    }

    if (mihft_write_auction(&inst, cross_amt, match_qty, imbal_qty) != 0) {
        return 16;
    }

    return 0;
}
