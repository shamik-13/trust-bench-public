/*
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  20240213  開発一課  経路スコア計算の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_LINE_MAX 1024
#define MIHFT_CODE_MAX 64
#define MIHFT_NAME_MAX 128
#define MIHFT_ROUTE_MAX 4096
#define MIHFT_BOOK_MAX 8192
#define MIHFT_INST_MAX 2048
#define MIHFT_PATH_ORD "SCORDF.csv"
#define MIHFT_PATH_ROUTE "SCROUTEF.csv"
#define MIHFT_PATH_BOOK "SCBOOK.csv"
#define MIHFT_PATH_INST "SCINSTF.csv"

typedef struct {
    char order_id[MIHFT_CODE_MAX];
    char cif_no[MIHFT_CODE_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    char ord_type;
    char tif_code[MIHFT_CODE_MAX];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} MihftOrder;

typedef struct {
    char route_key[MIHFT_CODE_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char board_code[MIHFT_CODE_MAX];
    char venue_kbn[MIHFT_CODE_MAX];
    int priority_no;
    int64_t max_slice_qty;
    char enabled_flg;
} MihftRoute;

typedef struct {
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int order_cnt;
    char entry_ts[MIHFT_CODE_MAX];
} MihftBook;

typedef struct {
    char instr_code[MIHFT_CODE_MAX];
    char instr_name[MIHFT_NAME_MAX];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[MIHFT_CODE_MAX];
} MihftInst;

typedef struct {
    const MihftRoute *route;
    int64_t exec_qty;
    int64_t slip_amt;
    int64_t score_amt;
} MihftBest;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);

    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_copy(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1U);
    return 0;
}

static int mihft_next(char **cur, char *dst, size_t dstsz)
{
    char *p = *cur;
    char *comma;
    size_t n;

    if (p == NULL) {
        return -1;
    }

    comma = strchr(p, ',');
    if (comma != NULL) {
        n = (size_t)(comma - p);
        *cur = comma + 1;
    } else {
        n = strlen(p);
        *cur = NULL;
    }

    if (n >= dstsz) {
        return -1;
    }
    memcpy(dst, p, n);
    dst[n] = '\0';
    return 0;
}

static int mihft_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    errno = 0;
    if (s[0] == '\0') {
        return -1;
    }
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }
    *out = (int64_t)v;
    return 0;
}

static int mihft_int(const char *s, int *out)
{
    int64_t v;

    if (mihft_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int mihft_mul_i64(int64_t a, int64_t b, int64_t *out)
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

static int mihft_board_valid(const char *board_code)
{
    return strcmp(board_code, "T1") == 0 ||
           strcmp(board_code, "ST") == 0 ||
           strcmp(board_code, "ETF") == 0;
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
    return 0;
}

static int mihft_read_order(MihftOrder *ord)
{
    FILE *fp = fopen(MIHFT_PATH_ORD, "r");
    char line[MIHFT_LINE_MAX];
    char tmp[MIHFT_CODE_MAX];
    char *cur;

    if (fp == NULL) {
        fprintf(stderr, "SCORDFを開けません\n");
        return -1;
    }
    if (fgets(line, sizeof(line), fp) == NULL || fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCORDFの注文行が不足しています\n");
        return -1;
    }
    fclose(fp);

    mihft_chomp(line);
    cur = line;
    if (mihft_next(&cur, ord->order_id, sizeof(ord->order_id)) != 0 ||
        mihft_next(&cur, ord->cif_no, sizeof(ord->cif_no)) != 0 ||
        mihft_next(&cur, ord->instr_code, sizeof(ord->instr_code)) != 0 ||
        mihft_next(&cur, tmp, sizeof(tmp)) != 0) {
        fprintf(stderr, "SCORDFの基本項目が不正です\n");
        return -1;
    }
    ord->side_kbn = tmp[0];

    if (mihft_next(&cur, tmp, sizeof(tmp)) != 0) {
        fprintf(stderr, "SCORDFの注文種別が不正です\n");
        return -1;
    }
    ord->ord_type = tmp[0];

    if (mihft_next(&cur, ord->tif_code, sizeof(ord->tif_code)) != 0 ||
        mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
        mihft_i64(tmp, &ord->ord_qty) != 0 ||
        mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
        mihft_i64(tmp, &ord->price_amt) != 0 ||
        mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
        mihft_int(tmp, &ord->instr_tier) != 0) {
        fprintf(stderr, "SCORDFの数量価格項目が不正です\n");
        return -1;
    }

    if ((ord->side_kbn != 'B' && ord->side_kbn != 'S') ||
        (ord->ord_type != 'L' && ord->ord_type != 'M') ||
        ord->ord_qty <= 0 || ord->price_amt < 0) {
        fprintf(stderr, "SCORDFのコード値が不正です\n");
        return -1;
    }

    return 0;
}

static int mihft_read_routes(MihftRoute *routes, size_t cap, size_t *cnt)
{
    FILE *fp = fopen(MIHFT_PATH_ROUTE, "r");
    char line[MIHFT_LINE_MAX];

    *cnt = 0U;
    if (fp == NULL) {
        fprintf(stderr, "SCROUTEFを開けません\n");
        return -1;
    }
    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCROUTEFの見出しが不足しています\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        MihftRoute *r;
        char *cur;
        char tmp[MIHFT_CODE_MAX];

        if (*cnt >= cap) {
            fclose(fp);
            fprintf(stderr, "SCROUTEFの件数が上限を超えました\n");
            return -1;
        }

        mihft_chomp(line);
        cur = line;
        r = &routes[*cnt];

        if (mihft_next(&cur, r->route_key, sizeof(r->route_key)) != 0 ||
            mihft_next(&cur, r->instr_code, sizeof(r->instr_code)) != 0 ||
            mihft_next(&cur, r->board_code, sizeof(r->board_code)) != 0 ||
            mihft_next(&cur, r->venue_kbn, sizeof(r->venue_kbn)) != 0 ||
            mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
            mihft_int(tmp, &r->priority_no) != 0 ||
            mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
            mihft_i64(tmp, &r->max_slice_qty) != 0 ||
            mihft_next(&cur, tmp, sizeof(tmp)) != 0) {
            fclose(fp);
            fprintf(stderr, "SCROUTEFの項目が不正です\n");
            return -1;
        }
        r->enabled_flg = tmp[0];
        (*cnt)++;
    }

    fclose(fp);
    return 0;
}

static int mihft_read_books(MihftBook *books, size_t cap, size_t *cnt)
{
    FILE *fp = fopen(MIHFT_PATH_BOOK, "r");
    char line[MIHFT_LINE_MAX];

    *cnt = 0U;
    if (fp == NULL) {
        fprintf(stderr, "SCBOOKを開けません\n");
        return -1;
    }
    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCBOOKの見出しが不足しています\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        MihftBook *b;
        char *cur;
        char tmp[MIHFT_CODE_MAX];

        if (*cnt >= cap) {
            fclose(fp);
            fprintf(stderr, "SCBOOKの件数が上限を超えました\n");
            return -1;
        }

        mihft_chomp(line);
        cur = line;
        b = &books[*cnt];

        if (mihft_next(&cur, b->instr_code, sizeof(b->instr_code)) != 0 ||
            mihft_next(&cur, tmp, sizeof(tmp)) != 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOKの基本項目が不正です\n");
            return -1;
        }
        b->side_kbn = tmp[0];

        if (mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
            mihft_int(tmp, &b->level_cnt) != 0 ||
            mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
            mihft_i64(tmp, &b->price_amt) != 0 ||
            mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
            mihft_i64(tmp, &b->book_qty) != 0 ||
            mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
            mihft_int(tmp, &b->order_cnt) != 0 ||
            mihft_next(&cur, b->entry_ts, sizeof(b->entry_ts)) != 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOKの数量価格項目が不正です\n");
            return -1;
        }

        if ((b->side_kbn != 'B' && b->side_kbn != 'S') ||
            b->level_cnt <= 0 || b->price_amt <= 0 || b->book_qty < 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOKの値が不正です\n");
            return -1;
        }
        (*cnt)++;
    }

    fclose(fp);
    return 0;
}

static int mihft_read_insts(MihftInst *insts, size_t cap, size_t *cnt)
{
    FILE *fp = fopen(MIHFT_PATH_INST, "r");
    char line[MIHFT_LINE_MAX];

    *cnt = 0U;
    if (fp == NULL) {
        fprintf(stderr, "SCINSTFを開けません\n");
        return -1;
    }
    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCINSTFの見出しが不足しています\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        MihftInst *i;
        char *cur;
        char tmp[MIHFT_CODE_MAX];

        if (*cnt >= cap) {
            fclose(fp);
            fprintf(stderr, "SCINSTFの件数が上限を超えました\n");
            return -1;
        }

        mihft_chomp(line);
        cur = line;
        i = &insts[*cnt];

        if (mihft_next(&cur, i->instr_code, sizeof(i->instr_code)) != 0 ||
            mihft_next(&cur, i->instr_name, sizeof(i->instr_name)) != 0 ||
            mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
            mihft_int(tmp, &i->instr_tier) != 0 ||
            mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
            mihft_i64(tmp, &i->tick_amt) != 0 ||
            mihft_next(&cur, tmp, sizeof(tmp)) != 0 ||
            mihft_i64(tmp, &i->lot_qty) != 0 ||
            mihft_next(&cur, i->board_code, sizeof(i->board_code)) != 0) {
            fclose(fp);
            fprintf(stderr, "SCINSTFの項目が不正です\n");
            return -1;
        }
        (*cnt)++;
    }

    fclose(fp);
    return 0;
}

static const MihftInst *mihft_find_inst(const MihftInst *insts, size_t cnt, const char *instr_code)
{
    size_t i;

    for (i = 0U; i < cnt; i++) {
        if (strcmp(insts[i].instr_code, instr_code) == 0) {
            return &insts[i];
        }
    }
    return NULL;
}

static int64_t mihft_min_i64(int64_t a, int64_t b)
{
    return a < b ? a : b;
}

static int mihft_price_ok(const MihftOrder *ord, const MihftBook *book)
{
    if (ord->ord_type == 'M') {
        return 1;
    }
    if (ord->side_kbn == 'B') {
        return book->price_amt <= ord->price_amt;
    }
    return book->price_amt >= ord->price_amt;
}

static int64_t mihft_slip_unit(const MihftOrder *ord, const MihftBook *book)
{
    int64_t d;

    if (ord->ord_type == 'M') {
        return 0;
    }
    if (ord->side_kbn == 'B') {
        d = book->price_amt - ord->price_amt;
    } else {
        d = ord->price_amt - book->price_amt;
    }
    return d > 0 ? d : 0;
}

static int mihft_select_best(const MihftOrder *ord,
                             const MihftRoute *routes,
                             size_t route_cnt,
                             const MihftBook *books,
                             size_t book_cnt,
                             const MihftInst *inst,
                             MihftBest *best)
{
    size_t r;
    int found = 0;
    char contra_side = ord->side_kbn == 'B' ? 'S' : 'B';

    best->route = NULL;
    best->exec_qty = 0;
    best->slip_amt = 0;
    best->score_amt = INT64_MAX;

    for (r = 0U; r < route_cnt; r++) {
        size_t b;
        const MihftRoute *route = &routes[r];
        int64_t route_qty = 0;
        int64_t route_slip = 0;
        int64_t route_score;

        if (route->enabled_flg != '1' ||
            strcmp(route->instr_code, ord->instr_code) != 0 ||
            strcmp(route->board_code, inst->board_code) != 0 ||
            !mihft_board_valid(route->board_code) ||
            route->max_slice_qty <= 0 ||
            ord->ord_qty > route->max_slice_qty) {
            continue;
        }

        for (b = 0U; b < book_cnt && route_qty < ord->ord_qty; b++) {
            const MihftBook *book = &books[b];
            int64_t take_qty;
            int64_t unit_slip;
            int64_t slip_add;

            if (strcmp(book->instr_code, ord->instr_code) != 0 ||
                book->side_kbn != contra_side ||
                !mihft_price_ok(ord, book) ||
                book->book_qty <= 0) {
                continue;
            }

            take_qty = mihft_min_i64(book->book_qty, ord->ord_qty - route_qty);
            unit_slip = mihft_slip_unit(ord, book);
            if (mihft_mul_i64(unit_slip, take_qty, &slip_add) != 0 ||
                route_slip > INT64_MAX - slip_add) {
                return -1;
            }
            route_slip += slip_add;
            route_qty += take_qty;
        }

        if (route_qty <= 0) {
            continue;
        }

        route_score = route_slip;
        if (route->priority_no > 0) {
            if (route_score > INT64_MAX - (int64_t)route->priority_no) {
                return -1;
            }
            route_score += (int64_t)route->priority_no;
        }

        if (!found ||
            route_score < best->score_amt ||
            (route_score == best->score_amt && route_qty > best->exec_qty)) {
            best->route = route;
            best->exec_qty = route_qty;
            best->slip_amt = route_slip;
            best->score_amt = route_score;
            found = 1;
        }
    }

    return found;
}

int main(void)
{
    MihftOrder ord;
    MihftRoute routes[MIHFT_ROUTE_MAX];
    MihftBook books[MIHFT_BOOK_MAX];
    MihftInst insts[MIHFT_INST_MAX];
    MihftBest best;
    const MihftInst *inst;
    size_t route_cnt;
    size_t book_cnt;
    size_t inst_cnt;
    int64_t notional;
    int64_t tick_amt;
    int selected;

    if (mihft_read_order(&ord) != 0 ||
        mihft_read_routes(routes, MIHFT_ROUTE_MAX, &route_cnt) != 0 ||
        mihft_read_books(books, MIHFT_BOOK_MAX, &book_cnt) != 0 ||
        mihft_read_insts(insts, MIHFT_INST_MAX, &inst_cnt) != 0) {
        return 99;
    }

    inst = mihft_find_inst(insts, inst_cnt, ord.instr_code);
    if (inst == NULL) {
        fprintf(stderr, "SCINSTFに銘柄が存在しません\n");
        return 99;
    }

    tick_amt = inst->tick_amt > 0 ? inst->tick_amt : mihft_tier_tick(ord.instr_tier);
    if (tick_amt <= 0 || ord.instr_tier != inst->instr_tier) {
        fprintf(stderr, "銘柄属性が不正です\n");
        return 99;
    }

    if (ord.ord_type == 'L' && (ord.price_amt <= 0 || ord.price_amt % tick_amt != 0)) {
        printf("ORDER-ID=%s,DECISION=12\n", ord.order_id);
        return 12;
    }

    if (mihft_mul_i64(ord.ord_qty, ord.price_amt, &notional) != 0) {
        fprintf(stderr, "想定元本の計算で桁あふれしました\n");
        return 99;
    }

    if (notional > MIHFT_MAX_NOTIONAL) {
        printf("ORDER-ID=%s,DECISION=8\n", ord.order_id);
        return 8;
    }

    selected = mihft_select_best(&ord, routes, route_cnt, books, book_cnt, inst, &best);
    if (selected < 0) {
        fprintf(stderr, "経路スコアの計算で桁あふれしました\n");
        return 99;
    }
    if (selected == 0) {
        printf("ORDER-ID=%s,DECISION=8\n", ord.order_id);
        return 8;
    }

    printf("ORDER-ID=%s,DECISION=0,ROUTE-KEY=%s,EXEC-QTY=%lld,SLIP-AMT=%lld,SCORE-AMT=%lld\n",
           ord.order_id,
           best.route->route_key,
           (long long)best.exec_qty,
           (long long)best.slip_amt,
           (long long)best.score_amt);

    return 0;
}
