/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20220906  市場基盤部  SCBOOK板価格ラダー構築の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_LINE_MAX 512
#define MIHFT_INSTR_MAX 32
#define MIHFT_SIDE_BUY 'B'
#define MIHFT_SIDE_SELL 'S'
#define MIHFT_EXIT_IO 2
#define MIHFT_EXIT_PARSE 3
#define MIHFT_EXIT_MEMORY 5
#define MIHFT_DECISION_ACCEPT 0
#define MIHFT_DECISION_REJECT_NOTIONAL 8
#define MIHFT_DECISION_REJECT_TICK 12

typedef struct {
    char instr_code[MIHFT_INSTR_MAX];
    char side_kbn;
    long level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    long order_cnt;
    int64_t entry_ts;
} MihftScbookRow;

typedef struct {
    char instr_code[MIHFT_INSTR_MAX];
    char side_kbn;
    int64_t price_amt;
    int64_t book_qty;
    long order_cnt;
    int64_t entry_ts;
} MihftLadderLevel;

typedef struct {
    MihftLadderLevel *v;
    size_t n;
    size_t cap;
} MihftLadder;

typedef struct {
    char instr_code[MIHFT_INSTR_MAX];
    int has_bid;
    int has_ask;
    int64_t best_bid;
    int64_t best_ask;
    uint64_t generation;
    size_t bid_levels;
    size_t ask_levels;
    int invalidated;
} MihftBookState;

typedef struct {
    MihftBookState *v;
    size_t n;
    size_t cap;
} MihftBookStates;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_parse_i64(const char *s, int64_t *out)
{
    char *end;
    long long v;

    if (s == NULL || *s == '\0') {
        return 0;
    }
    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || *end != '\0') {
        return 0;
    }
    *out = (int64_t)v;
    return 1;
}

static int mihft_parse_long(const char *s, long *out)
{
    char *end;
    long v;

    if (s == NULL || *s == '\0') {
        return 0;
    }
    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || *end != '\0') {
        return 0;
    }
    *out = v;
    return 1;
}

static int mihft_mul_over_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a < 0 || b < 0) {
        return 1;
    }
    if (a != 0 && b > INT64_MAX / a) {
        return 1;
    }
    *out = a * b;
    return 0;
}

static int mihft_tick_amt(const char *instr_code)
{
    unsigned char c = (unsigned char)instr_code[0];

    if (c >= '1' && c <= '3') {
        return 100;
    }
    if (c >= '4' && c <= '7') {
        return 500;
    }
    return 1000;
}

static int mihft_parse_row(char *line, MihftScbookRow *row)
{
    char *cols[7];
    char *p = line;
    size_t i;

    for (i = 0; i < 7; i++) {
        cols[i] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            if (i == 6) {
                break;
            }
            return 0;
        }
        *p++ = '\0';
    }
    if (strchr(cols[6], ',') != NULL) {
        return 0;
    }
    if (cols[0][0] == '\0' || strlen(cols[0]) >= sizeof(row->instr_code)) {
        return 0;
    }
    if (!((cols[1][0] == MIHFT_SIDE_BUY || cols[1][0] == MIHFT_SIDE_SELL) && cols[1][1] == '\0')) {
        return 0;
    }
    memcpy(row->instr_code, cols[0], strlen(cols[0]) + 1);
    row->side_kbn = cols[1][0];

    if (!mihft_parse_long(cols[2], &row->level_cnt) ||
        !mihft_parse_i64(cols[3], &row->price_amt) ||
        !mihft_parse_i64(cols[4], &row->book_qty) ||
        !mihft_parse_long(cols[5], &row->order_cnt) ||
        !mihft_parse_i64(cols[6], &row->entry_ts)) {
        return 0;
    }
    if (row->level_cnt < 0 || row->price_amt <= 0 || row->book_qty < 0 ||
        row->order_cnt < 0 || row->entry_ts < 0) {
        return 0;
    }
    return 1;
}

static int mihft_ladder_push(MihftLadder *ladder, const MihftScbookRow *row)
{
    MihftLadderLevel *nv;
    size_t nc;

    if (ladder->n == ladder->cap) {
        nc = ladder->cap == 0 ? 128u : ladder->cap * 2u;
        if (nc < ladder->cap) {
            return 0;
        }
        nv = (MihftLadderLevel *)realloc(ladder->v, nc * sizeof(*nv));
        if (nv == NULL) {
            return 0;
        }
        ladder->v = nv;
        ladder->cap = nc;
    }

    memcpy(ladder->v[ladder->n].instr_code, row->instr_code, sizeof(ladder->v[ladder->n].instr_code));
    ladder->v[ladder->n].side_kbn = row->side_kbn;
    ladder->v[ladder->n].price_amt = row->price_amt;
    ladder->v[ladder->n].book_qty = row->book_qty;
    ladder->v[ladder->n].order_cnt = row->order_cnt;
    ladder->v[ladder->n].entry_ts = row->entry_ts;
    ladder->n++;
    return 1;
}

static int mihft_level_cmp(const void *a, const void *b)
{
    const MihftLadderLevel *x = (const MihftLadderLevel *)a;
    const MihftLadderLevel *y = (const MihftLadderLevel *)b;
    int c = strcmp(x->instr_code, y->instr_code);

    if (c != 0) {
        return c;
    }
    if (x->side_kbn != y->side_kbn) {
        return x->side_kbn == MIHFT_SIDE_BUY ? -1 : 1;
    }
    if (x->side_kbn == MIHFT_SIDE_BUY) {
        return (x->price_amt < y->price_amt) - (x->price_amt > y->price_amt);
    }
    return (x->price_amt > y->price_amt) - (x->price_amt < y->price_amt);
}

static MihftBookState *mihft_state_get(MihftBookStates *states, const char *instr_code)
{
    MihftBookState *nv;
    size_t nc;
    size_t i;

    for (i = 0; i < states->n; i++) {
        if (strcmp(states->v[i].instr_code, instr_code) == 0) {
            return &states->v[i];
        }
    }

    if (states->n == states->cap) {
        nc = states->cap == 0 ? 32u : states->cap * 2u;
        if (nc < states->cap) {
            return NULL;
        }
        nv = (MihftBookState *)realloc(states->v, nc * sizeof(*nv));
        if (nv == NULL) {
            return NULL;
        }
        states->v = nv;
        states->cap = nc;
    }

    memset(&states->v[states->n], 0, sizeof(states->v[states->n]));
    memcpy(states->v[states->n].instr_code, instr_code, strlen(instr_code) + 1);
    states->v[states->n].generation = 1u;
    return &states->v[states->n++];
}

static int mihft_update_state(MihftBookStates *states, const MihftLadderLevel *lv)
{
    MihftBookState *st = mihft_state_get(states, lv->instr_code);

    if (st == NULL) {
        return 0;
    }
    if (lv->book_qty == 0) {
        return 1;
    }

    if (lv->side_kbn == MIHFT_SIDE_BUY) {
        st->bid_levels++;
        if (!st->has_bid || lv->price_amt > st->best_bid) {
            st->best_bid = lv->price_amt;
            st->has_bid = 1;
        }
    } else {
        st->ask_levels++;
        if (!st->has_ask || lv->price_amt < st->best_ask) {
            st->best_ask = lv->price_amt;
            st->has_ask = 1;
        }
    }
    return 1;
}

static void mihft_invalidate_bad_quotes(MihftBookStates *states)
{
    size_t i;

    for (i = 0; i < states->n; i++) {
        MihftBookState *st = &states->v[i];

        if (!st->has_bid || !st->has_ask || st->best_bid >= st->best_ask) {
            st->has_bid = 0;
            st->has_ask = 0;
            st->best_bid = 0;
            st->best_ask = 0;
            st->generation++;
            st->invalidated = 1;
        }
    }
}

int main(void)
{
    char line[MIHFT_LINE_MAX];
    MihftLadder ladder;
    MihftBookStates states;
    long physical_line = 0;
    int decision = MIHFT_DECISION_ACCEPT;
    int saw_data = 0;
    int level_mismatch = 0;
    int order_mismatch = 0;

    memset(&ladder, 0, sizeof(ladder));
    memset(&states, 0, sizeof(states));

    while (fgets(line, sizeof(line), stdin) != NULL) {
        MihftScbookRow row;
        int64_t notional;
        size_t len;

        physical_line++;
        len = strlen(line);
        if (len > 0 && line[len - 1] != '\n' && !feof(stdin)) {
            fprintf(stderr, "入力行が長すぎます: 行=%ld\n", physical_line);
            free(ladder.v);
            free(states.v);
            return MIHFT_EXIT_PARSE;
        }

        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (physical_line == 1 && strncmp(line, "INSTR-CODE,", 11) == 0) {
            continue;
        }
        if (!mihft_parse_row(line, &row)) {
            fprintf(stderr, "SCBOOK解析エラー: 行=%ld\n", physical_line);
            free(ladder.v);
            free(states.v);
            return MIHFT_EXIT_PARSE;
        }

        saw_data = 1;
        if (row.level_cnt == 0 && (row.book_qty != 0 || row.order_cnt != 0)) {
            level_mismatch = 1;
        }
        if (row.book_qty == 0 && row.order_cnt != 0) {
            order_mismatch = 1;
        }
        if (row.order_cnt == 0 && row.book_qty != 0) {
            order_mismatch = 1;
        }
        if ((row.price_amt % mihft_tick_amt(row.instr_code)) != 0) {
            decision = MIHFT_DECISION_REJECT_TICK;
        }
        if (mihft_mul_over_i64(row.price_amt, row.book_qty, &notional) ||
            notional > (int64_t)MIHFT_MAX_NOTIONAL) {
            if (decision == MIHFT_DECISION_ACCEPT) {
                decision = MIHFT_DECISION_REJECT_NOTIONAL;
            }
        }
        if (!mihft_ladder_push(&ladder, &row)) {
            fprintf(stderr, "ラダー領域を確保できません\n");
            free(ladder.v);
            free(states.v);
            return MIHFT_EXIT_MEMORY;
        }
    }

    if (ferror(stdin)) {
        fprintf(stderr, "SCBOOK読込エラー\n");
        free(ladder.v);
        free(states.v);
        return MIHFT_EXIT_IO;
    }

    if (!saw_data) {
        fprintf(stderr, "SCBOOKが空です\n");
        free(ladder.v);
        free(states.v);
        return MIHFT_EXIT_PARSE;
    }

    qsort(ladder.v, ladder.n, sizeof(ladder.v[0]), mihft_level_cmp);

    {
        size_t i;

        for (i = 0; i < ladder.n; i++) {
            if (!mihft_update_state(&states, &ladder.v[i])) {
                fprintf(stderr, "銘柄状態領域を確保できません\n");
                free(ladder.v);
                free(states.v);
                return MIHFT_EXIT_MEMORY;
            }
        }
    }

    mihft_invalidate_bad_quotes(&states);

    {
        size_t i;

        for (i = 0; i < states.n; i++) {
            const MihftBookState *st = &states.v[i];

            printf("%s,%zu,%zu,%" PRIu64 ",%s",
                   st->instr_code,
                   st->bid_levels,
                   st->ask_levels,
                   st->generation,
                   st->invalidated ? "無効" : "有効");
            if (st->has_bid && st->has_ask) {
                printf(",%" PRId64 ",%" PRId64 "\n", st->best_bid, st->best_ask);
            } else {
                printf(",0,0\n");
            }
        }
    }

    if (level_mismatch) {
        fprintf(stderr, "LEVEL-CNT整合性警告\n");
    }
    if (order_mismatch) {
        fprintf(stderr, "ORDER-CNT整合性警告\n");
    }

    free(ladder.v);
    free(states.v);
    return decision;
}
