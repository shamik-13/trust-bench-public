/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240213  大野 修 (E-225)  初版作成
 * 1.01  20240713  篠原 健 (E-203)  気配時刻の陳腐判定を追加
 * 1.02  20241213  西村 亮 (E-204)  呼値正規化と約定可能数量判定を追加
 */
#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_MAX_INST 1024
#define MIHFT_LINE_MAX 512
#define MIHFT_STALE_NS 5000000000LL

struct inst_row {
    char code[32];
    char name[96];
    int tier;
    long long tick;
    long long lot;
    char board[8];
};

struct book_side {
    int seen;
    int level_cnt;
    long long price;
    long long qty;
    int order_cnt;
    long long ts;
};

struct mktd_row {
    char code[32];
    long long bid;
    long long ask;
    long long last;
    long long vol;
    long long ts;
    int seen;
};

struct joined_row {
    struct inst_row inst;
    struct book_side bid_book;
    struct book_side ask_book;
    struct mktd_row mktd;
};

static char *next_field(char **cursor)
{
    char *head = *cursor;
    char *p;

    if (head == NULL) {
        return NULL;
    }

    p = head;
    while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
        p++;
    }

    if (*p == ',') {
        *p = '\0';
        *cursor = p + 1;
    } else {
        *p = '\0';
        *cursor = NULL;
    }

    return head;
}

static int copy_field(char *dst, size_t dst_size, const char *src)
{
    size_t len;

    if (dst_size == 0 || src == NULL) {
        return -1;
    }

    len = strlen(src);
    if (len >= dst_size) {
        return -1;
    }

    memcpy(dst, src, len + 1);
    return 0;
}

static int parse_ll(const char *text, long long *out)
{
    char *end;
    long long value;

    if (text == NULL || *text == '\0') {
        return -1;
    }

    errno = 0;
    value = strtoll(text, &end, 10);
    if (errno != 0 || end == text || *end != '\0') {
        return -1;
    }

    *out = value;
    return 0;
}

static int parse_int(const char *text, int *out)
{
    long long value;

    if (parse_ll(text, &value) != 0 || value < INT_MIN || value > INT_MAX) {
        return -1;
    }

    *out = (int)value;
    return 0;
}

static FILE *open_csv(const char *base)
{
    char path[64];
    FILE *fp;

    fp = fopen(base, "r");
    if (fp != NULL) {
        return fp;
    }

    if (snprintf(path, sizeof(path), "%s.csv", base) < 0) {
        return NULL;
    }

    fp = fopen(path, "r");
    if (fp != NULL) {
        return fp;
    }

    if (snprintf(path, sizeof(path), "%s.CSV", base) < 0) {
        return NULL;
    }

    return fopen(path, "r");
}

static int same_code(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

static int find_row(struct joined_row *rows, int count, const char *code)
{
    int i;

    for (i = 0; i < count; i++) {
        if (same_code(rows[i].inst.code, code)) {
            return i;
        }
    }

    return -1;
}

static int tier_tick(int tier, long long *tick)
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

static long long floor_tick(long long price, long long tick)
{
    if (tick <= 0 || price <= 0) {
        return 0;
    }
    return (price / tick) * tick;
}

static long long ceil_tick(long long price, long long tick)
{
    if (tick <= 0 || price <= 0) {
        return 0;
    }
    return ((price + tick - 1) / tick) * tick;
}

static int checked_notional(long long price, long long qty, long long *out)
{
    if (price < 0 || qty < 0) {
        return -1;
    }

    if (price != 0 && qty > LLONG_MAX / price) {
        return -1;
    }

    *out = price * qty;
    return 0;
}

static int load_inst(struct joined_row *rows, int *count)
{
    FILE *fp = open_csv("SCINSTF");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "SCINSTFを開けません\n");
        return -1;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCINSTFが空です\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cur = line;
        char *code = next_field(&cur);
        char *name = next_field(&cur);
        char *tier = next_field(&cur);
        char *tick = next_field(&cur);
        char *lot = next_field(&cur);
        char *board = next_field(&cur);
        long long parsed_tick;
        int idx = *count;

        if (idx >= MIHFT_MAX_INST) {
            fclose(fp);
            fprintf(stderr, "SCINSTFの件数が上限超過です\n");
            return -1;
        }

        memset(&rows[idx], 0, sizeof(rows[idx]));
        if (copy_field(rows[idx].inst.code, sizeof(rows[idx].inst.code), code) != 0 ||
            copy_field(rows[idx].inst.name, sizeof(rows[idx].inst.name), name) != 0 ||
            copy_field(rows[idx].inst.board, sizeof(rows[idx].inst.board), board) != 0 ||
            parse_int(tier, &rows[idx].inst.tier) != 0 ||
            parse_ll(tick, &rows[idx].inst.tick) != 0 ||
            parse_ll(lot, &rows[idx].inst.lot) != 0 ||
            tier_tick(rows[idx].inst.tier, &parsed_tick) != 0 ||
            rows[idx].inst.tick != parsed_tick ||
            rows[idx].inst.lot <= 0) {
            fclose(fp);
            fprintf(stderr, "SCINSTFの形式が不正です\n");
            return -1;
        }

        (*count)++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCINSTFの読込に失敗しました\n");
        return -1;
    }

    fclose(fp);
    return 0;
}

static int load_book(struct joined_row *rows, int count)
{
    FILE *fp = open_csv("SCBOOK");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "SCBOOKを開けません\n");
        return -1;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCBOOKが空です\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cur = line;
        char *code = next_field(&cur);
        char *side = next_field(&cur);
        char *level = next_field(&cur);
        char *price = next_field(&cur);
        char *qty = next_field(&cur);
        char *orders = next_field(&cur);
        char *ts = next_field(&cur);
        struct book_side cand;
        int idx;

        memset(&cand, 0, sizeof(cand));
        idx = find_row(rows, count, code);
        if (idx < 0 ||
            side == NULL ||
            parse_int(level, &cand.level_cnt) != 0 ||
            parse_ll(price, &cand.price) != 0 ||
            parse_ll(qty, &cand.qty) != 0 ||
            parse_int(orders, &cand.order_cnt) != 0 ||
            parse_ll(ts, &cand.ts) != 0 ||
            cand.level_cnt <= 0 ||
            cand.price <= 0 ||
            cand.qty < 0 ||
            cand.order_cnt < 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOKの形式が不正です\n");
            return -1;
        }

        cand.seen = 1;
        if (strcmp(side, "B") == 0) {
            if (!rows[idx].bid_book.seen || cand.price > rows[idx].bid_book.price) {
                rows[idx].bid_book = cand;
            }
        } else if (strcmp(side, "S") == 0) {
            if (!rows[idx].ask_book.seen || cand.price < rows[idx].ask_book.price) {
                rows[idx].ask_book = cand;
            }
        } else {
            fclose(fp);
            fprintf(stderr, "SCBOOKの売買区分が不正です\n");
            return -1;
        }
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCBOOKの読込に失敗しました\n");
        return -1;
    }

    fclose(fp);
    return 0;
}

static int load_mktd(struct joined_row *rows, int count)
{
    FILE *fp = open_csv("SCMKTD");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "SCMKTDを開けません\n");
        return -1;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCMKTDが空です\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cur = line;
        char *code = next_field(&cur);
        char *bid = next_field(&cur);
        char *ask = next_field(&cur);
        char *last = next_field(&cur);
        char *vol = next_field(&cur);
        char *ts = next_field(&cur);
        int idx = find_row(rows, count, code);

        if (idx < 0 ||
            copy_field(rows[idx].mktd.code, sizeof(rows[idx].mktd.code), code) != 0 ||
            parse_ll(bid, &rows[idx].mktd.bid) != 0 ||
            parse_ll(ask, &rows[idx].mktd.ask) != 0 ||
            parse_ll(last, &rows[idx].mktd.last) != 0 ||
            parse_ll(vol, &rows[idx].mktd.vol) != 0 ||
            parse_ll(ts, &rows[idx].mktd.ts) != 0 ||
            rows[idx].mktd.bid <= 0 ||
            rows[idx].mktd.ask <= 0 ||
            rows[idx].mktd.last <= 0 ||
            rows[idx].mktd.vol < 0) {
            fclose(fp);
            fprintf(stderr, "SCMKTDの形式が不正です\n");
            return -1;
        }

        rows[idx].mktd.seen = 1;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCMKTDの読込に失敗しました\n");
        return -1;
    }

    fclose(fp);
    return 0;
}

static int emit_snapshot(const struct joined_row *row)
{
    long long bid_px = 0;
    long long ask_px = 0;
    long long bid_qty = 0;
    long long ask_qty = 0;
    long long notional = 0;
    long long latest_ts;
    int stale = 0;
    int code = 0;

    if (!row->mktd.seen || !row->bid_book.seen || !row->ask_book.seen) {
        printf("%s,0,0,0,0,1,12\n", row->inst.code);
        return 12;
    }

    if (row->bid_book.price % row->inst.tick != 0 ||
        row->ask_book.price % row->inst.tick != 0 ||
        row->mktd.bid % row->inst.tick != 0 ||
        row->mktd.ask % row->inst.tick != 0 ||
        row->mktd.last % row->inst.tick != 0) {
        bid_px = floor_tick(row->bid_book.price, row->inst.tick);
        ask_px = ceil_tick(row->ask_book.price, row->inst.tick);
        code = 12;
    } else {
        bid_px = row->bid_book.price;
        ask_px = row->ask_book.price;
    }

    bid_qty = (row->bid_book.qty / row->inst.lot) * row->inst.lot;
    ask_qty = (row->ask_book.qty / row->inst.lot) * row->inst.lot;

    latest_ts = row->bid_book.ts;
    if (row->ask_book.ts > latest_ts) {
        latest_ts = row->ask_book.ts;
    }
    if (row->mktd.ts > latest_ts) {
        latest_ts = row->mktd.ts;
    }

    if (latest_ts - row->bid_book.ts > MIHFT_STALE_NS ||
        latest_ts - row->ask_book.ts > MIHFT_STALE_NS ||
        latest_ts - row->mktd.ts > MIHFT_STALE_NS) {
        stale = 1;
    }

    if (bid_px <= 0 || ask_px <= 0 || bid_px >= ask_px || bid_qty <= 0 || ask_qty <= 0) {
        code = 12;
    }

    if (checked_notional(ask_px, ask_qty, &notional) != 0 || notional > MIHFT_MAX_NOTIONAL) {
        code = 8;
    }

    printf("%s,%lld,%lld,%lld,%lld,%d,%d\n",
           row->inst.code, bid_px, bid_qty, ask_px, ask_qty, stale, code);

    return code;
}

int main(void)
{
    static struct joined_row rows[MIHFT_MAX_INST];
    int count = 0;
    int i;
    int final_code = 0;

    if (load_inst(rows, &count) != 0) {
        return 2;
    }
    if (load_book(rows, count) != 0) {
        return 2;
    }
    if (load_mktd(rows, count) != 0) {
        return 2;
    }

    printf("INSTR-CODE,BID-EXEC-AMT,BID-AVAIL-QTY,ASK-EXEC-AMT,ASK-AVAIL-QTY,STALE-FLG,DECISION-CODE\n");

    for (i = 0; i < count; i++) {
        int code = emit_snapshot(&rows[i]);

        if (code == 8) {
            final_code = 8;
        } else if (code == 12 && final_code == 0) {
            final_code = 12;
        }
    }

    return final_code;
}
