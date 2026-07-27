/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220906  福田 亮太 (E-211)  初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_IN_SCORDF  "SCORDF.csv"
#define MIHFT_IN_SCBOOK  "SCBOOK.csv"
#define MIHFT_OUT_SCEXEC "SCEXEC.csv"
#define MIHFT_OUT_SCBOOK "SCBOOK.out.csv"

#define MIHFT_MAX_ORDERS  4096
#define MIHFT_MAX_LEVELS  8192
#define MIHFT_MAX_EXECS   16384
#define MIHFT_LINE_MAX    512

typedef struct {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    long long ord_qty;
    long long price_amt;
    int instr_tier;
} MihftOrderRec;

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    long long price_amt;
    long long book_qty;
    int order_cnt;
    long long entry_ts;
} MihftBookRec;

typedef struct {
    char exec_id[20];
    char order_id[32];
    char instr_code[32];
    char side_kbn;
    long long fill_qty;
    long long fill_amt;
    long long exec_ts;
} MihftExecRec;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_split_csv(char *line, char **cols, int max_cols)
{
    int n = 0;
    char *p = line;

    while (*p != '\0' && n < max_cols) {
        if (*p == '"') {
            char *w;
            p++;
            cols[n++] = p;
            w = p;
            while (*p != '\0') {
                if (*p == '"' && p[1] == '"') {
                    *w++ = '"';
                    p += 2;
                } else if (*p == '"') {
                    p++;
                    break;
                } else {
                    *w++ = *p++;
                }
            }
            *w = '\0';
            if (*p == ',') {
                p++;
            }
        } else {
            cols[n++] = p;
            while (*p != '\0' && *p != ',') {
                p++;
            }
            if (*p == ',') {
                *p++ = '\0';
            }
        }
    }

    return n;
}

static int mihft_parse_ll(const char *s, long long *out)
{
    char *end = NULL;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }
    *out = v;
    return 0;
}

static int mihft_parse_int(const char *s, int *out)
{
    long long v;

    if (mihft_parse_ll(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int mihft_copy(char *dst, size_t cap, const char *src)
{
    size_t n = strlen(src);

    if (n >= cap) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int mihft_read_orders(MihftOrderRec *orders, size_t *count)
{
    FILE *fp = fopen(MIHFT_IN_SCORDF, "r");
    char line[MIHFT_LINE_MAX];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "注文ファイルを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *c[9];

        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (mihft_split_csv(line, c, 9) != 9) {
            fprintf(stderr, "注文CSV項目数が不正です\n");
            fclose(fp);
            return -1;
        }
        if (strcmp(c[0], "ORDER-ID") == 0) {
            continue;
        }
        if (n >= MIHFT_MAX_ORDERS) {
            fprintf(stderr, "注文件数が上限を超過しました\n");
            fclose(fp);
            return -1;
        }
        if (mihft_copy(orders[n].order_id, sizeof(orders[n].order_id), c[0]) != 0 ||
            mihft_copy(orders[n].cif_no, sizeof(orders[n].cif_no), c[1]) != 0 ||
            mihft_copy(orders[n].instr_code, sizeof(orders[n].instr_code), c[2]) != 0 ||
            mihft_copy(orders[n].tif_code, sizeof(orders[n].tif_code), c[5]) != 0 ||
            c[3][1] != '\0' || c[4][1] != '\0' ||
            mihft_parse_ll(c[6], &orders[n].ord_qty) != 0 ||
            mihft_parse_ll(c[7], &orders[n].price_amt) != 0 ||
            mihft_parse_int(c[8], &orders[n].instr_tier) != 0) {
            fprintf(stderr, "注文CSV値が不正です\n");
            fclose(fp);
            return -1;
        }
        orders[n].side_kbn = c[3][0];
        orders[n].ord_type = c[4][0];
        n++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "注文ファイル読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int mihft_read_book(MihftBookRec *book, size_t *count)
{
    FILE *fp = fopen(MIHFT_IN_SCBOOK, "r");
    char line[MIHFT_LINE_MAX];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "板ファイルを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *c[7];

        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (mihft_split_csv(line, c, 7) != 7) {
            fprintf(stderr, "板CSV項目数が不正です\n");
            fclose(fp);
            return -1;
        }
        if (strcmp(c[0], "INSTR-CODE") == 0) {
            continue;
        }
        if (n >= MIHFT_MAX_LEVELS) {
            fprintf(stderr, "板件数が上限を超過しました\n");
            fclose(fp);
            return -1;
        }
        if (mihft_copy(book[n].instr_code, sizeof(book[n].instr_code), c[0]) != 0 ||
            c[1][1] != '\0' ||
            mihft_parse_int(c[2], &book[n].level_cnt) != 0 ||
            mihft_parse_ll(c[3], &book[n].price_amt) != 0 ||
            mihft_parse_ll(c[4], &book[n].book_qty) != 0 ||
            mihft_parse_int(c[5], &book[n].order_cnt) != 0 ||
            mihft_parse_ll(c[6], &book[n].entry_ts) != 0) {
            fprintf(stderr, "板CSV値が不正です\n");
            fclose(fp);
            return -1;
        }
        book[n].side_kbn = c[1][0];
        n++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "板ファイル読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int mihft_tier_tick(int tier, long long *tick)
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

static int mihft_validate_order(const MihftOrderRec *o)
{
    long long tick;
    long long notional;

    if ((o->side_kbn != 'B' && o->side_kbn != 'S') ||
        (o->ord_type != 'L' && o->ord_type != 'M') ||
        (strcmp(o->tif_code, "DAY") != 0 && strcmp(o->tif_code, "IOC") != 0 && strcmp(o->tif_code, "FOK") != 0) ||
        o->ord_qty <= 0 || o->price_amt < 0 ||
        mihft_tier_tick(o->instr_tier, &tick) != 0) {
        return 8;
    }

    if (o->ord_type == 'L' && (o->price_amt == 0 || (o->price_amt % tick) != 0)) {
        return 12;
    }

    if (o->ord_type == 'L') {
        if (o->ord_qty > LLONG_MAX / o->price_amt) {
            return 8;
        }
        notional = o->ord_qty * o->price_amt;
        if (notional > MIHFT_MAX_NOTIONAL) {
            return 8;
        }
    }

    return 0;
}

static int mihft_is_opposite(char a, char b)
{
    return (a == 'B' && b == 'S') || (a == 'S' && b == 'B');
}

static int mihft_marketable(const MihftOrderRec *o, const MihftBookRec *b)
{
    if (o->ord_type == 'M') {
        return 1;
    }
    if (o->side_kbn == 'B') {
        return o->price_amt >= b->price_amt;
    }
    return o->price_amt <= b->price_amt;
}

static int mihft_better_level(const MihftOrderRec *o, const MihftBookRec *a, const MihftBookRec *b)
{
    if (o->side_kbn == 'B') {
        if (a->price_amt != b->price_amt) {
            return a->price_amt < b->price_amt;
        }
    } else {
        if (a->price_amt != b->price_amt) {
            return a->price_amt > b->price_amt;
        }
    }
    if (a->entry_ts != b->entry_ts) {
        return a->entry_ts < b->entry_ts;
    }
    return a->level_cnt < b->level_cnt;
}

static long long mihft_available_qty(const MihftOrderRec *o, const MihftBookRec *book, size_t book_count)
{
    size_t i;
    long long total = 0;

    for (i = 0; i < book_count; i++) {
        if (strcmp(o->instr_code, book[i].instr_code) == 0 &&
            mihft_is_opposite(o->side_kbn, book[i].side_kbn) &&
            book[i].book_qty > 0 &&
            mihft_marketable(o, &book[i])) {
            if (total > LLONG_MAX - book[i].book_qty) {
                return LLONG_MAX;
            }
            total += book[i].book_qty;
        }
    }

    return total;
}

static int mihft_next_level(const MihftOrderRec *o, const MihftBookRec *book, size_t book_count)
{
    size_t i;
    int best = -1;

    for (i = 0; i < book_count; i++) {
        if (strcmp(o->instr_code, book[i].instr_code) != 0 ||
            !mihft_is_opposite(o->side_kbn, book[i].side_kbn) ||
            book[i].book_qty <= 0 ||
            !mihft_marketable(o, &book[i])) {
            continue;
        }
        if (best < 0 || mihft_better_level(o, &book[i], &book[best])) {
            best = (int)i;
        }
    }

    return best;
}

static int mihft_emit_exec(MihftExecRec *execs, size_t *exec_count, const MihftOrderRec *o, long long qty, long long price)
{
    MihftExecRec *e;
    unsigned long long seq;

    if (*exec_count >= MIHFT_MAX_EXECS || qty <= 0 || price < 0 || qty > LLONG_MAX / (price == 0 ? 1 : price)) {
        return -1;
    }

    e = &execs[*exec_count];
    seq = (unsigned long long)(*exec_count + 1U);
    snprintf(e->exec_id, sizeof(e->exec_id), "EX%012llu", seq);
    if (mihft_copy(e->order_id, sizeof(e->order_id), o->order_id) != 0 ||
        mihft_copy(e->instr_code, sizeof(e->instr_code), o->instr_code) != 0) {
        return -1;
    }
    e->side_kbn = o->side_kbn;
    e->fill_qty = qty;
    e->fill_amt = qty * price;
    e->exec_ts = 202501150900000000LL + (long long)seq;

    (*exec_count)++;
    return 0;
}

static int mihft_match_order(const MihftOrderRec *o, MihftBookRec *book, size_t book_count, MihftExecRec *execs, size_t *exec_count)
{
    long long remain = o->ord_qty;
    int decision = mihft_validate_order(o);

    if (decision != 0) {
        return decision;
    }

    if (strcmp(o->tif_code, "FOK") == 0 && mihft_available_qty(o, book, book_count) < o->ord_qty) {
        return 0;
    }

    while (remain > 0) {
        int idx = mihft_next_level(o, book, book_count);
        long long fill_qty;

        if (idx < 0) {
            break;
        }

        fill_qty = remain < book[idx].book_qty ? remain : book[idx].book_qty;
        if (mihft_emit_exec(execs, exec_count, o, fill_qty, book[idx].price_amt) != 0) {
            return -1;
        }

        remain -= fill_qty;
        book[idx].book_qty -= fill_qty;
        if (book[idx].book_qty == 0) {
            book[idx].order_cnt = 0;
        } else if (book[idx].order_cnt > 1) {
            book[idx].order_cnt--;
        }
    }

    return 0;
}

static void mihft_compact_book(MihftBookRec *book, size_t *book_count)
{
    size_t r;
    size_t w = 0;

    for (r = 0; r < *book_count; r++) {
        if (book[r].book_qty > 0 && book[r].order_cnt > 0) {
            if (w != r) {
                book[w] = book[r];
            }
            w++;
        }
    }
    *book_count = w;
}

static int mihft_write_execs(const MihftExecRec *execs, size_t count)
{
    FILE *fp = fopen(MIHFT_OUT_SCEXEC, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "約定ファイルを開けません\n");
        return -1;
    }

    for (i = 0; i < count; i++) {
        if (fprintf(fp, "%s,%s,%s,%c,%lld,%lld,%lld\n",
                    execs[i].exec_id,
                    execs[i].order_id,
                    execs[i].instr_code,
                    execs[i].side_kbn,
                    execs[i].fill_qty,
                    execs[i].fill_amt,
                    execs[i].exec_ts) < 0) {
            fprintf(stderr, "約定ファイル書込に失敗しました\n");
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "約定ファイル終了処理に失敗しました\n");
        return -1;
    }
    return 0;
}

static int mihft_write_book(const MihftBookRec *book, size_t count)
{
    FILE *fp = fopen(MIHFT_OUT_SCBOOK, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "板出力ファイルを開けません\n");
        return -1;
    }

    for (i = 0; i < count; i++) {
        if (fprintf(fp, "%s,%c,%d,%lld,%lld,%d,%lld\n",
                    book[i].instr_code,
                    book[i].side_kbn,
                    book[i].level_cnt,
                    book[i].price_amt,
                    book[i].book_qty,
                    book[i].order_cnt,
                    book[i].entry_ts) < 0) {
            fprintf(stderr, "板出力ファイル書込に失敗しました\n");
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "板出力ファイル終了処理に失敗しました\n");
        return -1;
    }
    return 0;
}

int main(void)
{
    MihftOrderRec orders[MIHFT_MAX_ORDERS];
    MihftBookRec book[MIHFT_MAX_LEVELS];
    MihftExecRec execs[MIHFT_MAX_EXECS];
    size_t order_count = 0;
    size_t book_count = 0;
    size_t exec_count = 0;
    size_t i;
    int final_decision = 0;

    if (mihft_read_orders(orders, &order_count) != 0 ||
        mihft_read_book(book, &book_count) != 0) {
        return 99;
    }

    for (i = 0; i < order_count; i++) {
        int decision = mihft_match_order(&orders[i], book, book_count, execs, &exec_count);

        if (decision < 0) {
            fprintf(stderr, "約定照合処理に失敗しました\n");
            return 99;
        }
        if (final_decision == 0 && decision != 0) {
            final_decision = decision;
        }
        mihft_compact_book(book, &book_count);
    }

    if (mihft_write_execs(execs, exec_count) != 0 ||
        mihft_write_book(book, book_count) != 0) {
        return 99;
    }

    return final_decision;
}
