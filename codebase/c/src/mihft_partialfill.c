/************************************************************
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20250603  藤田 和也 (E-271)  初版作成
 ************************************************************/

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SCORDF_PATH "SCORDF.csv"
#define SCBOOK_PATH "SCBOOK.csv"
#define SCEXEC_PATH "SCEXEC.csv"

#define CSV_LINE_MAX 512
#define ORDER_MAX 4096
#define BOOK_MAX 16384
#define TOKEN_MAX 16
#define EXEC_ID_LEN 32

typedef struct {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} MihftLocalOrder;

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int order_cnt;
    int64_t entry_ts;
} MihftLocalBook;

static int mihft_streq(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_split_csv(char *line, char *tok[], size_t max_tok)
{
    size_t n = 0;
    char *p = line;

    while (n < max_tok) {
        tok[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return (int)n;
}

static int mihft_i64(const char *s, int64_t *out)
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

static int mihft_int(const char *s, int *out)
{
    int64_t v;

    if (mihft_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int mihft_copy(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int mihft_tick_size(int tier, int64_t *tick)
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

static int mihft_mul_over_i64(int64_t a, int64_t b, int64_t *out)
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

static int mihft_parse_order(char *line, MihftLocalOrder *o)
{
    char *tok[TOKEN_MAX];
    int ntok;

    mihft_chomp(line);
    ntok = mihft_split_csv(line, tok, TOKEN_MAX);
    if (ntok != 9) {
        return -1;
    }

    if (mihft_copy(o->order_id, sizeof(o->order_id), tok[0]) != 0 ||
        mihft_copy(o->cif_no, sizeof(o->cif_no), tok[1]) != 0 ||
        mihft_copy(o->instr_code, sizeof(o->instr_code), tok[2]) != 0) {
        return -1;
    }

    if ((tok[3][0] != 'B' && tok[3][0] != 'S') || tok[3][1] != '\0') {
        return -1;
    }
    o->side_kbn = tok[3][0];

    if ((tok[4][0] != 'L' && tok[4][0] != 'M') || tok[4][1] != '\0') {
        return -1;
    }
    o->ord_type = tok[4][0];

    if (!mihft_streq(tok[5], "DAY") && !mihft_streq(tok[5], "IOC") && !mihft_streq(tok[5], "FOK")) {
        return -1;
    }
    if (mihft_copy(o->tif_code, sizeof(o->tif_code), tok[5]) != 0) {
        return -1;
    }

    if (mihft_i64(tok[6], &o->ord_qty) != 0 ||
        mihft_i64(tok[7], &o->price_amt) != 0 ||
        mihft_int(tok[8], &o->instr_tier) != 0) {
        return -1;
    }
    if (o->ord_qty <= 0 || o->price_amt < 0) {
        return -1;
    }

    return 0;
}

static int mihft_parse_book(char *line, MihftLocalBook *b)
{
    char *tok[TOKEN_MAX];
    int ntok;

    mihft_chomp(line);
    ntok = mihft_split_csv(line, tok, TOKEN_MAX);
    if (ntok != 7) {
        return -1;
    }

    if (mihft_copy(b->instr_code, sizeof(b->instr_code), tok[0]) != 0) {
        return -1;
    }
    if ((tok[1][0] != 'B' && tok[1][0] != 'S') || tok[1][1] != '\0') {
        return -1;
    }
    b->side_kbn = tok[1][0];

    if (mihft_int(tok[2], &b->level_cnt) != 0 ||
        mihft_i64(tok[3], &b->price_amt) != 0 ||
        mihft_i64(tok[4], &b->book_qty) != 0 ||
        mihft_int(tok[5], &b->order_cnt) != 0 ||
        mihft_i64(tok[6], &b->entry_ts) != 0) {
        return -1;
    }
    if (b->level_cnt <= 0 || b->price_amt <= 0 || b->book_qty < 0 || b->order_cnt <= 0 || b->entry_ts < 0) {
        return -1;
    }

    return 0;
}

static int mihft_read_orders(MihftLocalOrder *orders, size_t *count)
{
    FILE *fp = fopen(SCORDF_PATH, "r");
    char line[CSV_LINE_MAX];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCORDFオープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        if (n >= ORDER_MAX) {
            fclose(fp);
            fprintf(stderr, "SCORDF件数超過\n");
            return -1;
        }
        if (mihft_parse_order(line, &orders[n]) != 0) {
            fclose(fp);
            fprintf(stderr, "SCORDF解析失敗\n");
            return -1;
        }
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCORDF読込失敗\n");
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int mihft_read_books(MihftLocalBook *books, size_t *count)
{
    FILE *fp = fopen(SCBOOK_PATH, "r");
    char line[CSV_LINE_MAX];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCBOOKオープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        if (n >= BOOK_MAX) {
            fclose(fp);
            fprintf(stderr, "SCBOOK件数超過\n");
            return -1;
        }
        if (mihft_parse_book(line, &books[n]) != 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOK解析失敗\n");
            return -1;
        }
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCBOOK読込失敗\n");
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int mihft_book_cmp(const void *pa, const void *pb)
{
    const MihftLocalBook *a = (const MihftLocalBook *)pa;
    const MihftLocalBook *b = (const MihftLocalBook *)pb;

    if (a->side_kbn != b->side_kbn) {
        return (a->side_kbn == 'S') ? -1 : 1;
    }
    if (a->side_kbn == 'S') {
        if (a->price_amt != b->price_amt) {
            return (a->price_amt < b->price_amt) ? -1 : 1;
        }
    } else {
        if (a->price_amt != b->price_amt) {
            return (a->price_amt > b->price_amt) ? -1 : 1;
        }
    }
    /* 同一価格帯内の充当順序(プライス・タイム等)は本部品では決定しない。
     * 概算表示の安定化のためレベル数量・板数量で並べるのみ。 */
    if (a->level_cnt != b->level_cnt) {
        return (a->level_cnt < b->level_cnt) ? -1 : 1;
    }
    if (a->book_qty != b->book_qty) {
        return (a->book_qty > b->book_qty) ? -1 : 1;
    }
    return 0;
}

static int mihft_is_crossable(const MihftLocalOrder *o, const MihftLocalBook *b)
{
    if (!mihft_streq(o->instr_code, b->instr_code)) {
        return 0;
    }
    if ((o->side_kbn == 'B' && b->side_kbn != 'S') || (o->side_kbn == 'S' && b->side_kbn != 'B')) {
        return 0;
    }
    if (o->ord_type == 'M') {
        return 1;
    }
    if (o->side_kbn == 'B') {
        return b->price_amt <= o->price_amt;
    }
    return b->price_amt >= o->price_amt;
}

static int mihft_validate_order(const MihftLocalOrder *o)
{
    int64_t tick;
    int64_t notional;

    if (mihft_tick_size(o->instr_tier, &tick) != 0) {
        return 12;
    }
    if (o->ord_type == 'L' && (o->price_amt == 0 || o->price_amt % tick != 0)) {
        return 12;
    }
    if (o->ord_type == 'M') {
        return 0;
    }
    if (mihft_mul_over_i64(o->ord_qty, o->price_amt, &notional) != 0) {
        return 8;
    }
    if (notional > MIHFT_MAX_NOTIONAL) {
        return 8;
    }
    return 0;
}

static int mihft_write_exec(FILE *out, const char *exec_id, const MihftLocalOrder *o, int64_t qty, int64_t price, int64_t ts)
{
    if (fprintf(out, "%s,%s,%s,%c,%lld,%lld,%lld\n",
                exec_id,
                o->order_id,
                o->instr_code,
                o->side_kbn,
                (long long)qty,
                (long long)price,
                (long long)ts) < 0) {
        return -1;
    }
    return 0;
}

/* 約定可能数量の概算行を生成する補助処理。
 *
 * 注意: 同一価格帯の約定選択順序(プライス・タイム)および TIF(IOC/FOK/DAY)の
 * 約定後処理(残数量の取消・全量条件・板残し)は当部品では一切判定しない。
 * それらの確定は約定エンジン本体 mihft_match に従う。ここでは画面・帳票向けに
 * クロス可能な板数量を単純に累計した「概算フィル」を出力するのみで、確定約定では
 * ないため発注の TIF 区分には依存しない。 */
static int mihft_execute_order(FILE *out, const MihftLocalOrder *o, const MihftLocalBook *books, size_t book_count, int *seq)
{
    int decision = mihft_validate_order(o);
    int64_t remain = o->ord_qty;
    size_t i;

    if (decision != 0) {
        return decision;
    }

    for (i = 0; i < book_count && remain > 0; i++) {
        char exec_id[EXEC_ID_LEN];
        int64_t qty;
        int64_t fill_amt;

        if (!mihft_is_crossable(o, &books[i]) || books[i].book_qty == 0) {
            continue;
        }

        qty = (books[i].book_qty < remain) ? books[i].book_qty : remain;
        if (mihft_mul_over_i64(qty, books[i].price_amt, &fill_amt) != 0) {
            return 8;
        }

        if (snprintf(exec_id, sizeof(exec_id), "EX%012d", *seq) < 0) {
            fprintf(stderr, "EXEC-ID生成失敗\n");
            return -1;
        }
        (*seq)++;

        if (mihft_write_exec(out, exec_id, o, qty, books[i].price_amt, books[i].entry_ts) != 0) {
            fprintf(stderr, "SCEXEC書込失敗\n");
            return -1;
        }

        remain -= qty;
    }

    return 0;
}

int main(void)
{
    MihftLocalOrder orders[ORDER_MAX];
    MihftLocalBook books[BOOK_MAX];
    size_t order_count = 0;
    size_t book_count = 0;
    size_t i;
    FILE *out;
    int exec_seq = 1;
    int last_decision = 0;

    if (mihft_read_orders(orders, &order_count) != 0) {
        return 91;
    }
    if (mihft_read_books(books, &book_count) != 0) {
        return 92;
    }

    qsort(books, book_count, sizeof(books[0]), mihft_book_cmp);

    out = fopen(SCEXEC_PATH, "w");
    if (out == NULL) {
        fprintf(stderr, "SCEXECオープン失敗\n");
        return 93;
    }

    for (i = 0; i < order_count; i++) {
        int decision = mihft_execute_order(out, &orders[i], books, book_count, &exec_seq);
        if (decision < 0) {
            fclose(out);
            return 94;
        }
        if (decision != 0 && last_decision == 0) {
            last_decision = decision;
        }
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "SCEXECクローズ失敗\n");
        return 95;
    }

    return last_decision;
}
