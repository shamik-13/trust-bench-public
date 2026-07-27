/*
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    20240709    福田 亮太 (E-211)  寄付き板寄せ照合の初版作成
 * 1.01    20241209    中川 美和 (E-283)  CSV検証、桁あふれ検査、価格決定順位を追加
 */
#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_MAX_BOOK 4096
#define MIHFT_MAX_INST 512
#define MIHFT_MAX_LINE 1024
#define MIHFT_SIDE_BUY 'B'
#define MIHFT_SIDE_SELL 'S'

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int order_cnt;
    char entry_ts[32];
} MihftBookRow;

typedef struct {
    char instr_code[32];
    char instr_name[128];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[16];
} MihftInstRow;

typedef struct {
    int64_t price;
    int64_t executable;
    int64_t imbalance;
    int64_t ref_distance;
} MihftCandidate;

static void mihft_trim_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_split_csv(char *line, char **cols, size_t max_cols)
{
    size_t count = 0;
    char *p = line;

    while (count < max_cols) {
        cols[count++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return (int)count;
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

static int mihft_copy_text(char *dst, size_t dst_sz, const char *src)
{
    size_t n;

    if (dst_sz == 0 || src == NULL) {
        return -1;
    }
    n = strlen(src);
    if (n >= dst_sz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int mihft_read_book(const char *path, MihftBookRow *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "入力ファイルを開けません: %s\n", path);
        return -1;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fprintf(stderr, "板ファイルが空です: %s\n", path);
        fclose(fp);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[7];
        int coln;
        MihftBookRow r;

        mihft_trim_eol(line);
        coln = mihft_split_csv(line, cols, 7);
        if (coln != 7 || n >= cap) {
            fprintf(stderr, "板CSV形式が不正です\n");
            fclose(fp);
            return -1;
        }
        memset(&r, 0, sizeof(r));
        if (mihft_copy_text(r.instr_code, sizeof(r.instr_code), cols[0]) != 0 ||
            strlen(cols[1]) != 1 ||
            (cols[1][0] != MIHFT_SIDE_BUY && cols[1][0] != MIHFT_SIDE_SELL) ||
            mihft_parse_int(cols[2], &r.level_cnt) != 0 ||
            mihft_parse_i64(cols[3], &r.price_amt) != 0 ||
            mihft_parse_i64(cols[4], &r.book_qty) != 0 ||
            mihft_parse_int(cols[5], &r.order_cnt) != 0 ||
            mihft_copy_text(r.entry_ts, sizeof(r.entry_ts), cols[6]) != 0 ||
            r.level_cnt < 0 || r.price_amt <= 0 || r.book_qty < 0 || r.order_cnt < 0) {
            fprintf(stderr, "板CSV値が不正です\n");
            fclose(fp);
            return -1;
        }
        r.side_kbn = cols[1][0];
        rows[n++] = r;
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

static int mihft_read_inst(const char *path, MihftInstRow *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "銘柄ファイルを開けません: %s\n", path);
        return -1;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fprintf(stderr, "銘柄ファイルが空です: %s\n", path);
        fclose(fp);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[6];
        int coln;
        MihftInstRow r;

        mihft_trim_eol(line);
        coln = mihft_split_csv(line, cols, 6);
        if (coln != 6 || n >= cap) {
            fprintf(stderr, "銘柄CSV形式が不正です\n");
            fclose(fp);
            return -1;
        }
        memset(&r, 0, sizeof(r));
        if (mihft_copy_text(r.instr_code, sizeof(r.instr_code), cols[0]) != 0 ||
            mihft_copy_text(r.instr_name, sizeof(r.instr_name), cols[1]) != 0 ||
            mihft_parse_int(cols[2], &r.instr_tier) != 0 ||
            mihft_parse_i64(cols[3], &r.tick_amt) != 0 ||
            mihft_parse_i64(cols[4], &r.lot_qty) != 0 ||
            mihft_copy_text(r.board_code, sizeof(r.board_code), cols[5]) != 0 ||
            r.instr_tier < 1 || r.instr_tier > 3 || r.tick_amt <= 0 || r.lot_qty <= 0) {
            fprintf(stderr, "銘柄CSV値が不正です\n");
            fclose(fp);
            return -1;
        }
        rows[n++] = r;
    }

    if (ferror(fp)) {
        fprintf(stderr, "銘柄ファイル読込に失敗しました\n");
        fclose(fp);
        return -1;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static int mihft_cmp_book(const void *a, const void *b)
{
    const MihftBookRow *x = (const MihftBookRow *)a;
    const MihftBookRow *y = (const MihftBookRow *)b;
    int c = strcmp(x->instr_code, y->instr_code);

    if (c != 0) {
        return c;
    }
    if (x->side_kbn != y->side_kbn) {
        return (x->side_kbn == MIHFT_SIDE_BUY) ? -1 : 1;
    }
    if (x->side_kbn == MIHFT_SIDE_BUY && x->price_amt != y->price_amt) {
        return (x->price_amt > y->price_amt) ? -1 : 1;
    }
    if (x->side_kbn == MIHFT_SIDE_SELL && x->price_amt != y->price_amt) {
        return (x->price_amt < y->price_amt) ? -1 : 1;
    }
    if (x->level_cnt != y->level_cnt) {
        return (x->level_cnt < y->level_cnt) ? -1 : 1;
    }
    /* 同一価格帯内の充当順序は約定エンジン本体(mihft_match)に従う。
     * 本コンポーネントは寄付の出力行を安定させるためレベル数量で並べるのみ。 */
    if (x->book_qty != y->book_qty) {
        return (x->book_qty > y->book_qty) ? -1 : 1;
    }
    return 0;
}

static int64_t mihft_abs_i64(int64_t v)
{
    return v < 0 ? -v : v;
}

static int mihft_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int mihft_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a < 0 || b < 0 || (a != 0 && b > INT64_MAX / a)) {
        return -1;
    }
    *out = a * b;
    return 0;
}

static int mihft_find_inst(const MihftInstRow *inst, size_t inst_count, const char *code, MihftInstRow *out)
{
    size_t i;

    for (i = 0; i < inst_count; i++) {
        if (strcmp(inst[i].instr_code, code) == 0) {
            *out = inst[i];
            return 0;
        }
    }
    return -1;
}

static int mihft_best_prices(const MihftBookRow *book, size_t book_count, const char *code, int64_t *bid, int64_t *ask)
{
    size_t i;
    int has_bid = 0;
    int has_ask = 0;

    *bid = 0;
    *ask = 0;
    for (i = 0; i < book_count; i++) {
        if (strcmp(book[i].instr_code, code) != 0 || book[i].book_qty <= 0) {
            continue;
        }
        if (book[i].side_kbn == MIHFT_SIDE_BUY && (!has_bid || book[i].price_amt > *bid)) {
            *bid = book[i].price_amt;
            has_bid = 1;
        }
        if (book[i].side_kbn == MIHFT_SIDE_SELL && (!has_ask || book[i].price_amt < *ask)) {
            *ask = book[i].price_amt;
            has_ask = 1;
        }
    }
    return (has_bid && has_ask) ? 0 : -1;
}

static int mihft_candidate_exists(const int64_t *prices, size_t count, int64_t price)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (prices[i] == price) {
            return 1;
        }
    }
    return 0;
}

static int mihft_collect_prices(const MihftBookRow *book, size_t book_count, const char *code, int64_t *prices, size_t cap, size_t *price_count)
{
    size_t i;
    size_t n = 0;

    for (i = 0; i < book_count; i++) {
        if (strcmp(book[i].instr_code, code) == 0 && book[i].book_qty > 0) {
            if (n >= cap) {
                return -1;
            }
            if (!mihft_candidate_exists(prices, n, book[i].price_amt)) {
                prices[n++] = book[i].price_amt;
            }
        }
    }
    *price_count = n;
    return 0;
}

static int mihft_eval_price(const MihftBookRow *book, size_t book_count, const char *code, int64_t price, int64_t ref, MihftCandidate *out)
{
    size_t i;
    int64_t buy = 0;
    int64_t sell = 0;

    for (i = 0; i < book_count; i++) {
        int64_t next;

        if (strcmp(book[i].instr_code, code) != 0 || book[i].book_qty <= 0) {
            continue;
        }
        if (book[i].side_kbn == MIHFT_SIDE_BUY && book[i].price_amt >= price) {
            if (mihft_add_i64(buy, book[i].book_qty, &next) != 0) {
                return -1;
            }
            buy = next;
        } else if (book[i].side_kbn == MIHFT_SIDE_SELL && book[i].price_amt <= price) {
            if (mihft_add_i64(sell, book[i].book_qty, &next) != 0) {
                return -1;
            }
            sell = next;
        }
    }

    out->price = price;
    out->executable = buy < sell ? buy : sell;
    out->imbalance = mihft_abs_i64(buy - sell);
    out->ref_distance = mihft_abs_i64(price - ref);
    return 0;
}

static int mihft_better_candidate(const MihftCandidate *a, const MihftCandidate *b)
{
    if (a->executable != b->executable) {
        return a->executable > b->executable;
    }
    if (a->imbalance != b->imbalance) {
        return a->imbalance < b->imbalance;
    }
    if (a->ref_distance != b->ref_distance) {
        return a->ref_distance < b->ref_distance;
    }
    return a->price < b->price;
}

static int mihft_decide_price(const MihftBookRow *book, size_t book_count, const char *code, int64_t *price, int64_t *qty)
{
    int64_t prices[MIHFT_MAX_BOOK];
    size_t price_count = 0;
    size_t i;
    int64_t bid;
    int64_t ask;
    int64_t ref;
    MihftCandidate best;
    int has_best = 0;

    if (mihft_best_prices(book, book_count, code, &bid, &ask) != 0 || bid < ask) {
        return 12;
    }
    if (mihft_add_i64(bid, ask, &ref) != 0) {
        return -1;
    }
    ref /= 2;

    if (mihft_collect_prices(book, book_count, code, prices, MIHFT_MAX_BOOK, &price_count) != 0) {
        return -1;
    }

    for (i = 0; i < price_count; i++) {
        MihftCandidate c;
        if (mihft_eval_price(book, book_count, code, prices[i], ref, &c) != 0) {
            return -1;
        }
        if (c.executable > 0 && (!has_best || mihft_better_candidate(&c, &best))) {
            best = c;
            has_best = 1;
        }
    }

    if (!has_best) {
        return 12;
    }
    *price = best.price;
    *qty = best.executable;
    return 0;
}

static int mihft_validate_tick_lot(const MihftBookRow *book, size_t book_count, const MihftInstRow *inst)
{
    size_t i;

    for (i = 0; i < book_count; i++) {
        if (strcmp(book[i].instr_code, inst->instr_code) != 0) {
            continue;
        }
        if (book[i].price_amt % inst->tick_amt != 0 || book[i].book_qty % inst->lot_qty != 0) {
            return 12;
        }
    }
    return 0;
}

static int mihft_write_exec_and_update(FILE *exec_fp, MihftBookRow *book, size_t book_count, const char *code, char side, int64_t price, int64_t target_qty, const char *ts)
{
    size_t i;
    int64_t remain = target_qty;
    int seq = 1;

    for (i = 0; i < book_count && remain > 0; i++) {
        int eligible = 0;
        int64_t fill;
        int64_t amount;

        if (strcmp(book[i].instr_code, code) != 0 || book[i].side_kbn != side || book[i].book_qty <= 0) {
            continue;
        }
        if (side == MIHFT_SIDE_BUY && book[i].price_amt >= price) {
            eligible = 1;
        }
        if (side == MIHFT_SIDE_SELL && book[i].price_amt <= price) {
            eligible = 1;
        }
        if (!eligible) {
            continue;
        }

        fill = book[i].book_qty < remain ? book[i].book_qty : remain;
        if (mihft_mul_i64(fill, price, &amount) != 0) {
            fprintf(stderr, "約定金額が上限を超えました\n");
            return -1;
        }
        fprintf(exec_fp, "AUC%06d,AUC-%s-%c-%03d,%s,%c,%lld,%lld,%s\n",
                seq, code, side, book[i].level_cnt, code, side,
                (long long)fill, (long long)amount, ts);

        book[i].book_qty -= fill;
        remain -= fill;
        seq++;
    }

    if (remain != 0) {
        fprintf(stderr, "約定数量の配分に失敗しました\n");
        return -1;
    }
    return 0;
}

static int mihft_write_book(const char *path, const MihftBookRow *book, size_t book_count)
{
    FILE *fp = fopen(path, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "板出力ファイルを開けません: %s\n", path);
        return -1;
    }

    fprintf(fp, "INSTR-CODE,SIDE-KBN,LEVEL-CNT,PRICE-AMT,BOOK-QTY,ORDER-CNT,ENTRY-TS\n");
    for (i = 0; i < book_count; i++) {
        fprintf(fp, "%s,%c,%d,%lld,%lld,%d,%s\n",
                book[i].instr_code, book[i].side_kbn, book[i].level_cnt,
                (long long)book[i].price_amt, (long long)book[i].book_qty,
                book[i].order_cnt, book[i].entry_ts);
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "板出力ファイルの確定に失敗しました\n");
        return -1;
    }
    return 0;
}

static int mihft_write_pos(const char *path, const char *code, int64_t qty, int64_t amount)
{
    FILE *fp = fopen(path, "w");

    if (fp == NULL) {
        fprintf(stderr, "建玉出力ファイルを開けません: %s\n", path);
        return -1;
    }
    fprintf(fp, "INSTR-CODE,NET-QTY,TRADED-AMT\n");
    fprintf(fp, "%s,0,%lld\n", code, (long long)amount);
    if (qty == 0 || fclose(fp) != 0) {
        fprintf(stderr, "建玉出力ファイルの確定に失敗しました\n");
        return -1;
    }
    return 0;
}

static void mihft_now(char *buf, size_t sz)
{
    time_t t = time(NULL);
    struct tm tmv;

#if defined(_WIN32)
    localtime_s(&tmv, &t);
#else
    localtime_r(&t, &tmv);
#endif
    strftime(buf, sz, "%Y%m%d%H%M%S", &tmv);
}

int main(void)
{
    MihftBookRow book[MIHFT_MAX_BOOK];
    MihftInstRow insts[MIHFT_MAX_INST];
    MihftInstRow inst;
    size_t book_count = 0;
    size_t inst_count = 0;
    int rc;
    int64_t price = 0;
    int64_t qty = 0;
    int64_t notional = 0;
    char ts[32];
    FILE *exec_fp;

    if (mihft_read_book("SCBOOK.csv", book, MIHFT_MAX_BOOK, &book_count) != 0 ||
        mihft_read_inst("SCINSTF.csv", insts, MIHFT_MAX_INST, &inst_count) != 0) {
        return 99;
    }
    if (book_count == 0 || mihft_find_inst(insts, inst_count, book[0].instr_code, &inst) != 0) {
        fprintf(stderr, "照合対象銘柄が不正です\n");
        return 99;
    }

    qsort(book, book_count, sizeof(book[0]), mihft_cmp_book);

    rc = mihft_validate_tick_lot(book, book_count, &inst);
    if (rc != 0) {
        return rc;
    }

    rc = mihft_decide_price(book, book_count, inst.instr_code, &price, &qty);
    if (rc != 0) {
        return rc;
    }

    if (mihft_mul_i64(price, qty, &notional) != 0) {
        fprintf(stderr, "想定元本が上限を超えました\n");
        return 99;
    }
    if (notional > MIHFT_MAX_NOTIONAL) {
        return 8;
    }

    exec_fp = fopen("SCEXEC.csv", "w");
    if (exec_fp == NULL) {
        fprintf(stderr, "約定出力ファイルを開けません: SCEXEC.csv\n");
        return 99;
    }
    fprintf(exec_fp, "EXEC-ID,ORDER-ID,INSTR-CODE,SIDE-KBN,FILL-QTY,FILL-AMT,EXEC-TS\n");

    mihft_now(ts, sizeof(ts));
    if (mihft_write_exec_and_update(exec_fp, book, book_count, inst.instr_code, MIHFT_SIDE_BUY, price, qty, ts) != 0 ||
        mihft_write_exec_and_update(exec_fp, book, book_count, inst.instr_code, MIHFT_SIDE_SELL, price, qty, ts) != 0) {
        fclose(exec_fp);
        return 99;
    }
    if (fclose(exec_fp) != 0) {
        fprintf(stderr, "約定出力ファイルの確定に失敗しました\n");
        return 99;
    }

    if (mihft_write_book("SCBOOK.out.csv", book, book_count) != 0 ||
        mihft_write_pos("mihft_pos.csv", inst.instr_code, qty, notional) != 0) {
        return 99;
    }

    return 0;
}
