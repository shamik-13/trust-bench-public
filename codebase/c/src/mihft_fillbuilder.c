/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240709  大野 修 (E-225)  約定フィル生成の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_LINE_MAX 512
#define MIHFT_ORDER_MAX 4096
#define MIHFT_BOOK_MAX 8192
#define MIHFT_EXEC_MAX 8192

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
} mihft_order_row;

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int order_cnt;
    int64_t entry_ts;
} mihft_book_row;

typedef struct {
    char exec_id[48];
    char order_id[32];
    char instr_code[32];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    int64_t exec_ts;
} mihft_exec_row;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_next_field(char **cur, char *dst, size_t dstsz)
{
    char *p = *cur;
    char *q;
    size_t len;

    if (dstsz == 0U || p == NULL) {
        return -1;
    }

    q = strchr(p, ',');
    if (q != NULL) {
        len = (size_t)(q - p);
        *cur = q + 1;
    } else {
        len = strlen(p);
        *cur = NULL;
    }

    if (len >= dstsz) {
        return -1;
    }

    memcpy(dst, p, len);
    dst[len] = '\0';
    return 0;
}

static int mihft_parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

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

static int mihft_read_orders(const char *path, mihft_order_row *rows, size_t cap, size_t *cnt)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    size_t n = 0U;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "注文ファイルを開けません: %s\n", path);
        return -1;
    }

    if (fgets(line, sizeof line, fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "注文ヘッダを読めません\n");
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *cur;
        char buf[9][64];

        if (n >= cap) {
            fclose(fp);
            fprintf(stderr, "注文件数が上限を超過しました\n");
            return -1;
        }

        mihft_chomp(line);
        cur = line;

        for (int i = 0; i < 9; i++) {
            if (mihft_next_field(&cur, buf[i], sizeof buf[i]) != 0) {
                fclose(fp);
                fprintf(stderr, "注文CSVの項目が不正です\n");
                return -1;
            }
        }

        if (buf[3][1] != '\0' || buf[4][1] != '\0') {
            fclose(fp);
            fprintf(stderr, "注文コード値が不正です\n");
            return -1;
        }

        snprintf(rows[n].order_id, sizeof rows[n].order_id, "%s", buf[0]);
        snprintf(rows[n].cif_no, sizeof rows[n].cif_no, "%s", buf[1]);
        snprintf(rows[n].instr_code, sizeof rows[n].instr_code, "%s", buf[2]);
        rows[n].side_kbn = buf[3][0];
        rows[n].ord_type = buf[4][0];
        snprintf(rows[n].tif_code, sizeof rows[n].tif_code, "%s", buf[5]);

        if (mihft_parse_i64(buf[6], &rows[n].ord_qty) != 0 ||
            mihft_parse_i64(buf[7], &rows[n].price_amt) != 0 ||
            mihft_parse_int(buf[8], &rows[n].instr_tier) != 0) {
            fclose(fp);
            fprintf(stderr, "注文数値項目が不正です\n");
            return -1;
        }

        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "注文ファイル読込で障害が発生しました\n");
        return -1;
    }

    fclose(fp);
    *cnt = n;
    return 0;
}

static int mihft_read_books(const char *path, mihft_book_row *rows, size_t cap, size_t *cnt)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    size_t n = 0U;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "板ファイルを開けません: %s\n", path);
        return -1;
    }

    if (fgets(line, sizeof line, fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "板ヘッダを読めません\n");
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *cur;
        char buf[7][64];

        if (n >= cap) {
            fclose(fp);
            fprintf(stderr, "板件数が上限を超過しました\n");
            return -1;
        }

        mihft_chomp(line);
        cur = line;

        for (int i = 0; i < 7; i++) {
            if (mihft_next_field(&cur, buf[i], sizeof buf[i]) != 0) {
                fclose(fp);
                fprintf(stderr, "板CSVの項目が不正です\n");
                return -1;
            }
        }

        if (buf[1][1] != '\0') {
            fclose(fp);
            fprintf(stderr, "板サイド値が不正です\n");
            return -1;
        }

        snprintf(rows[n].instr_code, sizeof rows[n].instr_code, "%s", buf[0]);
        rows[n].side_kbn = buf[1][0];

        if (mihft_parse_int(buf[2], &rows[n].level_cnt) != 0 ||
            mihft_parse_i64(buf[3], &rows[n].price_amt) != 0 ||
            mihft_parse_i64(buf[4], &rows[n].book_qty) != 0 ||
            mihft_parse_int(buf[5], &rows[n].order_cnt) != 0 ||
            mihft_parse_i64(buf[6], &rows[n].entry_ts) != 0) {
            fclose(fp);
            fprintf(stderr, "板数値項目が不正です\n");
            return -1;
        }

        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "板ファイル読込で障害が発生しました\n");
        return -1;
    }

    fclose(fp);
    *cnt = n;
    return 0;
}

static int64_t mihft_now_ts(void)
{
    return (int64_t)time(NULL);
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

static int mihft_crossable(const mihft_order_row *ord, const mihft_book_row *book)
{
    if (strcmp(ord->instr_code, book->instr_code) != 0) {
        return 0;
    }
    if (ord->side_kbn == book->side_kbn) {
        return 0;
    }
    if (ord->ord_type == 'M') {
        return 1;
    }
    if (ord->side_kbn == 'B') {
        return ord->price_amt >= book->price_amt;
    }
    return ord->price_amt <= book->price_amt;
}

static int mihft_validate_order(const mihft_order_row *ord)
{
    int64_t tick;
    int64_t notional;

    if ((ord->side_kbn != 'B' && ord->side_kbn != 'S') ||
        (ord->ord_type != 'L' && ord->ord_type != 'M') ||
        (strcmp(ord->tif_code, "DAY") != 0 &&
         strcmp(ord->tif_code, "IOC") != 0 &&
         strcmp(ord->tif_code, "FOK") != 0) ||
        ord->ord_qty <= 0 || ord->price_amt < 0) {
        return 8;
    }

    if (mihft_tick_size(ord->instr_tier, &tick) != 0) {
        return 12;
    }

    if (ord->ord_type == 'L' && ord->price_amt % tick != 0) {
        return 12;
    }

    if (ord->price_amt != 0 && ord->ord_qty > INT64_MAX / ord->price_amt) {
        return 8;
    }

    notional = ord->ord_qty * ord->price_amt;
    if (notional > MIHFT_MAX_NOTIONAL) {
        return 8;
    }

    return 0;
}

static int mihft_make_exec_id(char *dst, size_t dstsz, const mihft_order_row *ord, size_t seq)
{
    int n = snprintf(dst, dstsz, "EX%s%06zu", ord->order_id, seq + 1U);
    return (n > 0 && (size_t)n < dstsz) ? 0 : -1;
}

static int mihft_write_execs(const char *path, const mihft_exec_row *rows, size_t cnt)
{
    FILE *fp = fopen(path, "w");

    if (fp == NULL) {
        fprintf(stderr, "約定ファイルを開けません: %s\n", path);
        return -1;
    }

    if (fprintf(fp, "EXEC-ID,ORDER-ID,INSTR-CODE,SIDE-KBN,FILL-QTY,FILL-AMT,EXEC-TS\n") < 0) {
        fclose(fp);
        fprintf(stderr, "約定ヘッダを書けません\n");
        return -1;
    }

    for (size_t i = 0U; i < cnt; i++) {
        if (fprintf(fp, "%s,%s,%s,%c,%lld,%lld,%lld\n",
                    rows[i].exec_id,
                    rows[i].order_id,
                    rows[i].instr_code,
                    rows[i].side_kbn,
                    (long long)rows[i].fill_qty,
                    (long long)rows[i].fill_amt,
                    (long long)rows[i].exec_ts) < 0) {
            fclose(fp);
            fprintf(stderr, "約定行を書けません\n");
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "約定ファイルの終了処理に失敗しました\n");
        return -1;
    }

    return 0;
}

int main(void)
{
    mihft_order_row orders[MIHFT_ORDER_MAX];
    mihft_book_row books[MIHFT_BOOK_MAX];
    mihft_exec_row execs[MIHFT_EXEC_MAX];
    size_t order_cnt = 0U;
    size_t book_cnt = 0U;
    size_t exec_cnt = 0U;
    int final_code = 0;

    if (mihft_read_orders("SCORDF.csv", orders, MIHFT_ORDER_MAX, &order_cnt) != 0) {
        return 90;
    }
    if (mihft_read_books("SCBOOK.csv", books, MIHFT_BOOK_MAX, &book_cnt) != 0) {
        return 91;
    }

    for (size_t oi = 0U; oi < order_cnt; oi++) {
        int code = mihft_validate_order(&orders[oi]);
        int64_t remain = orders[oi].ord_qty;

        if (code != 0) {
            final_code = code;
            continue;
        }

        for (size_t bi = 0U; bi < book_cnt && remain > 0; bi++) {
            int64_t fill_qty;
            int64_t fill_amt;

            if (!mihft_crossable(&orders[oi], &books[bi]) || books[bi].book_qty <= 0) {
                continue;
            }

            fill_qty = remain < books[bi].book_qty ? remain : books[bi].book_qty;
            if (fill_qty <= 0) {
                continue;
            }
            if (books[bi].price_amt != 0 && fill_qty > INT64_MAX / books[bi].price_amt) {
                return 92;
            }

            fill_amt = fill_qty * books[bi].price_amt;
            if (exec_cnt >= MIHFT_EXEC_MAX) {
                fprintf(stderr, "約定件数が上限を超過しました\n");
                return 93;
            }

            if (mihft_make_exec_id(execs[exec_cnt].exec_id,
                                  sizeof execs[exec_cnt].exec_id,
                                  &orders[oi],
                                  exec_cnt) != 0) {
                fprintf(stderr, "約定ID生成に失敗しました\n");
                return 94;
            }

            snprintf(execs[exec_cnt].order_id, sizeof execs[exec_cnt].order_id, "%s", orders[oi].order_id);
            snprintf(execs[exec_cnt].instr_code, sizeof execs[exec_cnt].instr_code, "%s", orders[oi].instr_code);
            execs[exec_cnt].side_kbn = orders[oi].side_kbn;
            execs[exec_cnt].fill_qty = fill_qty;
            execs[exec_cnt].fill_amt = fill_amt;
            execs[exec_cnt].exec_ts = mihft_now_ts();

            books[bi].book_qty -= fill_qty;
            if (books[bi].book_qty == 0 && books[bi].order_cnt > 0) {
                books[bi].order_cnt--;
            }

            remain -= fill_qty;
            exec_cnt++;
        }

        /* TIF(IOC/FOK/DAY)の約定後処理は本ビルダーでは判定しない。
         * 有効期間に応じた残数量の取消・全量条件の成否は mihft_match 本体が確定し、
         * 当コンポーネントは確定済みフィルの記録生成のみを担う。 */
        (void)remain;
    }

    if (mihft_write_execs("SCEXEC.csv", execs, exec_cnt) != 0) {
        return 95;
    }

    return final_code;
}
