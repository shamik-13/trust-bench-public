/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20190416  今井 彩 (E-230)  初版作成、市場データ鮮度検査の判定前処理を実装
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_MAX_LINE 512
#define MIHFT_MAX_REC 4096
#define MIHFT_MAX_FIELD 16
#define MIHFT_MAX_DELAY_MS 50LL
#define MIHFT_REJECT_IO 2
#define MIHFT_REJECT_PARSE 3

typedef struct {
    char instr_code[32];
    long long bid_amt;
    long long ask_amt;
    long long last_amt;
    long long vol_qty;
    long long tick_ts;
} MktRec;

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    long long price_amt;
    long long book_qty;
    long long order_cnt;
    long long entry_ts;
} BookRec;

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
} OrderRec;

typedef struct {
    long long rate_bp;
    long long tick;
} TierRule;

static void trim_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int split_csv(char *line, char **field, size_t max_field, size_t *count)
{
    char *p = line;
    size_t n = 0;

    while (n < max_field) {
        field[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            *count = n;
            return 0;
        }
        *p++ = '\0';
    }

    return -1;
}

static int copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dstsz) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_ll(const char *s, long long *out)
{
    char *end = NULL;
    long long v;

    if (*s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }

    *out = v;
    return 0;
}

static int parse_int(const char *s, int *out)
{
    long long v;

    if (parse_ll(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int read_mkt(const char *path, MktRec *rec, size_t cap, size_t *out_count)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0;

    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[MIHFT_MAX_FIELD];
        size_t c = 0;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (split_csv(line, f, MIHFT_MAX_FIELD, &c) != 0 || c != 6 || n >= cap) {
            fclose(fp);
            return -2;
        }
        if (copy_field(rec[n].instr_code, sizeof(rec[n].instr_code), f[0]) != 0 ||
            parse_ll(f[1], &rec[n].bid_amt) != 0 ||
            parse_ll(f[2], &rec[n].ask_amt) != 0 ||
            parse_ll(f[3], &rec[n].last_amt) != 0 ||
            parse_ll(f[4], &rec[n].vol_qty) != 0 ||
            parse_ll(f[5], &rec[n].tick_ts) != 0) {
            fclose(fp);
            return -2;
        }
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *out_count = n;
    return 0;
}

static int read_book(const char *path, BookRec *rec, size_t cap, size_t *out_count)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0;

    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[MIHFT_MAX_FIELD];
        size_t c = 0;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (split_csv(line, f, MIHFT_MAX_FIELD, &c) != 0 || c != 7 || n >= cap ||
            strlen(f[1]) != 1) {
            fclose(fp);
            return -2;
        }
        if (copy_field(rec[n].instr_code, sizeof(rec[n].instr_code), f[0]) != 0 ||
            parse_int(f[2], &rec[n].level_cnt) != 0 ||
            parse_ll(f[3], &rec[n].price_amt) != 0 ||
            parse_ll(f[4], &rec[n].book_qty) != 0 ||
            parse_ll(f[5], &rec[n].order_cnt) != 0 ||
            parse_ll(f[6], &rec[n].entry_ts) != 0) {
            fclose(fp);
            return -2;
        }
        rec[n].side_kbn = f[1][0];
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *out_count = n;
    return 0;
}

static int read_order(const char *path, OrderRec *rec, size_t cap, size_t *out_count)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0;

    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[MIHFT_MAX_FIELD];
        size_t c = 0;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (split_csv(line, f, MIHFT_MAX_FIELD, &c) != 0 || c != 9 || n >= cap ||
            strlen(f[3]) != 1 || strlen(f[4]) != 1) {
            fclose(fp);
            return -2;
        }
        if (copy_field(rec[n].order_id, sizeof(rec[n].order_id), f[0]) != 0 ||
            copy_field(rec[n].cif_no, sizeof(rec[n].cif_no), f[1]) != 0 ||
            copy_field(rec[n].instr_code, sizeof(rec[n].instr_code), f[2]) != 0 ||
            copy_field(rec[n].tif_code, sizeof(rec[n].tif_code), f[5]) != 0 ||
            parse_ll(f[6], &rec[n].ord_qty) != 0 ||
            parse_ll(f[7], &rec[n].price_amt) != 0 ||
            parse_int(f[8], &rec[n].instr_tier) != 0) {
            fclose(fp);
            return -2;
        }
        rec[n].side_kbn = f[3][0];
        rec[n].ord_type = f[4][0];
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *out_count = n;
    return 0;
}

static const MktRec *find_mkt(const MktRec *rec, size_t count, const char *instr_code)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(rec[i].instr_code, instr_code) == 0) {
            return &rec[i];
        }
    }
    return NULL;
}

static const BookRec *find_book(const BookRec *rec, size_t count, const char *instr_code, char side_kbn)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (rec[i].side_kbn == side_kbn && strcmp(rec[i].instr_code, instr_code) == 0) {
            return &rec[i];
        }
    }
    return NULL;
}

static int tier_rule(int tier, TierRule *rule)
{
    if (tier == 1) {
        rule->rate_bp = 1000;
        rule->tick = 100;
        return 0;
    }
    if (tier == 2) {
        rule->rate_bp = 2000;
        rule->tick = 500;
        return 0;
    }
    if (tier == 3) {
        rule->rate_bp = 4000;
        rule->tick = 1000;
        return 0;
    }
    return -1;
}

static int checked_notional(long long qty, long long price, long long *out)
{
    if (qty <= 0 || price <= 0) {
        return -1;
    }
    if (qty > LLONG_MAX / price) {
        return -1;
    }
    *out = qty * price;
    return 0;
}

static int write_reject(FILE *fp, long long reject_id, const OrderRec *ord,
                        int reject_cd, const char *detail_cd, long long reject_ts)
{
    if (fprintf(fp, "%lld,%s,%s,%s,%d,%s,%lld\n",
                reject_id,
                ord->order_id,
                ord->cif_no,
                ord->instr_code,
                reject_cd,
                detail_cd,
                reject_ts) < 0) {
        return -1;
    }
    return 0;
}

static long long now_epoch_ms(void)
{
    time_t t = time(NULL);

    if (t == (time_t)-1) {
        return 0;
    }
    return (long long)t * 1000LL;
}

int main(void)
{
    MktRec mkt[MIHFT_MAX_REC];
    BookRec book[MIHFT_MAX_REC];
    OrderRec ord[MIHFT_MAX_REC];
    size_t mkt_count = 0;
    size_t book_count = 0;
    size_t ord_count = 0;
    size_t i;
    FILE *rej_fp;
    long long reject_id = 1;
    int final_code = 0;

    if (read_mkt("SCMKTD.csv", mkt, MIHFT_MAX_REC, &mkt_count) != 0 ||
        read_book("SCBOOK.csv", book, MIHFT_MAX_REC, &book_count) != 0 ||
        read_order("SCORDF.csv", ord, MIHFT_MAX_REC, &ord_count) != 0) {
        fprintf(stderr, "入力読込に失敗しました\n");
        return MIHFT_REJECT_IO;
    }

    rej_fp = fopen("HFRJCT.dat", "w");
    if (rej_fp == NULL) {
        fprintf(stderr, "拒否出力の開始に失敗しました\n");
        return MIHFT_REJECT_IO;
    }

    for (i = 0; i < ord_count; i++) {
        const MktRec *mr = find_mkt(mkt, mkt_count, ord[i].instr_code);
        const BookRec *bb = find_book(book, book_count, ord[i].instr_code, 'B');
        const BookRec *ba = find_book(book, book_count, ord[i].instr_code, 'S');
        const BookRec *side_book = find_book(book, book_count, ord[i].instr_code, ord[i].side_kbn);
        TierRule rule;
        long long reject_ts = now_epoch_ms();
        long long notional = 0;
        int reject_cd = 0;
        const char *detail_cd = NULL;

        if (mr == NULL || bb == NULL || ba == NULL || side_book == NULL) {
            reject_cd = 8;
            detail_cd = "MDMISS";
        } else if (mr->ask_amt <= 0 || mr->bid_amt <= 0 || mr->bid_amt >= mr->ask_amt ||
                   bb->price_amt <= 0 || ba->price_amt <= 0 || bb->price_amt >= ba->price_amt) {
            reject_cd = 8;
            detail_cd = "BIDCROSS";
        } else if (mr->tick_ts < 0 || side_book->entry_ts < 0 ||
                   llabs(mr->tick_ts - side_book->entry_ts) > MIHFT_MAX_DELAY_MS) {
            reject_cd = 8;
            detail_cd = "STALEMD";
        } else if ((ord[i].side_kbn != 'B' && ord[i].side_kbn != 'S') ||
                   (ord[i].ord_type != 'L' && ord[i].ord_type != 'M') ||
                   (strcmp(ord[i].tif_code, "DAY") != 0 &&
                    strcmp(ord[i].tif_code, "IOC") != 0 &&
                    strcmp(ord[i].tif_code, "FOK") != 0) ||
                   tier_rule(ord[i].instr_tier, &rule) != 0) {
            reject_cd = 12;
            detail_cd = "ITEM";
        } else {
            long long base_price = ord[i].ord_type == 'M' ? side_book->price_amt : ord[i].price_amt;

            if (checked_notional(ord[i].ord_qty, base_price, &notional) != 0) {
                reject_cd = 8;
                detail_cd = "AMOUNT";
            } else if (notional > MIHFT_MAX_NOTIONAL) {
                reject_cd = 8;
                detail_cd = "NOTIONAL";
            } else if (ord[i].ord_type == 'L' && ord[i].price_amt % rule.tick != 0) {
                reject_cd = 12;
                detail_cd = "TICK";
            }
        }

        if (reject_cd != 0) {
            if (write_reject(rej_fp, reject_id++, &ord[i], reject_cd, detail_cd, reject_ts) != 0) {
                fclose(rej_fp);
                fprintf(stderr, "拒否出力に失敗しました\n");
                return MIHFT_REJECT_IO;
            }
            if (final_code == 0) {
                final_code = reject_cd;
            }
        }
    }

    if (fclose(rej_fp) != 0) {
        fprintf(stderr, "拒否出力の終了に失敗しました\n");
        return MIHFT_REJECT_IO;
    }

    return final_code;
}
