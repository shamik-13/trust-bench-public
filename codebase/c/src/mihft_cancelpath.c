/************************************************************
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220222  小林 直樹 (E-252)  取消ホットパス初版作成
 * 1.01  20220722  三宅 拓也 (E-241)  CSV検査と取消照合を追加
 ************************************************************/

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_CANCEL_ACCEPT 0
#define MIHFT_CANCEL_REJECT_NOTIONAL 8
#define MIHFT_ERR_IO 64
#define MIHFT_ERR_PARSE 65
#define MIHFT_ERR_LIMIT 66

#define MIHFT_MAX_ORDERS 200000
#define MIHFT_MAX_BOOKS 200000
#define MIHFT_MAX_LINE 512
#define MIHFT_ID_LEN 32
#define MIHFT_CIF_LEN 32
#define MIHFT_INSTR_LEN 32
#define MIHFT_TS_LEN 32
#define MIHFT_EVENT_CANCEL "CXL"

typedef struct {
    char order_id[MIHFT_ID_LEN];
    char cif_no[MIHFT_CIF_LEN];
    char instr_code[MIHFT_INSTR_LEN];
    char state_kbn;
    int64_t leaves_qty;
    int64_t cum_qty;
    int64_t avg_fill_amt;
    char last_upd_ts[MIHFT_TS_LEN];
} scords_rec_t;

typedef struct {
    char instr_code[MIHFT_INSTR_LEN];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;
    char entry_ts[MIHFT_TS_LEN];
} scbook_rec_t;

typedef struct {
    uint64_t seq_no;
    char event_ts[MIHFT_TS_LEN];
    char order_id[MIHFT_ID_LEN];
    char instr_code[MIHFT_INSTR_LEN];
    uint64_t payload_hash;
} scjrnf_rec_t;

static int trim_field(char *s)
{
    size_t n;

    while (isspace((unsigned char)*s)) {
        memmove(s, s + 1, strlen(s));
    }

    n = strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1])) {
        s[--n] = '\0';
    }

    if (n >= 2 && s[0] == '"' && s[n - 1] == '"') {
        memmove(s, s + 1, n - 2);
        s[n - 2] = '\0';
    }

    return 0;
}

static int split_csv(char *line, char **field, size_t want)
{
    size_t idx = 0;
    char *p = line;
    char *start = line;
    int quoted = 0;

    while (*p != '\0') {
        if (*p == '"') {
            quoted = !quoted;
        } else if (*p == ',' && !quoted) {
            if (idx >= want) {
                return -1;
            }
            *p = '\0';
            field[idx++] = start;
            start = p + 1;
        }
        p++;
    }

    if (idx >= want) {
        return -1;
    }
    field[idx++] = start;

    if (idx != want) {
        return -1;
    }

    for (idx = 0; idx < want; idx++) {
        trim_field(field[idx]);
    }

    return 0;
}

static int copy_text(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
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

static uint64_t hash_mix(uint64_t h, const char *s)
{
    while (*s != '\0') {
        h ^= (unsigned char)*s++;
        h *= UINT64_C(1099511628211);
    }
    return h;
}

static uint64_t payload_hash(const scords_rec_t *o, uint64_t seq)
{
    char buf[64];
    uint64_t h = UINT64_C(1469598103934665603);

    h = hash_mix(h, o->order_id);
    h = hash_mix(h, o->instr_code);
    snprintf(buf, sizeof(buf), "%" PRId64 ":%" PRIu64, o->leaves_qty, seq);
    h = hash_mix(h, buf);
    return h;
}

static void make_ts(char out[MIHFT_TS_LEN])
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_POSIX_VERSION)
    localtime_r(&now, &tmv);
#else
    {
        struct tm *tmp = localtime(&now);
        if (tmp != NULL) {
            tmv = *tmp;
        } else {
            memset(&tmv, 0, sizeof(tmv));
        }
    }
#endif
    strftime(out, MIHFT_TS_LEN, "%Y%m%d%H%M%S", &tmv);
}

static int read_scords(const char *path, scords_rec_t **out, size_t *cnt)
{
    FILE *fp = fopen(path, "r");
    scords_rec_t *rows;
    char line[MIHFT_MAX_LINE];
    size_t used = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCORDSを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    rows = calloc(MIHFT_MAX_ORDERS, sizeof(*rows));
    if (rows == NULL) {
        fclose(fp);
        fprintf(stderr, "SCORDS領域を確保できません\n");
        return MIHFT_ERR_LIMIT;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[8];
        scords_rec_t r;
        line[strcspn(line, "\r\n")] = '\0';
        if (line[0] == '\0') {
            continue;
        }
        if (used == 0 && strstr(line, "ORDER-ID") != NULL) {
            continue;
        }
        if (used >= MIHFT_MAX_ORDERS || split_csv(line, f, 8) != 0) {
            free(rows);
            fclose(fp);
            fprintf(stderr, "SCORDS形式不正\n");
            return MIHFT_ERR_PARSE;
        }
        memset(&r, 0, sizeof(r));
        if (copy_text(r.order_id, sizeof(r.order_id), f[0]) != 0 ||
            copy_text(r.cif_no, sizeof(r.cif_no), f[1]) != 0 ||
            copy_text(r.instr_code, sizeof(r.instr_code), f[2]) != 0 ||
            strlen(f[3]) != 1 ||
            parse_i64(f[4], &r.leaves_qty) != 0 ||
            parse_i64(f[5], &r.cum_qty) != 0 ||
            parse_i64(f[6], &r.avg_fill_amt) != 0 ||
            copy_text(r.last_upd_ts, sizeof(r.last_upd_ts), f[7]) != 0 ||
            r.leaves_qty < 0 || r.cum_qty < 0 || r.avg_fill_amt < 0) {
            free(rows);
            fclose(fp);
            fprintf(stderr, "SCORDS項目不正\n");
            return MIHFT_ERR_PARSE;
        }
        r.state_kbn = f[3][0];
        rows[used++] = r;
    }

    if (ferror(fp)) {
        free(rows);
        fclose(fp);
        fprintf(stderr, "SCORDS読込失敗\n");
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    *out = rows;
    *cnt = used;
    return MIHFT_CANCEL_ACCEPT;
}

static int read_scbook(const char *path, scbook_rec_t **out, size_t *cnt)
{
    FILE *fp = fopen(path, "r");
    scbook_rec_t *rows;
    char line[MIHFT_MAX_LINE];
    size_t used = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCBOOKを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    rows = calloc(MIHFT_MAX_BOOKS, sizeof(*rows));
    if (rows == NULL) {
        fclose(fp);
        fprintf(stderr, "SCBOOK領域を確保できません\n");
        return MIHFT_ERR_LIMIT;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[7];
        scbook_rec_t r;
        line[strcspn(line, "\r\n")] = '\0';
        if (line[0] == '\0') {
            continue;
        }
        if (used == 0 && strstr(line, "INSTR-CODE") != NULL) {
            continue;
        }
        if (used >= MIHFT_MAX_BOOKS || split_csv(line, f, 7) != 0) {
            free(rows);
            fclose(fp);
            fprintf(stderr, "SCBOOK形式不正\n");
            return MIHFT_ERR_PARSE;
        }
        memset(&r, 0, sizeof(r));
        if (copy_text(r.instr_code, sizeof(r.instr_code), f[0]) != 0 ||
            strlen(f[1]) != 1 || (f[1][0] != 'B' && f[1][0] != 'S') ||
            parse_int(f[2], &r.level_cnt) != 0 ||
            parse_i64(f[3], &r.price_amt) != 0 ||
            parse_i64(f[4], &r.book_qty) != 0 ||
            parse_i64(f[5], &r.order_cnt) != 0 ||
            copy_text(r.entry_ts, sizeof(r.entry_ts), f[6]) != 0 ||
            r.level_cnt < 0 || r.price_amt < 0 || r.book_qty < 0 || r.order_cnt < 0) {
            free(rows);
            fclose(fp);
            fprintf(stderr, "SCBOOK項目不正\n");
            return MIHFT_ERR_PARSE;
        }
        r.side_kbn = f[1][0];
        rows[used++] = r;
    }

    if (ferror(fp)) {
        free(rows);
        fclose(fp);
        fprintf(stderr, "SCBOOK読込失敗\n");
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    *out = rows;
    *cnt = used;
    return MIHFT_CANCEL_ACCEPT;
}

static scords_rec_t *find_order(scords_rec_t *orders, size_t n, const char *order_id)
{
    size_t i;

    for (i = 0; i < n; i++) {
        if (strcmp(orders[i].order_id, order_id) == 0) {
            return &orders[i];
        }
    }
    return NULL;
}

static void remove_book_qty(scbook_rec_t *books, size_t n, const char *instr, int64_t qty)
{
    size_t i;

    for (i = 0; i < n && qty > 0; i++) {
        int64_t take;
        if (strcmp(books[i].instr_code, instr) != 0 || books[i].book_qty <= 0) {
            continue;
        }
        take = books[i].book_qty < qty ? books[i].book_qty : qty;
        books[i].book_qty -= take;
        qty -= take;
        if (books[i].book_qty == 0 && books[i].order_cnt > 0) {
            books[i].order_cnt--;
        }
    }
}

static int write_scords(const char *path, const scords_rec_t *orders, size_t n)
{
    FILE *fp = fopen(path, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "SCORDS出力を開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    fprintf(fp, "ORDER-ID,CIF-NO,INSTR-CODE,STATE-KBN,LEAVES-QTY,CUM-QTY,AVG-FILL-AMT,LAST-UPD-TS\n");
    for (i = 0; i < n; i++) {
        if (fprintf(fp, "%s,%s,%s,%c,%" PRId64 ",%" PRId64 ",%" PRId64 ",%s\n",
                    orders[i].order_id, orders[i].cif_no, orders[i].instr_code,
                    orders[i].state_kbn, orders[i].leaves_qty, orders[i].cum_qty,
                    orders[i].avg_fill_amt, orders[i].last_upd_ts) < 0) {
            fclose(fp);
            fprintf(stderr, "SCORDS出力失敗\n");
            return MIHFT_ERR_IO;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCORDS終了失敗\n");
        return MIHFT_ERR_IO;
    }
    return MIHFT_CANCEL_ACCEPT;
}

static int write_scbook(const char *path, const scbook_rec_t *books, size_t n)
{
    FILE *fp = fopen(path, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "SCBOOK出力を開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    fprintf(fp, "INSTR-CODE,SIDE-KBN,LEVEL-CNT,PRICE-AMT,BOOK-QTY,ORDER-CNT,ENTRY-TS\n");
    for (i = 0; i < n; i++) {
        if (fprintf(fp, "%s,%c,%d,%" PRId64 ",%" PRId64 ",%" PRId64 ",%s\n",
                    books[i].instr_code, books[i].side_kbn, books[i].level_cnt,
                    books[i].price_amt, books[i].book_qty, books[i].order_cnt,
                    books[i].entry_ts) < 0) {
            fclose(fp);
            fprintf(stderr, "SCBOOK出力失敗\n");
            return MIHFT_ERR_IO;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCBOOK終了失敗\n");
        return MIHFT_ERR_IO;
    }
    return MIHFT_CANCEL_ACCEPT;
}

static int write_journal(const char *path, const scjrnf_rec_t *jrns, size_t n)
{
    FILE *fp = fopen(path, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "SCJRNF出力を開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    fprintf(fp, "SEQ-NO,EVENT-TS,EVENT-KBN,ORDER-ID,INSTR-CODE,PAYLOAD-HASH\n");
    for (i = 0; i < n; i++) {
        if (fprintf(fp, "%" PRIu64 ",%s,%s,%s,%s,%" PRIu64 "\n",
                    jrns[i].seq_no, jrns[i].event_ts, MIHFT_EVENT_CANCEL,
                    jrns[i].order_id, jrns[i].instr_code, jrns[i].payload_hash) < 0) {
            fclose(fp);
            fprintf(stderr, "SCJRNF出力失敗\n");
            return MIHFT_ERR_IO;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCJRNF終了失敗\n");
        return MIHFT_ERR_IO;
    }
    return MIHFT_CANCEL_ACCEPT;
}

int main(void)
{
    scords_rec_t *orders = NULL;
    scbook_rec_t *books = NULL;
    scjrnf_rec_t *jrns = NULL;
    size_t order_cnt = 0;
    size_t book_cnt = 0;
    size_t jrn_cnt = 0;
    uint64_t seq = 1;
    char line[MIHFT_MAX_LINE];
    int rc;

    rc = read_scords("SCORDS.csv", &orders, &order_cnt);
    if (rc != MIHFT_CANCEL_ACCEPT) {
        return rc;
    }

    rc = read_scbook("SCBOOK.csv", &books, &book_cnt);
    if (rc != MIHFT_CANCEL_ACCEPT) {
        free(orders);
        return rc;
    }

    jrns = calloc(order_cnt == 0 ? 1 : order_cnt, sizeof(*jrns));
    if (jrns == NULL) {
        free(orders);
        free(books);
        fprintf(stderr, "SCJRNF領域を確保できません\n");
        return MIHFT_ERR_LIMIT;
    }

    while (fgets(line, sizeof(line), stdin) != NULL) {
        char *f[1];
        scords_rec_t *o;
        int64_t cancel_qty;

        line[strcspn(line, "\r\n")] = '\0';
        if (line[0] == '\0') {
            continue;
        }
        if (jrn_cnt == 0 && strstr(line, "ORDER-ID") != NULL) {
            continue;
        }
        if (split_csv(line, f, 1) != 0) {
            rc = MIHFT_ERR_PARSE;
            fprintf(stderr, "取消依頼形式不正\n");
            goto finish;
        }

        o = find_order(orders, order_cnt, f[0]);
        if (o == NULL || o->state_kbn == 'F' || o->state_kbn == 'R' || o->leaves_qty <= 0) {
            rc = MIHFT_CANCEL_REJECT_NOTIONAL;
            continue;
        }

        cancel_qty = o->leaves_qty;
        make_ts(o->last_upd_ts);
        o->state_kbn = 'C';
        o->leaves_qty = 0;

        jrns[jrn_cnt].seq_no = seq++;
        copy_text(jrns[jrn_cnt].event_ts, sizeof(jrns[jrn_cnt].event_ts), o->last_upd_ts);
        copy_text(jrns[jrn_cnt].order_id, sizeof(jrns[jrn_cnt].order_id), o->order_id);
        copy_text(jrns[jrn_cnt].instr_code, sizeof(jrns[jrn_cnt].instr_code), o->instr_code);
        jrns[jrn_cnt].payload_hash = payload_hash(o, jrns[jrn_cnt].seq_no);
        jrn_cnt++;

        remove_book_qty(books, book_cnt, o->instr_code, cancel_qty);
        rc = MIHFT_CANCEL_ACCEPT;
    }

    if (ferror(stdin)) {
        rc = MIHFT_ERR_IO;
        fprintf(stderr, "取消依頼読込失敗\n");
        goto finish;
    }

    rc = write_scords("SCORDS.out.csv", orders, order_cnt);
    if (rc != MIHFT_CANCEL_ACCEPT) {
        goto finish;
    }
    rc = write_scbook("SCBOOK.out.csv", books, book_cnt);
    if (rc != MIHFT_CANCEL_ACCEPT) {
        goto finish;
    }
    rc = write_journal("SCJRNF.csv", jrns, jrn_cnt);

finish:
    free(jrns);
    free(books);
    free(orders);
    return rc;
}
