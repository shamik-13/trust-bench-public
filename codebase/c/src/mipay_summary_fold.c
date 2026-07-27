/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    20240620    精算連携    初版作成、日次精算サマリ単一走査処理
 */

#include "mipay_trace.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MI_MAX_LINE            1024
#define MI_MAX_FIELD           16
#define MI_MAX_MERCHANT        64
#define MI_MAX_KBN             32
#define MI_MAX_DATE            16
#define MI_MAX_REASON          64
#define MI_INPUT_DETAIL        "PCDTLF.csv"
#define MI_INPUT_CARRY         "PCCARF.csv"
#define MI_INPUT_KBN           "PCKBNF.csv"
#define MI_OUTPUT_SUMMARY      "PCSUMF.csv"
#define MI_SETTLE_DATE         "20250115"
#define MI_OK                  0
#define MI_ERR_IO              10
#define MI_ERR_PARSE           11
#define MI_ERR_MEMORY          12
#define MI_ERR_OVERFLOW        13

typedef struct {
    char kbn[MI_MAX_KBN];
    char name[64];
    int nettable;
    int32_t fee_ppm;
    char valid_from[MI_MAX_DATE];
    char valid_to[MI_MAX_DATE];
} mi_kbn_t;

typedef struct {
    char merchant[MI_MAX_MERCHANT];
    char kbn[MI_MAX_KBN];
    int64_t amount;
    char reason[MI_MAX_REASON];
    char next_settle_date[MI_MAX_DATE];
} mi_carry_t;

typedef struct {
    char merchant[MI_MAX_MERCHANT];
    char kbn[MI_MAX_KBN];
    int64_t txn_count;
    int64_t total_amount;
    int64_t carry_amount;
    int nettable;
    int blocked;
    char carry_reason[MI_MAX_REASON];
} mi_summary_t;

typedef struct {
    mi_kbn_t *items;
    size_t len;
    size_t cap;
} mi_kbn_vec_t;

typedef struct {
    mi_carry_t *items;
    size_t len;
    size_t cap;
} mi_carry_vec_t;

typedef struct {
    mi_summary_t *items;
    size_t len;
    size_t cap;
} mi_summary_vec_t;

static void mi_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int mi_split_csv(char *line, char **field, int max_field)
{
    int n = 0;
    char *p = line;

    while (*p != '\0' && n < max_field) {
        if (*p == '"') {
            char *w;
            p++;
            field[n++] = p;
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
            field[n++] = p;
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

static int mi_parse_i64(const char *s, int64_t *out)
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

static int mi_parse_i32(const char *s, int32_t *out)
{
    int64_t v;

    if (mi_parse_i64(s, &v) != 0 || v < INT32_MIN || v > INT32_MAX) {
        return -1;
    }
    *out = (int32_t)v;
    return 0;
}

static int mi_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int mi_copy(char *dst, size_t dst_size, const char *src)
{
    size_t n;

    if (src == NULL) {
        return -1;
    }
    n = strlen(src);
    if (n >= dst_size) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int mi_reserve_kbn(mi_kbn_vec_t *v)
{
    mi_kbn_t *p;
    size_t next;

    if (v->len < v->cap) {
        return 0;
    }
    next = v->cap == 0 ? 16u : v->cap * 2u;
    if (next < v->cap) {
        return -1;
    }
    p = (mi_kbn_t *)realloc(v->items, next * sizeof(*p));
    if (p == NULL) {
        return -1;
    }
    v->items = p;
    v->cap = next;
    return 0;
}

static int mi_reserve_carry(mi_carry_vec_t *v)
{
    mi_carry_t *p;
    size_t next;

    if (v->len < v->cap) {
        return 0;
    }
    next = v->cap == 0 ? 32u : v->cap * 2u;
    if (next < v->cap) {
        return -1;
    }
    p = (mi_carry_t *)realloc(v->items, next * sizeof(*p));
    if (p == NULL) {
        return -1;
    }
    v->items = p;
    v->cap = next;
    return 0;
}

static int mi_reserve_summary(mi_summary_vec_t *v)
{
    mi_summary_t *p;
    size_t next;

    if (v->len < v->cap) {
        return 0;
    }
    next = v->cap == 0 ? 128u : v->cap * 2u;
    if (next < v->cap) {
        return -1;
    }
    p = (mi_summary_t *)realloc(v->items, next * sizeof(*p));
    if (p == NULL) {
        return -1;
    }
    v->items = p;
    v->cap = next;
    return 0;
}

static const mi_kbn_t *mi_find_kbn(const mi_kbn_vec_t *v, const char *kbn)
{
    size_t i;

    for (i = 0; i < v->len; i++) {
        if (strcmp(v->items[i].kbn, kbn) == 0) {
            return &v->items[i];
        }
    }
    return NULL;
}

static mi_summary_t *mi_find_summary(mi_summary_vec_t *v, const char *merchant, const char *kbn)
{
    size_t i;

    for (i = 0; i < v->len; i++) {
        if (strcmp(v->items[i].merchant, merchant) == 0 &&
            strcmp(v->items[i].kbn, kbn) == 0) {
            return &v->items[i];
        }
    }
    return NULL;
}

static mi_summary_t *mi_get_summary(mi_summary_vec_t *v, const char *merchant, const char *kbn)
{
    mi_summary_t *s;

    s = mi_find_summary(v, merchant, kbn);
    if (s != NULL) {
        return s;
    }
    if (mi_reserve_summary(v) != 0) {
        return NULL;
    }
    s = &v->items[v->len++];
    memset(s, 0, sizeof(*s));
    if (mi_copy(s->merchant, sizeof(s->merchant), merchant) != 0 ||
        mi_copy(s->kbn, sizeof(s->kbn), kbn) != 0) {
        return NULL;
    }
    return s;
}

static int mi_load_kbn(mi_kbn_vec_t *out)
{
    FILE *fp;
    char line[MI_MAX_LINE];
    int row = 0;

    fp = fopen(MI_INPUT_KBN, "r");
    if (fp == NULL) {
        fprintf(stderr, "区分ファイルを開けません:%s\n", MI_INPUT_KBN);
        return MI_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[MI_MAX_FIELD];
        int nf;
        mi_kbn_t rec;
        int32_t fee;

        row++;
        mi_chomp(line);
        if (row == 1 && strstr(line, "SETTLE-KBN") != NULL) {
            continue;
        }
        nf = mi_split_csv(line, f, MI_MAX_FIELD);
        if (nf != 6) {
            fprintf(stderr, "区分ファイル項目数不正:%d\n", row);
            fclose(fp);
            return MI_ERR_PARSE;
        }
        memset(&rec, 0, sizeof(rec));
        if (mi_copy(rec.kbn, sizeof(rec.kbn), f[0]) != 0 ||
            mi_copy(rec.name, sizeof(rec.name), f[1]) != 0 ||
            mi_copy(rec.valid_from, sizeof(rec.valid_from), f[4]) != 0 ||
            mi_copy(rec.valid_to, sizeof(rec.valid_to), f[5]) != 0 ||
            mi_parse_i32(f[3], &fee) != 0 ||
            (strcmp(f[2], "Y") != 0 && strcmp(f[2], "N") != 0)) {
            fprintf(stderr, "区分ファイル内容不正:%d\n", row);
            fclose(fp);
            return MI_ERR_PARSE;
        }
        rec.nettable = strcmp(f[2], "Y") == 0;
        rec.fee_ppm = fee;
        if (rec.fee_ppm < 0 || strcmp(rec.valid_from, rec.valid_to) > 0) {
            fprintf(stderr, "区分ファイル範囲不正:%d\n", row);
            fclose(fp);
            return MI_ERR_PARSE;
        }
        if (mi_reserve_kbn(out) != 0) {
            fclose(fp);
            return MI_ERR_MEMORY;
        }
        out->items[out->len++] = rec;
    }

    if (ferror(fp)) {
        fprintf(stderr, "区分ファイル読込失敗\n");
        fclose(fp);
        return MI_ERR_IO;
    }
    fclose(fp);
    return MI_OK;
}

static int mi_load_carry(mi_carry_vec_t *out)
{
    FILE *fp;
    char line[MI_MAX_LINE];
    int row = 0;

    fp = fopen(MI_INPUT_CARRY, "r");
    if (fp == NULL) {
        fprintf(stderr, "繰越ファイルを開けません:%s\n", MI_INPUT_CARRY);
        return MI_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[MI_MAX_FIELD];
        int nf;
        int64_t amount;
        mi_carry_t rec;

        row++;
        mi_chomp(line);
        if (row == 1 && strstr(line, "CARRY-ID") != NULL) {
            continue;
        }
        nf = mi_split_csv(line, f, MI_MAX_FIELD);
        if (nf != 6 || mi_parse_i64(f[3], &amount) != 0) {
            fprintf(stderr, "繰越ファイル内容不正:%d\n", row);
            fclose(fp);
            return MI_ERR_PARSE;
        }
        memset(&rec, 0, sizeof(rec));
        if (mi_copy(rec.merchant, sizeof(rec.merchant), f[1]) != 0 ||
            mi_copy(rec.kbn, sizeof(rec.kbn), f[2]) != 0 ||
            mi_copy(rec.reason, sizeof(rec.reason), f[4]) != 0 ||
            mi_copy(rec.next_settle_date, sizeof(rec.next_settle_date), f[5]) != 0) {
            fprintf(stderr, "繰越ファイル桁数不正:%d\n", row);
            fclose(fp);
            return MI_ERR_PARSE;
        }
        rec.amount = amount;
        if (mi_reserve_carry(out) != 0) {
            fclose(fp);
            return MI_ERR_MEMORY;
        }
        out->items[out->len++] = rec;
    }

    if (ferror(fp)) {
        fprintf(stderr, "繰越ファイル読込失敗\n");
        fclose(fp);
        return MI_ERR_IO;
    }
    fclose(fp);
    return MI_OK;
}

static int mi_apply_carry(const mi_carry_vec_t *carry, mi_summary_vec_t *sum)
{
    size_t i;

    for (i = 0; i < carry->len; i++) {
        mi_summary_t *s;
        int64_t next;

        if (strcmp(carry->items[i].next_settle_date, MI_SETTLE_DATE) != 0) {
            continue;
        }
        s = mi_get_summary(sum, carry->items[i].merchant, carry->items[i].kbn);
        if (s == NULL) {
            return MI_ERR_MEMORY;
        }
        if (mi_add_i64(s->carry_amount, carry->items[i].amount, &next) != 0) {
            return MI_ERR_OVERFLOW;
        }
        s->carry_amount = next;
        if (s->carry_reason[0] == '\0' &&
            mi_copy(s->carry_reason, sizeof(s->carry_reason), carry->items[i].reason) != 0) {
            return MI_ERR_PARSE;
        }
    }
    return MI_OK;
}

static int mi_load_detail(const mi_kbn_vec_t *kbn, mi_summary_vec_t *sum)
{
    FILE *fp;
    char line[MI_MAX_LINE];
    int row = 0;

    fp = fopen(MI_INPUT_DETAIL, "r");
    if (fp == NULL) {
        fprintf(stderr, "明細ファイルを開けません:%s\n", MI_INPUT_DETAIL);
        return MI_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[MI_MAX_FIELD];
        int nf;
        int64_t amount;
        const mi_kbn_t *kr;
        mi_summary_t *s;
        int64_t next;

        row++;
        mi_chomp(line);
        if (row == 1 && strstr(line, "DETAIL-ID") != NULL) {
            continue;
        }
        nf = mi_split_csv(line, f, MI_MAX_FIELD);
        if (nf != 6 || mi_parse_i64(f[3], &amount) != 0) {
            fprintf(stderr, "明細ファイル内容不正:%d\n", row);
            fclose(fp);
            return MI_ERR_PARSE;
        }
        if (strcmp(f[5], "0") != 0 && strcmp(f[5], "OK") != 0) {
            continue;
        }
        kr = mi_find_kbn(kbn, f[4]);
        s = mi_get_summary(sum, f[2], f[4]);
        if (s == NULL) {
            fclose(fp);
            return MI_ERR_MEMORY;
        }
        if (kr == NULL || strcmp(MI_SETTLE_DATE, kr->valid_from) < 0 ||
            strcmp(MI_SETTLE_DATE, kr->valid_to) > 0) {
            s->blocked = 1;
            if (s->carry_reason[0] == '\0' &&
                mi_copy(s->carry_reason, sizeof(s->carry_reason), "区分無効") != 0) {
                fclose(fp);
                return MI_ERR_PARSE;
            }
            continue;
        }
        s->nettable = kr->nettable;
        if (!kr->nettable) {
            s->blocked = 1;
            if (s->carry_reason[0] == '\0' &&
                mi_copy(s->carry_reason, sizeof(s->carry_reason), "相殺不可") != 0) {
                fclose(fp);
                return MI_ERR_PARSE;
            }
        }
        if (mi_add_i64(s->total_amount, amount, &next) != 0) {
            fclose(fp);
            return MI_ERR_OVERFLOW;
        }
        s->total_amount = next;
        if (mi_add_i64(s->txn_count, 1, &next) != 0) {
            fclose(fp);
            return MI_ERR_OVERFLOW;
        }
        s->txn_count = next;
    }

    if (ferror(fp)) {
        fprintf(stderr, "明細ファイル読込失敗\n");
        fclose(fp);
        return MI_ERR_IO;
    }
    fclose(fp);
    return MI_OK;
}

static int mi_write_summary(const mi_summary_vec_t *sum)
{
    FILE *fp;
    size_t i;

    fp = fopen(MI_OUTPUT_SUMMARY, "w");
    if (fp == NULL) {
        fprintf(stderr, "サマリファイルを作成できません:%s\n", MI_OUTPUT_SUMMARY);
        return MI_ERR_IO;
    }

    if (fprintf(fp, "MERCHANT-CODE,SETTLE-DATE,SETTLE-KBN,TXN-COUNT,TOTAL-AMT,CARRY-AMT\n") < 0) {
        fclose(fp);
        return MI_ERR_IO;
    }

    for (i = 0; i < sum->len; i++) {
        const mi_summary_t *s = &sum->items[i];
        int64_t carry_out = s->carry_amount;

        if (s->blocked && s->carry_amount == 0) {
            carry_out = s->total_amount;
        }
        if (fprintf(fp, "%s,%s,%s,%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
                    s->merchant,
                    MI_SETTLE_DATE,
                    s->kbn,
                    s->txn_count,
                    s->total_amount,
                    carry_out) < 0) {
            fclose(fp);
            return MI_ERR_IO;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "サマリファイル確定失敗\n");
        return MI_ERR_IO;
    }
    return MI_OK;
}

int main(void)
{
    mi_kbn_vec_t kbn;
    mi_carry_vec_t carry;
    mi_summary_vec_t sum;
    int rc;

    memset(&kbn, 0, sizeof(kbn));
    memset(&carry, 0, sizeof(carry));
    memset(&sum, 0, sizeof(sum));

    rc = mi_load_kbn(&kbn);
    if (rc == MI_OK) {
        rc = mi_load_carry(&carry);
    }
    if (rc == MI_OK) {
        rc = mi_apply_carry(&carry, &sum);
    }
    if (rc == MI_OK) {
        rc = mi_load_detail(&kbn, &sum);
    }
    if (rc == MI_OK) {
        rc = mi_write_summary(&sum);
    }

    free(sum.items);
    free(carry.items);
    free(kbn.items);

    if (rc != MI_OK) {
        return rc;
    }
    return MI_OK;
}
