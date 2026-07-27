/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20210715  市場基盤部  初版作成、判定結果のHFDEC追記処理を実装
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_DEC_ACCEPT          0
#define MIHFT_DEC_REJECT_MARGIN   4
#define MIHFT_DEC_REJECT_NOTIONAL 8
#define MIHFT_DEC_REJECT_TICK     12

#define MIHFT_SCORDF_PATH "SCORDF.csv"
#define MIHFT_HFDEC_PATH  "HFDEC.csv"

#define MIHFT_LINE_MAX 1024
#define MIHFT_FIELD_MAX 64
#define MIHFT_KEY_MAX 32
#define MIHFT_REASON_MAX 16
#define MIHFT_CIF_BUCKETS 257

typedef struct {
    char order_id[MIHFT_KEY_MAX];
    char cif_no[MIHFT_KEY_MAX];
    char instr_code[MIHFT_KEY_MAX];
    char side_kbn[2];
    char ord_type[2];
    char tif_code[4];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} scordf_record_t;

typedef struct cif_limit_node {
    char cif_no[MIHFT_KEY_MAX];
    int64_t used_amt;
    struct cif_limit_node *next;
} cif_limit_node_t;

static cif_limit_node_t *g_cif_limits[MIHFT_CIF_BUCKETS];

static unsigned long hash_cif(const char *s)
{
    unsigned long h = 5381U;

    while (*s != '\0') {
        h = ((h << 5) + h) ^ (unsigned char)*s;
        ++s;
    }
    return h % MIHFT_CIF_BUCKETS;
}

static void trim_field(char *s)
{
    char *p = s;
    char *end;

    while (isspace((unsigned char)*p)) {
        ++p;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1U);
    }

    end = s + strlen(s);
    while (end > s && isspace((unsigned char)end[-1])) {
        --end;
    }
    *end = '\0';
}

static int next_field(char **cursor, char *out, size_t out_sz)
{
    char *src = *cursor;
    size_t n = 0U;
    int quoted = 0;

    if (out_sz == 0U || src == NULL || *src == '\0') {
        return -1;
    }

    if (*src == '"') {
        quoted = 1;
        ++src;
    }

    while (*src != '\0') {
        if (quoted != 0) {
            if (*src == '"' && src[1] == '"') {
                if (n + 1U >= out_sz) {
                    return -1;
                }
                out[n++] = '"';
                src += 2;
                continue;
            }
            if (*src == '"') {
                ++src;
                if (*src == ',') {
                    ++src;
                }
                break;
            }
        } else if (*src == ',') {
            ++src;
            break;
        } else if (*src == '\n' || *src == '\r') {
            break;
        }

        if (n + 1U >= out_sz) {
            return -1;
        }
        out[n++] = *src++;
    }

    out[n] = '\0';
    trim_field(out);
    *cursor = src;
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s) {
        return -1;
    }
    while (*end != '\0') {
        if (!isspace((unsigned char)*end)) {
            return -1;
        }
        ++end;
    }
    if (v < 0) {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int parse_int(const char *s, int *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v > INT_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int checked_mul_i64(int64_t a, int64_t b, int64_t *out)
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

static int checked_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if (b > 0 && a > INT64_MAX - b) {
        return -1;
    }
    if (b < 0 && a < INT64_MIN - b) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int tier_rate_bp(int tier, int *rate_bp)
{
    switch (tier) {
    case 1:
        *rate_bp = 1000;
        return 0;
    case 2:
        *rate_bp = 2000;
        return 0;
    case 3:
        *rate_bp = 4000;
        return 0;
    default:
        return -1;
    }
}

static int tier_tick(int tier, int64_t *tick)
{
    switch (tier) {
    case 1:
        *tick = 100;
        return 0;
    case 2:
        *tick = 500;
        return 0;
    case 3:
        *tick = 1000;
        return 0;
    default:
        return -1;
    }
}

static int validate_codes(const scordf_record_t *r)
{
    if (!(strcmp(r->side_kbn, "B") == 0 || strcmp(r->side_kbn, "S") == 0)) {
        return -1;
    }
    if (!(strcmp(r->ord_type, "L") == 0 || strcmp(r->ord_type, "M") == 0)) {
        return -1;
    }
    if (!(strcmp(r->tif_code, "DAY") == 0 ||
          strcmp(r->tif_code, "IOC") == 0 ||
          strcmp(r->tif_code, "FOK") == 0)) {
        return -1;
    }
    if (r->ord_qty <= 0 || r->price_amt <= 0) {
        return -1;
    }
    return 0;
}

static int parse_scordf_line(char *line, scordf_record_t *r)
{
    char *cur = line;
    char qty[MIHFT_FIELD_MAX];
    char price[MIHFT_FIELD_MAX];
    char tier[MIHFT_FIELD_MAX];

    if (next_field(&cur, r->order_id, sizeof(r->order_id)) != 0 ||
        next_field(&cur, r->cif_no, sizeof(r->cif_no)) != 0 ||
        next_field(&cur, r->instr_code, sizeof(r->instr_code)) != 0 ||
        next_field(&cur, r->side_kbn, sizeof(r->side_kbn)) != 0 ||
        next_field(&cur, r->ord_type, sizeof(r->ord_type)) != 0 ||
        next_field(&cur, r->tif_code, sizeof(r->tif_code)) != 0 ||
        next_field(&cur, qty, sizeof(qty)) != 0 ||
        next_field(&cur, price, sizeof(price)) != 0 ||
        next_field(&cur, tier, sizeof(tier)) != 0) {
        return -1;
    }

    if (parse_i64(qty, &r->ord_qty) != 0 ||
        parse_i64(price, &r->price_amt) != 0 ||
        parse_int(tier, &r->instr_tier) != 0) {
        return -1;
    }

    return validate_codes(r);
}

static cif_limit_node_t *get_cif_node(const char *cif_no, int create)
{
    unsigned long bucket = hash_cif(cif_no);
    cif_limit_node_t *node = g_cif_limits[bucket];

    while (node != NULL) {
        if (strcmp(node->cif_no, cif_no) == 0) {
            return node;
        }
        node = node->next;
    }

    if (create == 0) {
        return NULL;
    }

    node = (cif_limit_node_t *)calloc(1U, sizeof(*node));
    if (node == NULL) {
        return NULL;
    }

    snprintf(node->cif_no, sizeof(node->cif_no), "%s", cif_no);
    node->next = g_cif_limits[bucket];
    g_cif_limits[bucket] = node;
    return node;
}

static void free_cif_limits(void)
{
    size_t i;

    for (i = 0U; i < MIHFT_CIF_BUCKETS; ++i) {
        cif_limit_node_t *node = g_cif_limits[i];
        while (node != NULL) {
            cif_limit_node_t *next = node->next;
            free(node);
            node = next;
        }
        g_cif_limits[i] = NULL;
    }
}

static int read_hfdec_state(int64_t *max_decision_id)
{
    FILE *fp = fopen(MIHFT_HFDEC_PATH, "r");
    char line[MIHFT_LINE_MAX];

    *max_decision_id = 0;

    if (fp == NULL) {
        if (errno == ENOENT) {
            return 0;
        }
        fprintf(stderr, "HFDEC入力オープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cur = line;
        char decision_id_s[MIHFT_FIELD_MAX];
        char order_id[MIHFT_FIELD_MAX];
        char cif_no[MIHFT_KEY_MAX];
        char instr_code[MIHFT_FIELD_MAX];
        char decision_cd_s[MIHFT_FIELD_MAX];
        char reason_cd[MIHFT_FIELD_MAX];
        char notional_s[MIHFT_FIELD_MAX];
        char limit_used_s[MIHFT_FIELD_MAX];
        char decision_ts[MIHFT_FIELD_MAX];
        int64_t decision_id;
        int64_t limit_used;
        int decision_cd;

        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0') {
            continue;
        }

        if (next_field(&cur, decision_id_s, sizeof(decision_id_s)) != 0 ||
            next_field(&cur, order_id, sizeof(order_id)) != 0 ||
            next_field(&cur, cif_no, sizeof(cif_no)) != 0 ||
            next_field(&cur, instr_code, sizeof(instr_code)) != 0 ||
            next_field(&cur, decision_cd_s, sizeof(decision_cd_s)) != 0 ||
            next_field(&cur, reason_cd, sizeof(reason_cd)) != 0 ||
            next_field(&cur, notional_s, sizeof(notional_s)) != 0 ||
            next_field(&cur, limit_used_s, sizeof(limit_used_s)) != 0 ||
            next_field(&cur, decision_ts, sizeof(decision_ts)) != 0) {
            fclose(fp);
            fprintf(stderr, "HFDEC入力形式不正\n");
            return -1;
        }

        if (parse_i64(decision_id_s, &decision_id) != 0 ||
            parse_int(decision_cd_s, &decision_cd) != 0 ||
            parse_i64(limit_used_s, &limit_used) != 0) {
            fclose(fp);
            fprintf(stderr, "HFDEC数値形式不正\n");
            return -1;
        }

        if (decision_id > *max_decision_id) {
            *max_decision_id = decision_id;
        }

        if (decision_cd == MIHFT_DEC_ACCEPT) {
            cif_limit_node_t *node = get_cif_node(cif_no, 1);
            if (node == NULL) {
                fclose(fp);
                fprintf(stderr, "与信表領域不足\n");
                return -1;
            }
            if (limit_used > node->used_amt) {
                node->used_amt = limit_used;
            }
        }
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "HFDEC入力読込失敗\n");
        return -1;
    }

    fclose(fp);
    return 0;
}

static void make_timestamp(char *out, size_t out_sz)
{
    time_t now = time(NULL);
    struct tm tmv;

    if (now == (time_t)-1) {
        snprintf(out, out_sz, "19700101000000");
        return;
    }

#if defined(_POSIX_VERSION)
    if (localtime_r(&now, &tmv) == NULL) {
        snprintf(out, out_sz, "19700101000000");
        return;
    }
#else
    {
        struct tm *tmp = localtime(&now);
        if (tmp == NULL) {
            snprintf(out, out_sz, "19700101000000");
            return;
        }
        tmv = *tmp;
    }
#endif

    strftime(out, out_sz, "%Y%m%d%H%M%S", &tmv);
}

static int decide_record(const scordf_record_t *r, int64_t used_before,
                         int64_t *notional, int64_t *limit_after,
                         char *reason, size_t reason_sz)
{
    int rate_bp;
    int64_t margin_base;
    int64_t required_margin;
    int64_t tick;

    if (tier_rate_bp(r->instr_tier, &rate_bp) != 0 ||
        tier_tick(r->instr_tier, &tick) != 0) {
        snprintf(reason, reason_sz, "TIER");
        *notional = 0;
        *limit_after = used_before;
        return MIHFT_DEC_REJECT_TICK;
    }

    if (checked_mul_i64(r->ord_qty, r->price_amt, notional) != 0) {
        snprintf(reason, reason_sz, "OVF");
        *notional = INT64_MAX;
        *limit_after = used_before;
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    if (strcmp(r->ord_type, "L") == 0 && (r->price_amt % tick) != 0) {
        snprintf(reason, reason_sz, "TICK");
        *limit_after = used_before;
        return MIHFT_DEC_REJECT_TICK;
    }

    if (*notional > (int64_t)MIHFT_MAX_NOTIONAL) {
        snprintf(reason, reason_sz, "NOTIONAL");
        *limit_after = used_before;
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    if (checked_mul_i64(*notional, (int64_t)rate_bp, &margin_base) != 0) {
        snprintf(reason, reason_sz, "OVF");
        *limit_after = used_before;
        return MIHFT_DEC_REJECT_MARGIN;
    }

    required_margin = (margin_base + 9999) / 10000;
    if (checked_add_i64(used_before, required_margin, limit_after) != 0 ||
        *limit_after > (int64_t)MIHFT_MAX_NOTIONAL) {
        snprintf(reason, reason_sz, "MARGIN");
        *limit_after = used_before;
        return MIHFT_DEC_REJECT_MARGIN;
    }

    snprintf(reason, reason_sz, "OK");
    return MIHFT_DEC_ACCEPT;
}

int main(void)
{
    FILE *in = NULL;
    FILE *out = NULL;
    char line[MIHFT_LINE_MAX];
    int64_t decision_id;
    int final_code = MIHFT_DEC_ACCEPT;

    if (read_hfdec_state(&decision_id) != 0) {
        free_cif_limits();
        return 2;
    }

    in = fopen(MIHFT_SCORDF_PATH, "r");
    if (in == NULL) {
        fprintf(stderr, "SCORDF入力オープン失敗\n");
        free_cif_limits();
        return 2;
    }

    out = fopen(MIHFT_HFDEC_PATH, "a");
    if (out == NULL) {
        fprintf(stderr, "HFDEC出力オープン失敗\n");
        fclose(in);
        free_cif_limits();
        return 2;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        scordf_record_t rec;
        cif_limit_node_t *node;
        int64_t notional;
        int64_t limit_after;
        char reason[MIHFT_REASON_MAX];
        char ts[32];
        int decision_cd;

        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0') {
            continue;
        }

        memset(&rec, 0, sizeof(rec));
        if (parse_scordf_line(line, &rec) != 0) {
            fprintf(stderr, "SCORDF入力形式不正\n");
            fclose(out);
            fclose(in);
            free_cif_limits();
            return 2;
        }

        node = get_cif_node(rec.cif_no, 1);
        if (node == NULL) {
            fprintf(stderr, "与信表領域不足\n");
            fclose(out);
            fclose(in);
            free_cif_limits();
            return 2;
        }

        decision_cd = decide_record(&rec, node->used_amt, &notional,
                                    &limit_after, reason, sizeof(reason));
        if (decision_cd == MIHFT_DEC_ACCEPT) {
            node->used_amt = limit_after;
        }

        if (checked_add_i64(decision_id, 1, &decision_id) != 0) {
            fprintf(stderr, "採番上限超過\n");
            fclose(out);
            fclose(in);
            free_cif_limits();
            return 2;
        }

        make_timestamp(ts, sizeof(ts));
        if (fprintf(out, "%lld,%s,%s,%s,%d,%s,%lld,%lld,%s\n",
                    (long long)decision_id,
                    rec.order_id,
                    rec.cif_no,
                    rec.instr_code,
                    decision_cd,
                    reason,
                    (long long)notional,
                    (long long)limit_after,
                    ts) < 0) {
            fprintf(stderr, "HFDEC出力書込失敗\n");
            fclose(out);
            fclose(in);
            free_cif_limits();
            return 2;
        }

        if (decision_cd != MIHFT_DEC_ACCEPT && final_code == MIHFT_DEC_ACCEPT) {
            final_code = decision_cd;
        }
    }

    if (ferror(in)) {
        fprintf(stderr, "SCORDF入力読込失敗\n");
        fclose(out);
        fclose(in);
        free_cif_limits();
        return 2;
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "HFDEC出力クローズ失敗\n");
        fclose(in);
        free_cif_limits();
        return 2;
    }

    fclose(in);
    free_cif_limits();
    return final_code;
}
