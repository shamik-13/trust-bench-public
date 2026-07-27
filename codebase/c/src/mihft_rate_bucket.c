/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210715  大野 修 (E-225)   レートバケット更新の初版作成
 * 1.01  20211215  大野 修 (E-225)   既存スロットル判定との突合と厳格側採用を追加
 */

#include "mihft_types.h"

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

#define MIHFT_MAX_LINE       1024
#define MIHFT_MAX_FIELD      16
#define MIHFT_KEY_LEN        96
#define MIHFT_TS_LEN         20
#define MIHFT_REJECT_ID_LEN  48
#define MIHFT_REJECT_CD_LEN  16
#define MIHFT_DETAIL_CD_LEN  24

typedef struct {
    char order_id[32];
    char cif_no[24];
    char instr_code[24];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    uint64_t ord_qty;
    uint64_t price_amt;
    int instr_tier;
} scordf_rec_t;

typedef struct {
    char bucket_key[MIHFT_KEY_LEN];
    char window_ts[MIHFT_TS_LEN];
    uint64_t order_cnt;
    uint64_t notional_amt;
    uint64_t drop_cnt;
    int used;
} hfrate_rec_t;

typedef struct {
    char key[MIHFT_KEY_LEN];
    size_t index;
    int found;
} bucket_pos_t;

static int jp_str_eq(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

static void trim_eol(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int split_csv(char *line, char *field[], size_t max_field, size_t *count)
{
    size_t n = 0;
    char *p = line;

    while (n < max_field) {
        field[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            *count = n;
            return 1;
        }
        *p++ = '\0';
    }

    return strchr(p, ',') == NULL ? (*count = n, 1) : 0;
}

static int parse_u64(const char *s, uint64_t *out)
{
    char *end = NULL;
    unsigned long long v;

    if (s == NULL || *s == '\0' || *s == '-') {
        return 0;
    }

    errno = 0;
    v = strtoull(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return 0;
    }

    *out = (uint64_t)v;
    return 1;
}

static int parse_i32(const char *s, int *out)
{
    char *end = NULL;
    long v;

    if (s == NULL || *s == '\0') {
        return 0;
    }

    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0' || v < INT_MIN || v > INT_MAX) {
        return 0;
    }

    *out = (int)v;
    return 1;
}

static int checked_mul_u64(uint64_t a, uint64_t b, uint64_t *out)
{
    if (a != 0 && b > UINT64_MAX / a) {
        return 0;
    }
    *out = a * b;
    return 1;
}

static int checked_add_u64(uint64_t a, uint64_t b, uint64_t *out)
{
    if (b > UINT64_MAX - a) {
        return 0;
    }
    *out = a + b;
    return 1;
}

static int tier_rate_bp(int tier, uint64_t *rate_bp, uint64_t *tick)
{
    if (tier == 1) {
        *rate_bp = 1000;
        *tick = 100;
        return 1;
    }
    if (tier == 2) {
        *rate_bp = 2000;
        *tick = 500;
        return 1;
    }
    if (tier == 3) {
        *rate_bp = 4000;
        *tick = 1000;
        return 1;
    }
    return 0;
}

static uint64_t tier_order_limit(int tier)
{
    if (tier == 1) {
        return 1200;
    }
    if (tier == 2) {
        return 600;
    }
    return 240;
}

static uint64_t tier_notional_limit(int tier)
{
    if (tier == 1) {
        return MIHFT_MAX_NOTIONAL;
    }
    if (tier == 2) {
        return MIHFT_MAX_NOTIONAL / 2U;
    }
    return MIHFT_MAX_NOTIONAL / 5U;
}

static int make_bucket_key(const scordf_rec_t *o, char *dst, size_t dst_len)
{
    int n = snprintf(dst, dst_len, "%s|%s|%c", o->cif_no, o->instr_code, o->ord_type);
    return n > 0 && (size_t)n < dst_len;
}

static int parse_scordf(char *line, scordf_rec_t *o)
{
    char *f[MIHFT_MAX_FIELD];
    size_t c = 0;

    trim_eol(line);
    if (!split_csv(line, f, MIHFT_MAX_FIELD, &c) || c != 9) {
        return 0;
    }

    if (strlen(f[0]) >= sizeof(o->order_id) ||
        strlen(f[1]) >= sizeof(o->cif_no) ||
        strlen(f[2]) >= sizeof(o->instr_code) ||
        strlen(f[5]) >= sizeof(o->tif_code)) {
        return 0;
    }

    if (strlen(f[3]) != 1 || strlen(f[4]) != 1) {
        return 0;
    }

    strcpy(o->order_id, f[0]);
    strcpy(o->cif_no, f[1]);
    strcpy(o->instr_code, f[2]);
    o->side_kbn = f[3][0];
    o->ord_type = f[4][0];
    strcpy(o->tif_code, f[5]);

    if ((o->side_kbn != 'B' && o->side_kbn != 'S') ||
        (o->ord_type != 'L' && o->ord_type != 'M') ||
        (!jp_str_eq(o->tif_code, "DAY") && !jp_str_eq(o->tif_code, "IOC") && !jp_str_eq(o->tif_code, "FOK"))) {
        return 0;
    }

    return parse_u64(f[6], &o->ord_qty) &&
           parse_u64(f[7], &o->price_amt) &&
           parse_i32(f[8], &o->instr_tier);
}

static int parse_hfrate(char *line, hfrate_rec_t *r)
{
    char *f[MIHFT_MAX_FIELD];
    size_t c = 0;

    trim_eol(line);
    if (!split_csv(line, f, MIHFT_MAX_FIELD, &c) || c != 5) {
        return 0;
    }

    if (strlen(f[0]) >= sizeof(r->bucket_key) || strlen(f[1]) >= sizeof(r->window_ts)) {
        return 0;
    }

    strcpy(r->bucket_key, f[0]);
    strcpy(r->window_ts, f[1]);

    r->used = 1;
    return parse_u64(f[2], &r->order_cnt) &&
           parse_u64(f[3], &r->notional_amt) &&
           parse_u64(f[4], &r->drop_cnt);
}

static int load_hfrate(const char *path, hfrate_rec_t **rows, size_t *len, size_t *cap)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];

    *rows = NULL;
    *len = 0;
    *cap = 0;

    if (fp == NULL) {
        return errno == ENOENT ? 1 : 0;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        hfrate_rec_t rec;
        hfrate_rec_t *tmp;

        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fclose(fp);
            return 0;
        }

        if (!parse_hfrate(line, &rec)) {
            fclose(fp);
            return 0;
        }

        if (*len == *cap) {
            size_t next_cap = *cap == 0 ? 128U : *cap * 2U;
            if (next_cap < *cap) {
                fclose(fp);
                return 0;
            }
            tmp = (hfrate_rec_t *)realloc(*rows, next_cap * sizeof(**rows));
            if (tmp == NULL) {
                fclose(fp);
                return 0;
            }
            *rows = tmp;
            *cap = next_cap;
        }

        (*rows)[(*len)++] = rec;
    }

    if (ferror(fp)) {
        fclose(fp);
        return 0;
    }

    return fclose(fp) == 0;
}

static bucket_pos_t find_bucket(hfrate_rec_t *rows, size_t len, const char *key, const char *window_ts)
{
    size_t i;
    bucket_pos_t pos;

    pos.key[0] = '\0';
    pos.index = 0;
    pos.found = 0;

    for (i = 0; i < len; i++) {
        if (rows[i].used &&
            jp_str_eq(rows[i].bucket_key, key) &&
            jp_str_eq(rows[i].window_ts, window_ts)) {
            pos.index = i;
            pos.found = 1;
            return pos;
        }
    }

    return pos;
}

static int append_bucket(hfrate_rec_t **rows, size_t *len, size_t *cap, const char *key, const char *window_ts, size_t *index)
{
    hfrate_rec_t *tmp;

    if (*len == *cap) {
        size_t next_cap = *cap == 0 ? 128U : *cap * 2U;
        if (next_cap < *cap) {
            return 0;
        }
        tmp = (hfrate_rec_t *)realloc(*rows, next_cap * sizeof(**rows));
        if (tmp == NULL) {
            return 0;
        }
        *rows = tmp;
        *cap = next_cap;
    }

    memset(&(*rows)[*len], 0, sizeof((*rows)[*len]));
    strcpy((*rows)[*len].bucket_key, key);
    strcpy((*rows)[*len].window_ts, window_ts);
    (*rows)[*len].used = 1;
    *index = (*len)++;
    return 1;
}

static int existing_throttle_decision(const scordf_rec_t *o, uint64_t notional)
{
    uint64_t rate_bp;
    uint64_t tick;
    uint64_t required_margin;

    if (!tier_rate_bp(o->instr_tier, &rate_bp, &tick)) {
        return MIHFT_DEC_REJECT_MARGIN;
    }

    if (o->price_amt == 0 || (o->ord_type == 'L' && o->price_amt % tick != 0)) {
        return MIHFT_DEC_REJECT_TICK;
    }

    if (notional > MIHFT_MAX_NOTIONAL) {
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    if (!checked_mul_u64(notional, rate_bp, &required_margin)) {
        return MIHFT_DEC_REJECT_MARGIN;
    }

    required_margin = (required_margin + 9999U) / 10000U;
    if (required_margin > MIHFT_MAX_NOTIONAL / 10U) {
        return MIHFT_DEC_REJECT_MARGIN;
    }

    return MIHFT_DEC_ACCEPT;
}

static int rate_bucket_decision(const scordf_rec_t *o, const hfrate_rec_t *b, uint64_t notional)
{
    uint64_t next_cnt;
    uint64_t next_notional;

    if (!checked_add_u64(b->order_cnt, 1U, &next_cnt) ||
        !checked_add_u64(b->notional_amt, notional, &next_notional)) {
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    if (next_cnt > tier_order_limit(o->instr_tier)) {
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    if (next_notional > tier_notional_limit(o->instr_tier)) {
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    return MIHFT_DEC_ACCEPT;
}

static int stricter_decision(int a, int b)
{
    return a > b ? a : b;
}

static const char *reject_cd(int decision)
{
    if (decision == MIHFT_DEC_REJECT_MARGIN) {
        return "RJ-MARGIN";
    }
    if (decision == MIHFT_DEC_REJECT_NOTIONAL) {
        return "RJ-NOTIONAL";
    }
    if (decision == MIHFT_DEC_REJECT_TICK) {
        return "RJ-TICK";
    }
    return "ACCEPT";
}

static const char *detail_cd(int throttle_decision, int rate_decision)
{
    if (throttle_decision != rate_decision) {
        return "STRICTER-APPLIED";
    }
    if (rate_decision == MIHFT_DEC_REJECT_NOTIONAL) {
        return "RATE-LIMIT";
    }
    if (throttle_decision == MIHFT_DEC_REJECT_MARGIN) {
        return "MARGIN-LIMIT";
    }
    if (throttle_decision == MIHFT_DEC_REJECT_TICK) {
        return "TICK-LIMIT";
    }
    return "OK";
}

static void make_reject_id(char *dst, size_t dst_len, uint64_t seq)
{
    (void)snprintf(dst, dst_len, "HFRJCT%014llu", (unsigned long long)seq);
}

static int write_reject(FILE *fp, uint64_t seq, const scordf_rec_t *o, int throttle_decision, int rate_decision, const char *ts)
{
    char reject_id[MIHFT_REJECT_ID_LEN];

    make_reject_id(reject_id, sizeof(reject_id), seq);

    return fprintf(fp, "%s,%s,%s,%s,%s,%s,%s\n",
                   reject_id,
                   o->order_id,
                   o->cif_no,
                   o->instr_code,
                   reject_cd(stricter_decision(throttle_decision, rate_decision)),
                   detail_cd(throttle_decision, rate_decision),
                   ts) > 0;
}

static int write_hfrate(const char *path, const hfrate_rec_t *rows, size_t len)
{
    FILE *fp = fopen(path, "w");
    size_t i;

    if (fp == NULL) {
        return 0;
    }

    for (i = 0; i < len; i++) {
        if (!rows[i].used) {
            continue;
        }

        if (fprintf(fp, "%s,%s,%llu,%llu,%llu\n",
                    rows[i].bucket_key,
                    rows[i].window_ts,
                    (unsigned long long)rows[i].order_cnt,
                    (unsigned long long)rows[i].notional_amt,
                    (unsigned long long)rows[i].drop_cnt) < 0) {
            fclose(fp);
            return 0;
        }
    }

    return fclose(fp) == 0;
}

static void current_ts(char *dst, size_t dst_len)
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_POSIX_THREAD_SAFE_FUNCTIONS) || defined(__APPLE__)
    if (localtime_r(&now, &tmv) == NULL) {
        memset(&tmv, 0, sizeof(tmv));
        tmv.tm_year = 126;
        tmv.tm_mon = 5;
        tmv.tm_mday = 26;
    }
#else
    {
        struct tm *p = localtime(&now);
        if (p != NULL) {
            tmv = *p;
        } else {
            memset(&tmv, 0, sizeof(tmv));
            tmv.tm_year = 126;
            tmv.tm_mon = 5;
            tmv.tm_mday = 26;
        }
    }
#endif

    (void)strftime(dst, dst_len, "%Y%m%d%H%M%S", &tmv);
}

static const char *env_or_default(const char *name, const char *fallback)
{
    const char *v = getenv(name);
    return v != NULL && *v != '\0' ? v : fallback;
}

int main(void)
{
    const char *scordf_path = env_or_default("MIHFT_SCORDF", "SCORDF.csv");
    const char *hfrate_in_path = env_or_default("MIHFT_HFRATE_IN", "HFRATE.csv");
    const char *hfrate_out_path = env_or_default("MIHFT_HFRATE_OUT", "HFRATE.out.csv");
    const char *hfrjct_path = env_or_default("MIHFT_HFRJCT", "HFRJCT.csv");
    const char *window_env = getenv("MIHFT_WINDOW_TS");

    char window_ts[MIHFT_TS_LEN];
    char reject_ts[MIHFT_TS_LEN];
    char line[MIHFT_MAX_LINE];

    hfrate_rec_t *buckets = NULL;
    size_t bucket_len = 0;
    size_t bucket_cap = 0;

    FILE *scordf = NULL;
    FILE *hfrjct = NULL;

    uint64_t reject_seq = 1;
    int batch_decision = MIHFT_DEC_ACCEPT;

    if (window_env != NULL && *window_env != '\0') {
        if (strlen(window_env) >= sizeof(window_ts)) {
            fprintf(stderr, "ウィンドウ時刻が長すぎます\n");
            return 2;
        }
        strcpy(window_ts, window_env);
    } else {
        current_ts(window_ts, sizeof(window_ts));
    }
    current_ts(reject_ts, sizeof(reject_ts));

    if (!load_hfrate(hfrate_in_path, &buckets, &bucket_len, &bucket_cap)) {
        fprintf(stderr, "HFRATE読込失敗\n");
        free(buckets);
        return 2;
    }

    scordf = fopen(scordf_path, "r");
    if (scordf == NULL) {
        fprintf(stderr, "SCORDF読込失敗\n");
        free(buckets);
        return 2;
    }

    hfrjct = fopen(hfrjct_path, "w");
    if (hfrjct == NULL) {
        fprintf(stderr, "HFRJCT書込開始失敗\n");
        fclose(scordf);
        free(buckets);
        return 2;
    }

    while (fgets(line, sizeof(line), scordf) != NULL) {
        scordf_rec_t order;
        char key[MIHFT_KEY_LEN];
        uint64_t notional;
        size_t bucket_index;
        bucket_pos_t pos;
        int throttle_decision;
        int rate_decision;
        int final_decision;

        if (strchr(line, '\n') == NULL && !feof(scordf)) {
            fprintf(stderr, "SCORDF行長超過\n");
            fclose(hfrjct);
            fclose(scordf);
            free(buckets);
            return 2;
        }

        if (!parse_scordf(line, &order)) {
            fprintf(stderr, "SCORDF解析失敗\n");
            fclose(hfrjct);
            fclose(scordf);
            free(buckets);
            return 2;
        }

        if (!checked_mul_u64(order.ord_qty, order.price_amt, &notional)) {
            fprintf(stderr, "元本算出桁あふれ\n");
            fclose(hfrjct);
            fclose(scordf);
            free(buckets);
            return 2;
        }

        if (!make_bucket_key(&order, key, sizeof(key))) {
            fprintf(stderr, "バケットキー生成失敗\n");
            fclose(hfrjct);
            fclose(scordf);
            free(buckets);
            return 2;
        }

        pos = find_bucket(buckets, bucket_len, key, window_ts);
        if (pos.found) {
            bucket_index = pos.index;
        } else if (!append_bucket(&buckets, &bucket_len, &bucket_cap, key, window_ts, &bucket_index)) {
            fprintf(stderr, "HFRATE領域確保失敗\n");
            fclose(hfrjct);
            fclose(scordf);
            free(buckets);
            return 2;
        }

        throttle_decision = existing_throttle_decision(&order, notional);
        rate_decision = rate_bucket_decision(&order, &buckets[bucket_index], notional);
        final_decision = stricter_decision(throttle_decision, rate_decision);
        batch_decision = stricter_decision(batch_decision, final_decision);

        if (final_decision == MIHFT_DEC_ACCEPT) {
            uint64_t next_cnt;
            uint64_t next_notional;

            if (!checked_add_u64(buckets[bucket_index].order_cnt, 1U, &next_cnt) ||
                !checked_add_u64(buckets[bucket_index].notional_amt, notional, &next_notional)) {
                fprintf(stderr, "HFRATE更新桁あふれ\n");
                fclose(hfrjct);
                fclose(scordf);
                free(buckets);
                return 2;
            }

            buckets[bucket_index].order_cnt = next_cnt;
            buckets[bucket_index].notional_amt = next_notional;
        } else {
            uint64_t next_drop;

            if (!checked_add_u64(buckets[bucket_index].drop_cnt, 1U, &next_drop)) {
                fprintf(stderr, "ドロップ件数桁あふれ\n");
                fclose(hfrjct);
                fclose(scordf);
                free(buckets);
                return 2;
            }

            buckets[bucket_index].drop_cnt = next_drop;

            if (!write_reject(hfrjct, reject_seq++, &order, throttle_decision, rate_decision, reject_ts)) {
                fprintf(stderr, "HFRJCT書込失敗\n");
                fclose(hfrjct);
                fclose(scordf);
                free(buckets);
                return 2;
            }
        }
    }

    if (ferror(scordf)) {
        fprintf(stderr, "SCORDF読込中断\n");
        fclose(hfrjct);
        fclose(scordf);
        free(buckets);
        return 2;
    }

    if (fclose(scordf) != 0) {
        fprintf(stderr, "SCORDF終了失敗\n");
        fclose(hfrjct);
        free(buckets);
        return 2;
    }

    if (fclose(hfrjct) != 0) {
        fprintf(stderr, "HFRJCT終了失敗\n");
        free(buckets);
        return 2;
    }

    if (!write_hfrate(hfrate_out_path, buckets, bucket_len)) {
        fprintf(stderr, "HFRATE書込失敗\n");
        free(buckets);
        return 2;
    }

    free(buckets);
    return batch_decision;
}
