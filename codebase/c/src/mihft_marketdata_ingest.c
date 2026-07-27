/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220906  福田 亮太 (E-211)  初版作成
 * 1.01  20230206  今井 彩 (E-230)  逆行ティック破棄と気配出力を追加
 */
#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_RC_OK 0
#define MIHFT_RC_IO 12
#define MIHFT_RC_PARSE 16

#define MIHFT_MAX_LINE 512
#define MIHFT_MAX_CODE 32
#define MIHFT_CACHE_SIZE 4096

typedef struct {
    char instr_code[MIHFT_MAX_CODE];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    int64_t tick_ts;
} mktd_row_t;

typedef struct {
    char instr_code[MIHFT_MAX_CODE];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t mid_amt;
    int64_t spread_amt;
    int64_t quote_ts;
    int64_t last_amt;
    int64_t vol_qty;
    int used;
} quote_cache_t;

static quote_cache_t g_cache[MIHFT_CACHE_SIZE];

static void trim_eol(char *s)
{
    size_t n = strlen(s);

    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int parse_i64(const char *s, int64_t *out)
{
    char *endp;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || endp == s || *endp != '\0') {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int copy_code(char *dst, size_t dstsz, const char *src)
{
    size_t len;

    if (dst == NULL || src == NULL || dstsz == 0U || *src == '\0') {
        return -1;
    }

    len = strlen(src);
    if (len >= dstsz) {
        return -1;
    }

    memcpy(dst, src, len + 1U);
    return 0;
}

static int parse_mktd_csv(char *line, mktd_row_t *row)
{
    char *cols[6];
    char *p;
    size_t i;

    if (line == NULL || row == NULL) {
        return -1;
    }

    trim_eol(line);
    p = line;

    for (i = 0U; i < 6U; i++) {
        cols[i] = p;
        p = strchr(p, ',');
        if (i < 5U) {
            if (p == NULL) {
                return -1;
            }
            *p++ = '\0';
        } else if (p != NULL) {
            return -1;
        }
    }

    if (copy_code(row->instr_code, sizeof(row->instr_code), cols[0]) != 0) {
        return -1;
    }

    if (parse_i64(cols[1], &row->bid_amt) != 0 ||
        parse_i64(cols[2], &row->ask_amt) != 0 ||
        parse_i64(cols[3], &row->last_amt) != 0 ||
        parse_i64(cols[4], &row->vol_qty) != 0 ||
        parse_i64(cols[5], &row->tick_ts) != 0) {
        return -1;
    }

    if (row->bid_amt <= 0 ||
        row->ask_amt <= 0 ||
        row->last_amt <= 0 ||
        row->vol_qty < 0 ||
        row->tick_ts <= 0 ||
        row->bid_amt > row->ask_amt) {
        return -1;
    }

    return 0;
}

static unsigned long hash_code(const char *s)
{
    unsigned long h = 1469598103UL;

    while (*s != '\0') {
        h ^= (unsigned char)*s++;
        h *= 16777619UL;
    }

    return h;
}

static quote_cache_t *find_quote_slot(const char *instr_code)
{
    unsigned long start;
    unsigned long i;

    if (instr_code == NULL || *instr_code == '\0') {
        return NULL;
    }

    start = hash_code(instr_code) % MIHFT_CACHE_SIZE;

    for (i = 0UL; i < MIHFT_CACHE_SIZE; i++) {
        quote_cache_t *slot = &g_cache[(start + i) % MIHFT_CACHE_SIZE];

        if (!slot->used) {
            if (copy_code(slot->instr_code, sizeof(slot->instr_code), instr_code) != 0) {
                return NULL;
            }
            slot->used = 1;
            return slot;
        }

        if (strcmp(slot->instr_code, instr_code) == 0) {
            return slot;
        }
    }

    return NULL;
}

static int mihft_pricecache_apply(const mktd_row_t *tick, quote_cache_t **updated)
{
    quote_cache_t *slot;
    int64_t spread;
    int64_t mid;

    if (tick == NULL || updated == NULL) {
        return -1;
    }

    *updated = NULL;

    slot = find_quote_slot(tick->instr_code);
    if (slot == NULL) {
        return -1;
    }

    if (slot->quote_ts > 0 && tick->tick_ts < slot->quote_ts) {
        return 0;
    }

    if (tick->bid_amt > INT64_MAX - tick->ask_amt) {
        return -1;
    }

    spread = tick->ask_amt - tick->bid_amt;
    mid = tick->bid_amt + (spread / 2);

    slot->bid_amt = tick->bid_amt;
    slot->ask_amt = tick->ask_amt;
    slot->mid_amt = mid;
    slot->spread_amt = spread;
    slot->quote_ts = tick->tick_ts;
    slot->last_amt = tick->last_amt;
    slot->vol_qty = tick->vol_qty;
    *updated = slot;

    return 0;
}

static int write_hfquotf(FILE *fp, const quote_cache_t *q)
{
    if (fp == NULL || q == NULL) {
        return -1;
    }

    if (fprintf(fp, "%s,%lld,%lld,%lld,%lld,%lld\n",
                q->instr_code,
                (long long)q->bid_amt,
                (long long)q->ask_amt,
                (long long)q->mid_amt,
                (long long)q->spread_amt,
                (long long)q->quote_ts) < 0) {
        return -1;
    }

    return 0;
}

static int is_header_line(const char *line)
{
    return strncmp(line, "INSTR-CODE,", 11U) == 0 ||
           strncmp(line, "INSTR_CODE,", 11U) == 0;
}

int main(void)
{
    const char *in_path = getenv("SCMKTD_CSV");
    const char *out_path = getenv("HFQUOTF_CSV");
    FILE *in_fp;
    FILE *out_fp;
    char line[MIHFT_MAX_LINE];
    unsigned long line_no = 0UL;
    unsigned long accepted = 0UL;
    unsigned long rejected = 0UL;

    if (in_path == NULL || *in_path == '\0') {
        in_path = "SCMKTD.csv";
    }
    if (out_path == NULL || *out_path == '\0') {
        out_path = "HFQUOTF.csv";
    }

    in_fp = fopen(in_path, "r");
    if (in_fp == NULL) {
        fprintf(stderr, "E001:SCMKTD入力オープン失敗:%s\n", in_path);
        return MIHFT_RC_IO;
    }

    out_fp = fopen(out_path, "w");
    if (out_fp == NULL) {
        fprintf(stderr, "E002:HFQUOTF出力オープン失敗:%s\n", out_path);
        fclose(in_fp);
        return MIHFT_RC_IO;
    }

    if (fprintf(out_fp, "INSTR-CODE,BID-AMT,ASK-AMT,MID-AMT,SPREAD-AMT,QUOTE-TS\n") < 0) {
        fprintf(stderr, "E003:HFQUOTF見出し出力失敗\n");
        fclose(out_fp);
        fclose(in_fp);
        return MIHFT_RC_IO;
    }

    while (fgets(line, sizeof(line), in_fp) != NULL) {
        mktd_row_t tick;
        quote_cache_t *updated = NULL;

        line_no++;
        if (line_no == 1UL && is_header_line(line)) {
            continue;
        }

        if (strchr(line, '\n') == NULL && !feof(in_fp)) {
            fprintf(stderr, "E004:SCMKTD行長過大:%lu\n", line_no);
            fclose(out_fp);
            fclose(in_fp);
            return MIHFT_RC_PARSE;
        }

        if (parse_mktd_csv(line, &tick) != 0) {
            fprintf(stderr, "E005:SCMKTD形式不正:%lu\n", line_no);
            fclose(out_fp);
            fclose(in_fp);
            return MIHFT_RC_PARSE;
        }

        if (mihft_pricecache_apply(&tick, &updated) != 0) {
            fprintf(stderr, "E006:気配キャッシュ更新失敗:%lu\n", line_no);
            fclose(out_fp);
            fclose(in_fp);
            return MIHFT_RC_PARSE;
        }

        if (updated == NULL) {
            rejected++;
            continue;
        }

        if (write_hfquotf(out_fp, updated) != 0) {
            fprintf(stderr, "E007:HFQUOTF出力失敗:%lu\n", line_no);
            fclose(out_fp);
            fclose(in_fp);
            return MIHFT_RC_IO;
        }

        accepted++;
    }

    if (ferror(in_fp)) {
        fprintf(stderr, "E008:SCMKTD入力読込失敗\n");
        fclose(out_fp);
        fclose(in_fp);
        return MIHFT_RC_IO;
    }

    if (fflush(out_fp) != 0 || fclose(out_fp) != 0) {
        fprintf(stderr, "E009:HFQUOTFクローズ失敗\n");
        fclose(in_fp);
        return MIHFT_RC_IO;
    }

    if (fclose(in_fp) != 0) {
        fprintf(stderr, "E010:SCMKTDクローズ失敗\n");
        return MIHFT_RC_IO;
    }

    fprintf(stderr, "I001:採用=%lu,破棄=%lu\n", accepted, rejected);
    return MIHFT_RC_OK;
}
