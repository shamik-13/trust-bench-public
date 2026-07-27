/**************************************************************
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20191022  市場基盤部  初版作成
 * 1.01  20200322  市場基盤部  参照データ世代と手数料・立会判定を追加
 **************************************************************/

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_ERR_IO 64
#define MIHFT_ERR_PARSE 65
#define MIHFT_MAX_INSTR 4096
#define MIHFT_MAX_FEE 64
#define MIHFT_MAX_SESS 64
#define MIHFT_LINE_MAX 1024
#define MIHFT_CODE_MAX 32
#define MIHFT_NAME_MAX 96
#define MIHFT_BOARD_MAX 8

typedef struct {
    char board_code[MIHFT_BOARD_MAX];
    int64_t fee_rate_bp;
    int64_t min_fee_x100;
} MihftFeeRec;

typedef struct {
    int32_t sess_dt;
    char sess_kbn[MIHFT_CODE_MAX];
    int32_t open_hms;
    int32_t close_hms;
} MihftCalRec;

typedef struct {
    char instr_code[MIHFT_CODE_MAX];
    char instr_name[MIHFT_NAME_MAX];
    int32_t instr_tier;
    int64_t margin_rate_bp;
    int64_t tick_x100;
    int64_t lot_qty;
    char board_code[MIHFT_BOARD_MAX];
    const MihftFeeRec *fee;
    bool session_open;
} MihftInstrCacheRec;

typedef struct {
    MihftInstrCacheRec instr[MIHFT_MAX_INSTR];
    size_t instr_count;
    MihftFeeRec fee[MIHFT_MAX_FEE];
    size_t fee_count;
    MihftCalRec cal[MIHFT_MAX_SESS];
    size_t cal_count;
    uint64_t generation;
} MihftRefdataCache;

static char *mihft_trim(char *s)
{
    char *e;

    while (isspace((unsigned char)*s)) {
        ++s;
    }

    e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) {
        --e;
    }
    *e = '\0';
    return s;
}

static int mihft_split_csv(char *line, char **cols, size_t max_cols)
{
    size_t n = 0;
    char *p = line;

    while (n < max_cols) {
        cols[n++] = mihft_trim(p);
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return (int)n;
}

static bool mihft_copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dst_len) {
        return false;
    }
    memcpy(dst, src, n + 1);
    return true;
}

static bool mihft_parse_i64(const char *s, int64_t *out)
{
    char *end;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *mihft_trim(end) != '\0') {
        return false;
    }
    *out = (int64_t)v;
    return true;
}

static bool mihft_parse_i32(const char *s, int32_t *out)
{
    int64_t v;

    if (!mihft_parse_i64(s, &v) || v < INT32_MIN || v > INT32_MAX) {
        return false;
    }
    *out = (int32_t)v;
    return true;
}

static bool mihft_parse_decimal_x100(const char *s, int64_t *out)
{
    int64_t whole = 0;
    int64_t frac = 0;
    int frac_digits = 0;
    bool neg = false;
    const unsigned char *p = (const unsigned char *)s;

    if (*p == '-') {
        neg = true;
        ++p;
    }
    if (!isdigit(*p)) {
        return false;
    }
    while (isdigit(*p)) {
        if (whole > (INT64_MAX - 9) / 10) {
            return false;
        }
        whole = whole * 10 + (*p++ - '0');
    }
    if (*p == '.') {
        ++p;
        while (isdigit(*p) && frac_digits < 2) {
            frac = frac * 10 + (*p++ - '0');
            ++frac_digits;
        }
        while (isdigit(*p)) {
            if (*p++ != '0') {
                return false;
            }
        }
    }
    if (*p != '\0' || whole > (INT64_MAX - 99) / 100) {
        return false;
    }
    while (frac_digits++ < 2) {
        frac *= 10;
    }
    *out = neg ? -(whole * 100 + frac) : (whole * 100 + frac);
    return true;
}

static bool mihft_tier_spec(int32_t tier, int64_t *rate_bp, int64_t *tick_x100)
{
    if (tier == 1) {
        *rate_bp = 1000;
        *tick_x100 = 100;
        return true;
    }
    if (tier == 2) {
        *rate_bp = 2000;
        *tick_x100 = 500;
        return true;
    }
    if (tier == 3) {
        *rate_bp = 4000;
        *tick_x100 = 1000;
        return true;
    }
    return false;
}

static const MihftFeeRec *mihft_find_fee(const MihftRefdataCache *cache, const char *board_code)
{
    size_t i;

    for (i = 0; i < cache->fee_count; ++i) {
        if (strcmp(cache->fee[i].board_code, board_code) == 0) {
            return &cache->fee[i];
        }
    }
    return NULL;
}

static bool mihft_valid_board(const char *board_code)
{
    return strcmp(board_code, "T1") == 0 ||
           strcmp(board_code, "ST") == 0 ||
           strcmp(board_code, "ETF") == 0;
}

static int32_t mihft_today_yyyymmdd(void)
{
    time_t now = time(NULL);
    struct tm *lt = localtime(&now);

    if (lt == NULL) {
        return 0;
    }
    return (int32_t)((lt->tm_year + 1900) * 10000 + (lt->tm_mon + 1) * 100 + lt->tm_mday);
}

static int32_t mihft_now_hms(void)
{
    time_t now = time(NULL);
    struct tm *lt = localtime(&now);

    if (lt == NULL) {
        return -1;
    }
    return (int32_t)(lt->tm_hour * 10000 + lt->tm_min * 100 + lt->tm_sec);
}

static bool mihft_session_open(const MihftRefdataCache *cache)
{
    int32_t today = mihft_today_yyyymmdd();
    int32_t now_hms = mihft_now_hms();
    size_t i;

    if (today == 0 || now_hms < 0) {
        return false;
    }
    for (i = 0; i < cache->cal_count; ++i) {
        const MihftCalRec *r = &cache->cal[i];

        if (r->sess_dt == today && r->open_hms <= now_hms && now_hms <= r->close_hms) {
            return true;
        }
    }
    return false;
}

static int mihft_load_fee(MihftRefdataCache *cache, const char *path)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "SCFEEFを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[3];
        int n;
        MihftFeeRec *r;

        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fclose(fp);
            fprintf(stderr, "SCFEEFの行が長すぎます\n");
            return MIHFT_ERR_PARSE;
        }
        if (strncmp(line, "BOARD-CODE", 10) == 0 || mihft_trim(line)[0] == '\0') {
            continue;
        }
        n = mihft_split_csv(line, cols, 3);
        if (n != 3 || cache->fee_count >= MIHFT_MAX_FEE) {
            fclose(fp);
            fprintf(stderr, "SCFEEFの項目数または件数が不正です\n");
            return MIHFT_ERR_PARSE;
        }

        r = &cache->fee[cache->fee_count];
        if (!mihft_copy_field(r->board_code, sizeof(r->board_code), cols[0]) ||
            !mihft_valid_board(r->board_code) ||
            !mihft_parse_decimal_x100(cols[1], &r->fee_rate_bp) ||
            !mihft_parse_decimal_x100(cols[2], &r->min_fee_x100) ||
            r->fee_rate_bp < 0 || r->min_fee_x100 < 0) {
            fclose(fp);
            fprintf(stderr, "SCFEEFの値が不正です\n");
            return MIHFT_ERR_PARSE;
        }
        ++cache->fee_count;
    }

    if (ferror(fp) || fclose(fp) != 0) {
        fprintf(stderr, "SCFEEFの読込に失敗しました\n");
        return MIHFT_ERR_IO;
    }
    return 0;
}

static int mihft_load_calendar(MihftRefdataCache *cache, const char *path)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "SCCALFを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[4];
        int n;
        MihftCalRec *r;

        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fclose(fp);
            fprintf(stderr, "SCCALFの行が長すぎます\n");
            return MIHFT_ERR_PARSE;
        }
        if (strncmp(line, "SESS-DT", 7) == 0 || mihft_trim(line)[0] == '\0') {
            continue;
        }
        n = mihft_split_csv(line, cols, 4);
        if (n != 4 || cache->cal_count >= MIHFT_MAX_SESS) {
            fclose(fp);
            fprintf(stderr, "SCCALFの項目数または件数が不正です\n");
            return MIHFT_ERR_PARSE;
        }

        r = &cache->cal[cache->cal_count];
        if (!mihft_parse_i32(cols[0], &r->sess_dt) ||
            !mihft_copy_field(r->sess_kbn, sizeof(r->sess_kbn), cols[1]) ||
            !mihft_parse_i32(cols[2], &r->open_hms) ||
            !mihft_parse_i32(cols[3], &r->close_hms) ||
            r->sess_dt < 20000101 || r->open_hms < 0 || r->close_hms > 235959 ||
            r->open_hms > r->close_hms) {
            fclose(fp);
            fprintf(stderr, "SCCALFの値が不正です\n");
            return MIHFT_ERR_PARSE;
        }
        ++cache->cal_count;
    }

    if (ferror(fp) || fclose(fp) != 0) {
        fprintf(stderr, "SCCALFの読込に失敗しました\n");
        return MIHFT_ERR_IO;
    }
    return 0;
}

static int mihft_load_instruments(MihftRefdataCache *cache, const char *path)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];
    bool open_now = mihft_session_open(cache);

    if (fp == NULL) {
        fprintf(stderr, "SCINSTFを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[6];
        int n;
        MihftInstrCacheRec *r;
        int64_t tick_in;

        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fclose(fp);
            fprintf(stderr, "SCINSTFの行が長すぎます\n");
            return MIHFT_ERR_PARSE;
        }
        if (strncmp(line, "INSTR-CODE", 10) == 0 || mihft_trim(line)[0] == '\0') {
            continue;
        }
        n = mihft_split_csv(line, cols, 6);
        if (n != 6 || cache->instr_count >= MIHFT_MAX_INSTR) {
            fclose(fp);
            fprintf(stderr, "SCINSTFの項目数または件数が不正です\n");
            return MIHFT_ERR_PARSE;
        }

        r = &cache->instr[cache->instr_count];
        if (!mihft_copy_field(r->instr_code, sizeof(r->instr_code), cols[0]) ||
            !mihft_copy_field(r->instr_name, sizeof(r->instr_name), cols[1]) ||
            !mihft_parse_i32(cols[2], &r->instr_tier) ||
            !mihft_parse_decimal_x100(cols[3], &tick_in) ||
            !mihft_parse_i64(cols[4], &r->lot_qty) ||
            !mihft_copy_field(r->board_code, sizeof(r->board_code), cols[5]) ||
            !mihft_tier_spec(r->instr_tier, &r->margin_rate_bp, &r->tick_x100) ||
            !mihft_valid_board(r->board_code) ||
            tick_in != r->tick_x100 ||
            r->lot_qty <= 0) {
            fclose(fp);
            fprintf(stderr, "SCINSTFの値が不正です\n");
            return MIHFT_ERR_PARSE;
        }

        r->fee = mihft_find_fee(cache, r->board_code);
        r->session_open = open_now;
        ++cache->instr_count;
    }

    if (ferror(fp) || fclose(fp) != 0) {
        fprintf(stderr, "SCINSTFの読込に失敗しました\n");
        return MIHFT_ERR_IO;
    }
    return 0;
}

static int mihft_hotpath_probe(const MihftRefdataCache *cache)
{
    size_t i;

    for (i = 0; i < cache->instr_count; ++i) {
        const MihftInstrCacheRec *r = &cache->instr[i];
        int64_t notional;

        if (r->margin_rate_bp <= 0) {
            return 4;
        }
        if (r->tick_x100 <= 0 || (r->tick_x100 % 100) != 0) {
            return 12;
        }
        if (r->lot_qty > INT64_MAX / r->tick_x100) {
            return 8;
        }
        notional = r->tick_x100 * r->lot_qty / 100;
        if (notional > MIHFT_MAX_NOTIONAL || r->fee == NULL || !r->session_open) {
            return 8;
        }
    }
    return 0;
}

int main(void)
{
    MihftRefdataCache cache;
    int rc;

    memset(&cache, 0, sizeof(cache));
    cache.generation = 1;

    rc = mihft_load_fee(&cache, "SCFEEF.csv");
    if (rc != 0) {
        return rc;
    }

    rc = mihft_load_calendar(&cache, "SCCALF.csv");
    if (rc != 0) {
        return rc;
    }

    rc = mihft_load_instruments(&cache, "SCINSTF.csv");
    if (rc != 0) {
        return rc;
    }

    if (cache.instr_count == 0 || cache.fee_count == 0 || cache.cal_count == 0) {
        fprintf(stderr, "参照データが空です\n");
        return MIHFT_ERR_PARSE;
    }

    return mihft_hotpath_probe(&cache);
}
