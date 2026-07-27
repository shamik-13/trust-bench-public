/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210715  中川 美和 (E-283)  初版作成
 * 1.01  20211215  中川 美和 (E-283)  ティック階層判定と価格倍数検査を追加
 * 1.02  20220515  藤田 和也 (E-271)  SCINSTF読込時の桁あふれ検査を強化
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
    MIHFT_RC_PARSE = 2,
    MIHFT_RC_IO = 6,
    MIHFT_RC_NO_DATA = 10
};

#define MIHFT_SCINSTF_PATH "SCINSTF.csv"
#define MIHFT_LINE_MAX 1024
#define MIHFT_FIELD_MAX 128

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);

    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static char *mihft_trim(char *s)
{
    char *end;

    while (*s == ' ' || *s == '\t') {
        ++s;
    }

    end = s + strlen(s);
    while (end > s && (end[-1] == ' ' || end[-1] == '\t')) {
        *--end = '\0';
    }

    return s;
}

static bool mihft_parse_i64(const char *s, int64_t min_value, int64_t max_value, int64_t *out)
{
    char *end = NULL;
    long long value;

    if (s == NULL || *s == '\0') {
        return false;
    }

    errno = 0;
    value = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return false;
    }
    if (value < min_value || value > max_value) {
        return false;
    }

    *out = (int64_t)value;
    return true;
}

static bool mihft_split_csv(char *line, char *fields[], size_t want)
{
    size_t count = 0U;
    char *p = line;

    while (count < want) {
        fields[count++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    if (count != want || strchr(fields[want - 1U], ',') != NULL) {
        return false;
    }

    for (size_t i = 0U; i < want; ++i) {
        fields[i] = mihft_trim(fields[i]);
    }

    return true;
}

static bool mihft_is_header(const char *s)
{
    return strcmp(s, "INSTR-CODE") == 0 || strcmp(s, "INSTR_CODE") == 0;
}

static int mihft_tick_from_tier(int tier, int64_t *tick_minor, int *rate_bp)
{
    switch (tier) {
    case 1:
        *tick_minor = 100;
        *rate_bp = 1000;
        return 0;
    case 2:
        *tick_minor = 500;
        *rate_bp = 2000;
        return 0;
    case 3:
        *tick_minor = 1000;
        *rate_bp = 4000;
        return 0;
    default:
        return 12;
    }
}

static bool mihft_price_multiple_ok(int64_t price_minor, int64_t tick_minor)
{
    return tick_minor > 0 && price_minor >= 0 && (price_minor % tick_minor) == 0;
}

static bool mihft_mul_over_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a < 0 || b < 0) {
        return true;
    }
    if (a != 0 && b > INT64_MAX / a) {
        return true;
    }

    *out = a * b;
    return false;
}

int main(void)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    unsigned long lineno = 0UL;
    unsigned long accepted = 0UL;
    int result_code = 0;

    fp = fopen(MIHFT_SCINSTF_PATH, "r");
    if (fp == NULL) {
        fprintf(stderr, "SCINSTFを開けません: %s\n", MIHFT_SCINSTF_PATH);
        return MIHFT_RC_IO;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *fields[6];
        int64_t tier64;
        int64_t tick_file;
        int64_t lot_qty;
        int64_t tick_auth;
        int64_t notional_unit;
        int rate_bp;
        int tier;
        int tick_rc;

        ++lineno;
        mihft_chomp(line);

        if (line[0] == '\0') {
            continue;
        }

        if (!mihft_split_csv(line, fields, 6U)) {
            fprintf(stderr, "SCINSTF形式不正: 行=%lu\n", lineno);
            fclose(fp);
            return MIHFT_RC_PARSE;
        }

        if (lineno == 1UL && mihft_is_header(fields[0])) {
            continue;
        }

        if (fields[0][0] == '\0' || strlen(fields[0]) >= MIHFT_FIELD_MAX ||
            fields[1][0] == '\0' || strlen(fields[1]) >= MIHFT_FIELD_MAX ||
            fields[5][0] == '\0' || strlen(fields[5]) >= MIHFT_FIELD_MAX) {
            fprintf(stderr, "SCINSTF項目長不正: 行=%lu\n", lineno);
            fclose(fp);
            return MIHFT_RC_PARSE;
        }

        if (!mihft_parse_i64(fields[2], 1, 3, &tier64) ||
            !mihft_parse_i64(fields[3], 1, INT64_MAX, &tick_file) ||
            !mihft_parse_i64(fields[4], 1, INT64_MAX, &lot_qty)) {
            fprintf(stderr, "SCINSTF数値不正: 行=%lu 銘柄=%s\n", lineno, fields[0]);
            fclose(fp);
            return MIHFT_RC_PARSE;
        }

        tier = (int)tier64;
        tick_rc = mihft_tick_from_tier(tier, &tick_auth, &rate_bp);
        if (tick_rc != 0) {
            result_code = 12;
            fprintf(stderr, "ティック階層不正: 行=%lu 銘柄=%s\n", lineno, fields[0]);
            break;
        }

        if (!mihft_price_multiple_ok(tick_file, tick_auth)) {
            result_code = 12;
            fprintf(stderr, "ティック金額不正: 行=%lu 銘柄=%s\n", lineno, fields[0]);
            break;
        }

        if (mihft_mul_over_i64(tick_auth, lot_qty, &notional_unit)) {
            fprintf(stderr, "想定元本計算あふれ: 行=%lu 銘柄=%s\n", lineno, fields[0]);
            fclose(fp);
            return MIHFT_RC_PARSE;
        }

        if (notional_unit > MIHFT_MAX_NOTIONAL) {
            result_code = 8;
            fprintf(stderr, "想定元本上限超過: 行=%lu 銘柄=%s\n", lineno, fields[0]);
            break;
        }

        if (rate_bp <= 0 || rate_bp > 10000) {
            result_code = 4;
            fprintf(stderr, "証拠金率不正: 行=%lu 銘柄=%s\n", lineno, fields[0]);
            break;
        }

        ++accepted;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCINSTF読込失敗: %s\n", MIHFT_SCINSTF_PATH);
        fclose(fp);
        return MIHFT_RC_IO;
    }

    fclose(fp);

    if (accepted == 0UL && result_code == 0) {
        fprintf(stderr, "SCINSTF有効行なし: %s\n", MIHFT_SCINSTF_PATH);
        return MIHFT_RC_NO_DATA;
    }

    return result_code;
}
