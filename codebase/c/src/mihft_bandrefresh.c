/* 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  20200310  藤田 和也 (E-271)  初版作成
 * 1.01  20200810  岡本 涼 (E-294)  気配境界による縮小抑止を追加
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MIHFT_DECISION_OK
#define MIHFT_DECISION_OK 0
#endif

#ifndef MIHFT_DECISION_REJECT
#define MIHFT_DECISION_REJECT 2
#endif

#ifndef MIHFT_DECISION_ERROR
#define MIHFT_DECISION_ERROR 8
#endif

#define AMT_SCALE 10000LL
#define MAX_LINE 512
#define MAX_CODE 32
#define MAX_TS 32
#define MAX_SOURCE 8
#define MAX_BAND 8192
#define MIN_BAND_TICKS 20LL
#define MAX_WIDEN_TICKS 200LL

typedef struct {
    char instr_code[MAX_CODE];
    int64_t lower_amt;
    int64_t upper_amt;
    char band_ts[MAX_TS];
    char source_kbn[MAX_SOURCE];
    int used;
} band_record_t;

typedef struct {
    char instr_code[MAX_CODE];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    char tick_ts[MAX_TS];
} market_record_t;

static void trim_field(char *s)
{
    size_t n;
    char *p = s;

    while (*p != '\0' && isspace((unsigned char)*p)) {
        p++;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1U);
    }

    n = strlen(s);
    while (n > 0U && isspace((unsigned char)s[n - 1U])) {
        s[--n] = '\0';
    }

    if (n >= 2U && s[0] == '"' && s[n - 1U] == '"') {
        memmove(s, s + 1, n - 2U);
        s[n - 2U] = '\0';
    }
}

static int split_csv(char *line, char *fields[], size_t max_fields, size_t *count)
{
    size_t n = 0U;
    int quoted = 0;
    char *start = line;
    char *w = line;

    for (char *r = line; *r != '\0'; r++) {
        if (*r == '"') {
            quoted = !quoted;
            *w++ = *r;
        } else if (*r == ',' && !quoted) {
            if (n >= max_fields) {
                return -1;
            }
            *w++ = '\0';
            fields[n++] = start;
            start = w;
        } else if (*r != '\n' && *r != '\r') {
            *w++ = *r;
        }
    }

    if (n >= max_fields) {
        return -1;
    }
    *w = '\0';
    fields[n++] = start;

    for (size_t i = 0U; i < n; i++) {
        trim_field(fields[i]);
    }

    *count = n;
    return quoted ? -1 : 0;
}

static int copy_text(char *dst, size_t dst_size, const char *src)
{
    size_t n = strlen(src);

    if (n == 0U || n >= dst_size) {
        return -1;
    }
    memcpy(dst, src, n + 1U);
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

static int parse_amt(const char *s, int64_t *out)
{
    const char *p = s;
    int sign = 1;
    int64_t whole = 0;
    int64_t frac = 0;
    int frac_digits = 0;

    if (*p == '-') {
        sign = -1;
        p++;
    }

    if (!isdigit((unsigned char)*p)) {
        return -1;
    }

    while (isdigit((unsigned char)*p)) {
        int d = *p - '0';
        if (whole > (INT64_MAX - d) / 10LL) {
            return -1;
        }
        whole = whole * 10LL + d;
        p++;
    }

    if (*p == '.') {
        p++;
        while (isdigit((unsigned char)*p)) {
            if (frac_digits < 4) {
                frac = frac * 10LL + (*p - '0');
                frac_digits++;
            } else if (*p != '0') {
                return -1;
            }
            p++;
        }
    }

    if (*p != '\0' || whole > (INT64_MAX / AMT_SCALE)) {
        return -1;
    }

    while (frac_digits < 4) {
        frac *= 10LL;
        frac_digits++;
    }

    *out = (whole * AMT_SCALE + frac) * sign;
    return 0;
}

static int format_amt(int64_t v, char *buf, size_t size)
{
    int64_t whole;
    int64_t frac;
    int neg = v < 0;

    if (v == INT64_MIN) {
        return -1;
    }

    if (neg) {
        v = -v;
    }

    whole = v / AMT_SCALE;
    frac = v % AMT_SCALE;

    if (frac == 0) {
        return snprintf(buf, size, "%s%" PRId64, neg ? "-" : "", whole) > 0 ? 0 : -1;
    }

    if (frac % 1000LL == 0) {
        return snprintf(buf, size, "%s%" PRId64 ".%" PRId64, neg ? "-" : "", whole, frac / 1000LL) > 0 ? 0 : -1;
    }
    if (frac % 100LL == 0) {
        return snprintf(buf, size, "%s%" PRId64 ".%02" PRId64, neg ? "-" : "", whole, frac / 100LL) > 0 ? 0 : -1;
    }
    if (frac % 10LL == 0) {
        return snprintf(buf, size, "%s%" PRId64 ".%03" PRId64, neg ? "-" : "", whole, frac / 10LL) > 0 ? 0 : -1;
    }
    return snprintf(buf, size, "%s%" PRId64 ".%04" PRId64, neg ? "-" : "", whole, frac) > 0 ? 0 : -1;
}

static int64_t tick_size(int64_t amt)
{
    int64_t yen = amt / AMT_SCALE;

    if (yen < 1000LL) {
        return AMT_SCALE;
    }
    if (yen < 3000LL) {
        return 5LL * AMT_SCALE;
    }
    if (yen < 30000LL) {
        return 10LL * AMT_SCALE;
    }
    if (yen < 50000LL) {
        return 50LL * AMT_SCALE;
    }
    return 100LL * AMT_SCALE;
}

static int64_t floor_tick(int64_t amt)
{
    int64_t tick = tick_size(amt);
    if (amt >= 0) {
        return (amt / tick) * tick;
    }
    return -(((-amt + tick - 1LL) / tick) * tick);
}

static int64_t ceil_tick(int64_t amt)
{
    int64_t tick = tick_size(amt);
    if (amt >= 0) {
        return ((amt + tick - 1LL) / tick) * tick;
    }
    return -((-amt / tick) * tick);
}

static int read_band_file(const char *path, band_record_t bands[], size_t *band_count)
{
    FILE *fp = fopen(path, "r");
    char line[MAX_LINE];
    size_t n = 0U;

    if (fp == NULL) {
        fprintf(stderr, "SCBAND入力を開けません:%s\n", path);
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *fields[5];
        size_t count = 0U;

        if (n >= MAX_BAND || split_csv(line, fields, 5U, &count) != 0 || count != 5U) {
            fprintf(stderr, "SCBAND形式不正:%zu\n", n + 1U);
            fclose(fp);
            return -1;
        }

        if (copy_text(bands[n].instr_code, sizeof bands[n].instr_code, fields[0]) != 0 ||
            parse_amt(fields[1], &bands[n].lower_amt) != 0 ||
            parse_amt(fields[2], &bands[n].upper_amt) != 0 ||
            copy_text(bands[n].band_ts, sizeof bands[n].band_ts, fields[3]) != 0 ||
            copy_text(bands[n].source_kbn, sizeof bands[n].source_kbn, fields[4]) != 0 ||
            bands[n].lower_amt <= 0 || bands[n].upper_amt <= bands[n].lower_amt) {
            fprintf(stderr, "SCBAND値不正:%zu\n", n + 1U);
            fclose(fp);
            return -1;
        }

        bands[n].used = 0;
        n++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCBAND読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *band_count = n;
    return 0;
}

static int parse_market_line(char *line, market_record_t *mkt)
{
    char *fields[6];
    size_t count = 0U;

    if (split_csv(line, fields, 6U, &count) != 0 || count != 6U) {
        return -1;
    }

    if (copy_text(mkt->instr_code, sizeof mkt->instr_code, fields[0]) != 0 ||
        parse_amt(fields[1], &mkt->bid_amt) != 0 ||
        parse_amt(fields[2], &mkt->ask_amt) != 0 ||
        parse_amt(fields[3], &mkt->last_amt) != 0 ||
        parse_i64(fields[4], &mkt->vol_qty) != 0 ||
        copy_text(mkt->tick_ts, sizeof mkt->tick_ts, fields[5]) != 0) {
        return -1;
    }

    if (mkt->bid_amt <= 0 || mkt->ask_amt <= mkt->bid_amt ||
        mkt->last_amt <= 0 || mkt->vol_qty < 0) {
        return -1;
    }

    return 0;
}

static band_record_t *find_band(band_record_t bands[], size_t band_count, const char *instr_code)
{
    for (size_t i = 0U; i < band_count; i++) {
        if (strcmp(bands[i].instr_code, instr_code) == 0) {
            return &bands[i];
        }
    }
    return NULL;
}

static int refresh_band(band_record_t *band, const market_record_t *mkt)
{
    int64_t old_lower = band->lower_amt;
    int64_t old_upper = band->upper_amt;
    int64_t old_mid = old_lower + (old_upper - old_lower) / 2LL;
    int64_t tick = tick_size(mkt->last_amt);
    int64_t width = old_upper - old_lower;
    int64_t min_width = tick * MIN_BAND_TICKS;
    int64_t move_ticks;
    int64_t shift;
    int64_t widen;
    int64_t new_lower;
    int64_t new_upper;

    if (width < min_width) {
        width = min_width;
    }

    move_ticks = (mkt->last_amt - old_mid) / tick;
    if (move_ticks < 0) {
        move_ticks = -move_ticks;
    }
    if (move_ticks > MAX_WIDEN_TICKS) {
        move_ticks = MAX_WIDEN_TICKS;
    }

    shift = (mkt->last_amt - old_mid) / 2LL;
    widen = move_ticks * tick;
    if (mkt->vol_qty > 1000000LL && widen <= INT64_MAX - (10LL * tick)) {
        widen += 10LL * tick;
    }

    if (width > INT64_MAX - widen) {
        return -1;
    }
    width += widen;

    new_lower = floor_tick(mkt->last_amt + shift - width / 2LL);
    new_upper = ceil_tick(mkt->last_amt + shift + width / 2LL);

    if (new_lower > mkt->bid_amt) {
        new_lower = floor_tick(mkt->bid_amt);
    }
    if (new_upper < mkt->ask_amt) {
        new_upper = ceil_tick(mkt->ask_amt);
    }
    if (new_upper <= new_lower) {
        new_lower = floor_tick(mkt->bid_amt - min_width / 2LL);
        new_upper = ceil_tick(mkt->ask_amt + min_width / 2LL);
    }

    band->lower_amt = new_lower;
    band->upper_amt = new_upper;
    if (copy_text(band->band_ts, sizeof band->band_ts, mkt->tick_ts) != 0 ||
        copy_text(band->source_kbn, sizeof band->source_kbn, "M") != 0) {
        return -1;
    }
    band->used = 1;

    return 0;
}

static int write_band_file(const char *path, const band_record_t bands[], size_t band_count)
{
    FILE *fp = fopen(path, "w");

    if (fp == NULL) {
        fprintf(stderr, "SCBAND出力を開けません:%s\n", path);
        return -1;
    }

    for (size_t i = 0U; i < band_count; i++) {
        char lower[48];
        char upper[48];

        if (format_amt(bands[i].lower_amt, lower, sizeof lower) != 0 ||
            format_amt(bands[i].upper_amt, upper, sizeof upper) != 0) {
            fprintf(stderr, "SCBAND金額編集失敗:%zu\n", i + 1U);
            fclose(fp);
            return -1;
        }

        if (fprintf(fp, "%s,%s,%s,%s,%s\n",
                    bands[i].instr_code, lower, upper,
                    bands[i].band_ts, bands[i].source_kbn) < 0) {
            fprintf(stderr, "SCBAND書込失敗:%zu\n", i + 1U);
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCBAND終端書込失敗\n");
        return -1;
    }

    return 0;
}

int main(void)
{
    const char *band_in = "SCBAND.csv";
    const char *market_in = "SCMKTD.csv";
    const char *band_out = "SCBAND.out.csv";
    band_record_t bands[MAX_BAND];
    size_t band_count = 0U;
    FILE *mfp;
    char line[MAX_LINE];
    size_t line_no = 0U;
    int rejected = 0;

    if (read_band_file(band_in, bands, &band_count) != 0) {
        return MIHFT_DECISION_ERROR;
    }

    mfp = fopen(market_in, "r");
    if (mfp == NULL) {
        fprintf(stderr, "SCMKTD入力を開けません:%s\n", market_in);
        return MIHFT_DECISION_ERROR;
    }

    while (fgets(line, sizeof line, mfp) != NULL) {
        market_record_t mkt;
        band_record_t *band;

        line_no++;
        if (parse_market_line(line, &mkt) != 0) {
            fprintf(stderr, "SCMKTD形式不正:%zu\n", line_no);
            fclose(mfp);
            return MIHFT_DECISION_ERROR;
        }

        band = find_band(bands, band_count, mkt.instr_code);
        if (band == NULL) {
            rejected = 1;
            fprintf(stderr, "価格帯未登録:%s\n", mkt.instr_code);
            continue;
        }

        if (refresh_band(band, &mkt) != 0) {
            fprintf(stderr, "価格帯更新失敗:%s\n", mkt.instr_code);
            fclose(mfp);
            return MIHFT_DECISION_ERROR;
        }
    }

    if (ferror(mfp)) {
        fprintf(stderr, "SCMKTD読込失敗\n");
        fclose(mfp);
        return MIHFT_DECISION_ERROR;
    }
    fclose(mfp);

    if (write_band_file(band_out, bands, band_count) != 0) {
        return MIHFT_DECISION_ERROR;
    }

    return rejected ? MIHFT_DECISION_REJECT : MIHFT_DECISION_OK;
}
