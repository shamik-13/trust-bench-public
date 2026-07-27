/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240624  決済基盤  初版作成
 * 1.01  20241118  決済基盤  失効済み通貨設定の除外を追加
 */

#include "mipay_settle.h"

#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

enum {
    MIPAY_FX_OK = 0,
    MIPAY_FX_ERR_IO = 20,
    MIPAY_FX_ERR_PARSE = 21,
    MIPAY_FX_MAX_LINE = 512,
    MIPAY_FX_MAX_CONF = 256,
    MIPAY_FX_KEY_LEN = 32,
    MIPAY_FX_VAL_LEN = 128,
    MIPAY_FX_PAIR_LEN = 8,
    MIPAY_FX_DATE_LEN = 8,
    MIPAY_FX_SRC_LEN = 12
};

typedef struct {
    char key[MIPAY_FX_KEY_LEN + 1];
    char value[MIPAY_FX_VAL_LEN + 1];
    char apply_dt[MIPAY_FX_DATE_LEN + 1];
    char expire_dt[MIPAY_FX_DATE_LEN + 1];
    char updated_at[20];
} FxConfRec;

typedef struct {
    char ccy_pair[MIPAY_FX_PAIR_LEN + 1];
    char rate_dt[MIPAY_FX_DATE_LEN + 1];
    double ttm_rate;
    char source_cd[MIPAY_FX_SRC_LEN + 1];
    char load_status[4];
} FxRateRec;

static void trim_field(char *s)
{
    size_t len;
    char *p;

    while (isspace((unsigned char)*s)) {
        memmove(s, s + 1, strlen(s));
    }

    len = strlen(s);
    while (len > 0 && isspace((unsigned char)s[len - 1])) {
        s[--len] = '\0';
    }

    if (len >= 2 && s[0] == '"' && s[len - 1] == '"') {
        memmove(s, s + 1, len - 2);
        s[len - 2] = '\0';
        for (p = s; *p != '\0'; ++p) {
            if (*p == '"' && p[1] == '"') {
                memmove(p, p + 1, strlen(p));
            }
        }
    }
}

static int split_csv(char *line, char *fields[], size_t max_fields)
{
    size_t n = 0;
    int quote = 0;
    char *p = line;
    char *start = line;

    while (*p != '\0') {
        if (*p == '"') {
            quote = !quote;
        } else if (*p == ',' && !quote) {
            if (n >= max_fields) {
                return -1;
            }
            *p = '\0';
            fields[n++] = start;
            start = p + 1;
        }
        ++p;
    }

    if (quote || n >= max_fields) {
        return -1;
    }

    fields[n++] = start;
    while (n < max_fields) {
        fields[n++] = NULL;
    }

    return (int)n;
}

static int copy_checked(char *dst, size_t dstsz, const char *src)
{
    size_t len;

    if (src == NULL || dstsz == 0) {
        return -1;
    }

    len = strlen(src);
    if (len >= dstsz) {
        return -1;
    }

    memcpy(dst, src, len + 1);
    return 0;
}

static int valid_yyyymmdd(const char *s)
{
    int y;
    int m;
    int d;
    struct tm tmv;
    char buf[5];

    if (s == NULL || strlen(s) != 8) {
        return 0;
    }
    for (size_t i = 0; i < 8; ++i) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
    }

    memcpy(buf, s, 4);
    buf[4] = '\0';
    y = atoi(buf);
    memcpy(buf, s + 4, 2);
    buf[2] = '\0';
    m = atoi(buf);
    memcpy(buf, s + 6, 2);
    buf[2] = '\0';
    d = atoi(buf);

    if (y < 1990 || y > 2099 || m < 1 || m > 12 || d < 1 || d > 31) {
        return 0;
    }

    memset(&tmv, 0, sizeof(tmv));
    tmv.tm_year = y - 1900;
    tmv.tm_mon = m - 1;
    tmv.tm_mday = d;
    tmv.tm_isdst = -1;

    if (mktime(&tmv) == (time_t)-1) {
        return 0;
    }

    return tmv.tm_year == y - 1900 && tmv.tm_mon == m - 1 && tmv.tm_mday == d;
}

static int valid_pair(const char *s)
{
    if (s == NULL || strlen(s) != 6) {
        return 0;
    }

    for (size_t i = 0; i < 6; ++i) {
        if (s[i] < 'A' || s[i] > 'Z') {
            return 0;
        }
    }

    return strncmp(s, s + 3, 3) != 0;
}

static int parse_rate(const char *s, double *out)
{
    char *endp;
    double v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtod(s, &endp);
    if (errno == ERANGE || endp == s || *endp != '\0' || v <= 0.0 || v > DBL_MAX) {
        return -1;
    }

    *out = v;
    return 0;
}

static int parse_conf_line(char *line, FxConfRec *rec)
{
    char *fields[5];

    if (split_csv(line, fields, 5) != 5) {
        return -1;
    }

    for (size_t i = 0; i < 5; ++i) {
        trim_field(fields[i]);
    }

    if (copy_checked(rec->key, sizeof(rec->key), fields[0]) != 0 ||
        copy_checked(rec->value, sizeof(rec->value), fields[1]) != 0 ||
        copy_checked(rec->apply_dt, sizeof(rec->apply_dt), fields[2]) != 0 ||
        copy_checked(rec->expire_dt, sizeof(rec->expire_dt), fields[3]) != 0 ||
        copy_checked(rec->updated_at, sizeof(rec->updated_at), fields[4]) != 0) {
        return -1;
    }

    if (!valid_yyyymmdd(rec->apply_dt) || !valid_yyyymmdd(rec->expire_dt)) {
        return -1;
    }

    return 0;
}

static int parse_rate_line(char *line, FxRateRec *rec)
{
    char *fields[4];

    if (split_csv(line, fields, 4) != 4) {
        return -1;
    }

    for (size_t i = 0; i < 4; ++i) {
        trim_field(fields[i]);
    }

    if (copy_checked(rec->ccy_pair, sizeof(rec->ccy_pair), fields[0]) != 0 ||
        copy_checked(rec->rate_dt, sizeof(rec->rate_dt), fields[1]) != 0 ||
        copy_checked(rec->source_cd, sizeof(rec->source_cd), fields[3]) != 0) {
        return -1;
    }

    if (!valid_pair(rec->ccy_pair) || !valid_yyyymmdd(rec->rate_dt) ||
        parse_rate(fields[2], &rec->ttm_rate) != 0) {
        return -1;
    }

    strcpy(rec->load_status, "00");
    return 0;
}

static int active_pair_allowed(const FxConfRec confs[], size_t n, const char *pair, const char *rate_dt)
{
    for (size_t i = 0; i < n; ++i) {
        if (strcmp(confs[i].key, "ALLOW_CCY_PAIR") == 0 &&
            strcmp(confs[i].value, pair) == 0 &&
            strcmp(confs[i].apply_dt, rate_dt) <= 0 &&
            strcmp(rate_dt, confs[i].expire_dt) <= 0) {
            return 1;
        }
    }

    return 0;
}

static int load_conf(const char *path, FxConfRec confs[], size_t *count)
{
    FILE *fp;
    char line[MIPAY_FX_MAX_LINE];
    size_t n = 0;
    unsigned long lineno = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fputs("PSC0:PSCONF入力を開始できません\n", stderr);
        return MIPAY_FX_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        size_t len = strlen(line);
        ++lineno;

        if (len > 0 && line[len - 1] == '\n') {
            line[--len] = '\0';
        } else if (len == sizeof(line) - 1) {
            fprintf(stderr, "PSC1:PSCONF行長超過:%lu\n", lineno);
            fclose(fp);
            return MIPAY_FX_ERR_PARSE;
        }

        if (lineno == 1 && strncmp(line, "CONF-KEY,", 9) == 0) {
            continue;
        }
        if (line[0] == '\0') {
            continue;
        }
        if (n >= MIPAY_FX_MAX_CONF) {
            fputs("PSC2:PSCONF件数上限超過\n", stderr);
            fclose(fp);
            return MIPAY_FX_ERR_PARSE;
        }
        if (parse_conf_line(line, &confs[n]) != 0) {
            fprintf(stderr, "PSC3:PSCONF形式不正:%lu\n", lineno);
            fclose(fp);
            return MIPAY_FX_ERR_PARSE;
        }
        ++n;
    }

    if (ferror(fp)) {
        fputs("PSC4:PSCONF読込異常\n", stderr);
        fclose(fp);
        return MIPAY_FX_ERR_IO;
    }

    fclose(fp);
    *count = n;
    return MIPAY_FX_OK;
}

static int write_fxrate(FILE *out, const FxRateRec *rec)
{
    if (fprintf(out, "%s,%s,%.10f,%s,%s\n",
                rec->ccy_pair,
                rec->rate_dt,
                rec->ttm_rate,
                rec->source_cd,
                rec->load_status) < 0) {
        return -1;
    }

    return 0;
}

int main(void)
{
    FxConfRec confs[MIPAY_FX_MAX_CONF];
    size_t conf_count = 0;
    FILE *in;
    FILE *out;
    char line[MIPAY_FX_MAX_LINE];
    unsigned long lineno = 0;
    int rc;

    rc = load_conf("psconf.csv", confs, &conf_count);
    if (rc != MIPAY_FX_OK) {
        return rc;
    }

    in = fopen("fxrate.csv", "r");
    if (in == NULL) {
        fputs("FXL0:為替レート入力を開始できません\n", stderr);
        return MIPAY_FX_ERR_IO;
    }

    out = fopen("psfxrf.csv", "w");
    if (out == NULL) {
        fputs("FXL1:PSFXRF出力を開始できません\n", stderr);
        fclose(in);
        return MIPAY_FX_ERR_IO;
    }

    if (fputs("CCY-PAIR,RATE-DT,TTM-RATE,SOURCE-CD,LOAD-STATUS\n", out) == EOF) {
        fputs("FXL2:PSFXRF見出し出力異常\n", stderr);
        fclose(out);
        fclose(in);
        return MIPAY_FX_ERR_IO;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        FxRateRec rec;
        size_t len = strlen(line);

        ++lineno;

        if (len > 0 && line[len - 1] == '\n') {
            line[--len] = '\0';
        } else if (len == sizeof(line) - 1) {
            fprintf(stderr, "FXL3:為替レート行長超過:%lu\n", lineno);
            fclose(out);
            fclose(in);
            return MIPAY_FX_ERR_PARSE;
        }

        if (lineno == 1 && strncmp(line, "CCY-PAIR,", 9) == 0) {
            continue;
        }
        if (line[0] == '\0') {
            continue;
        }

        if (parse_rate_line(line, &rec) != 0) {
            fprintf(stderr, "FXL4:為替レート形式不正:%lu\n", lineno);
            fclose(out);
            fclose(in);
            return MIPAY_FX_ERR_PARSE;
        }

        if (!active_pair_allowed(confs, conf_count, rec.ccy_pair, rec.rate_dt)) {
            continue;
        }

        if (write_fxrate(out, &rec) != 0) {
            fputs("FXL5:PSFXRF明細出力異常\n", stderr);
            fclose(out);
            fclose(in);
            return MIPAY_FX_ERR_IO;
        }
    }

    if (ferror(in)) {
        fputs("FXL6:為替レート読込異常\n", stderr);
        fclose(out);
        fclose(in);
        return MIPAY_FX_ERR_IO;
    }

    if (fclose(out) != 0) {
        fputs("FXL7:PSFXRF終了処理異常\n", stderr);
        fclose(in);
        return MIPAY_FX_ERR_IO;
    }

    if (fclose(in) != 0) {
        fputs("FXL8:為替レート終了処理異常\n", stderr);
        return MIPAY_FX_ERR_IO;
    }

    return MIPAY_FX_OK;
}
