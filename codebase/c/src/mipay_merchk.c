/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20250129  精算基盤  加盟店マスタ検証バッチ初版
 */

#include "mipay_settle.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIPAY_PSMERF_PATH "PSMERF.csv"
#define MIPAY_PSCONF_PATH "PSCONF.csv"
#define MIPAY_MERCHANT_CODE_LEN_LOCAL 10u
#define MIPAY_BANK_ACCT_MIN_LEN 7u
#define MIPAY_BANK_ACCT_MAX_LEN 8u
#define MIPAY_LINE_MAX 1024u
#define MIPAY_FIELD_MAX 256u
#define MIPAY_DATE_LEN 8u
#define MIPAY_TS_LEN 14u

enum {
    MIPAY_EXIT_OK = 0,
    MIPAY_ERR_IO = 20,
    MIPAY_ERR_PARSE = 21,
    MIPAY_ERR_OVERFLOW = 22
};

typedef struct {
    char merchant_code[MIPAY_FIELD_MAX];
    char merchant_name[MIPAY_FIELD_MAX];
    char merchant_status[3];
    char bank_acct_no[MIPAY_FIELD_MAX];
} psmerf_record_t;

typedef struct {
    unsigned long long total;
    unsigned long long invalid_code_len;
    unsigned long long invalid_bank_acct;
    unsigned long long stopped_merchant;
    unsigned long long missing_name;
    unsigned long long excluded;
} batch_count_t;

static void trim_crlf(char *s)
{
    size_t n = strlen(s);

    while (n > 0u && (s[n - 1u] == '\n' || s[n - 1u] == '\r')) {
        s[n - 1u] = '\0';
        --n;
    }
}

static int copy_field(char *dst, size_t dst_size, const char *src, size_t n)
{
    size_t head = 0u;
    size_t tail = n;
    size_t out = 0u;
    int quoted = 0;

    while (head < tail && (src[head] == ' ' || src[head] == '\t')) {
        ++head;
    }
    while (tail > head && (src[tail - 1u] == ' ' || src[tail - 1u] == '\t')) {
        --tail;
    }

    if (tail > head && src[head] == '"' && src[tail - 1u] == '"') {
        quoted = 1;
        ++head;
        --tail;
    }

    while (head < tail) {
        if (out + 1u >= dst_size) {
            return -1;
        }
        if (quoted && src[head] == '"' && head + 1u < tail && src[head + 1u] == '"') {
            dst[out++] = '"';
            head += 2u;
        } else {
            dst[out++] = src[head++];
        }
    }

    dst[out] = '\0';
    return 0;
}

static int parse_csv4(char *line, psmerf_record_t *rec)
{
    char *p = line;
    char *start = line;
    char *fields[4];
    size_t lens[4];
    size_t count = 0u;
    int quoted = 0;

    trim_crlf(line);

    for (; *p != '\0'; ++p) {
        if (*p == '"') {
            if (quoted && p[1] == '"') {
                ++p;
            } else {
                quoted = !quoted;
            }
        } else if (*p == ',' && !quoted) {
            if (count >= 4u) {
                return -1;
            }
            fields[count] = start;
            lens[count] = (size_t)(p - start);
            ++count;
            start = p + 1;
        }
    }

    if (quoted || count != 3u) {
        return -1;
    }

    fields[count] = start;
    lens[count] = (size_t)(p - start);

    if (copy_field(rec->merchant_code, sizeof(rec->merchant_code), fields[0], lens[0]) != 0 ||
        copy_field(rec->merchant_name, sizeof(rec->merchant_name), fields[1], lens[1]) != 0 ||
        copy_field(rec->merchant_status, sizeof(rec->merchant_status), fields[2], lens[2]) != 0 ||
        copy_field(rec->bank_acct_no, sizeof(rec->bank_acct_no), fields[3], lens[3]) != 0) {
        return -1;
    }

    return 0;
}

static int all_digits(const char *s)
{
    size_t i;

    if (s[0] == '\0') {
        return 0;
    }

    for (i = 0u; s[i] != '\0'; ++i) {
        if (s[i] < '0' || s[i] > '9') {
            return 0;
        }
    }

    return 1;
}

static int yyyymmdd(char out[MIPAY_DATE_LEN + 1u], const struct tm *tmv)
{
    int n = snprintf(out, MIPAY_DATE_LEN + 1u, "%04d%02d%02d",
                     tmv->tm_year + 1900, tmv->tm_mon + 1, tmv->tm_mday);
    return n == (int)MIPAY_DATE_LEN ? 0 : -1;
}

static int yyyymmddhhmmss(char out[MIPAY_TS_LEN + 1u], const struct tm *tmv)
{
    int n = snprintf(out, MIPAY_TS_LEN + 1u, "%04d%02d%02d%02d%02d%02d",
                     tmv->tm_year + 1900, tmv->tm_mon + 1, tmv->tm_mday,
                     tmv->tm_hour, tmv->tm_min, tmv->tm_sec);
    return n == (int)MIPAY_TS_LEN ? 0 : -1;
}

static int next_day(char out[MIPAY_DATE_LEN + 1u], struct tm base)
{
    time_t t;
    struct tm *tmv;

    base.tm_hour = 12;
    base.tm_min = 0;
    base.tm_sec = 0;
    base.tm_mday += 1;

    t = mktime(&base);
    if (t == (time_t)-1) {
        return -1;
    }

    tmv = localtime(&t);
    if (tmv == NULL) {
        return -1;
    }

    return yyyymmdd(out, tmv);
}

static int add_count(unsigned long long *v)
{
    if (*v == ULLONG_MAX) {
        return -1;
    }
    ++(*v);
    return 0;
}

static unsigned int validate_record(const psmerf_record_t *rec)
{
    unsigned int flag = 0u;
    size_t acct_len = strlen(rec->bank_acct_no);

    if (strlen(rec->merchant_code) != MIPAY_MERCHANT_CODE_LEN_LOCAL ||
        !all_digits(rec->merchant_code)) {
        flag |= 0x01u;
    }

    if (acct_len < MIPAY_BANK_ACCT_MIN_LEN ||
        acct_len > MIPAY_BANK_ACCT_MAX_LEN ||
        !all_digits(rec->bank_acct_no)) {
        flag |= 0x02u;
    }

    if (strcmp(rec->merchant_status, "01") != 0) {
        flag |= 0x04u;
    }

    if (rec->merchant_name[0] == '\0') {
        flag |= 0x08u;
    }

    return flag;
}

static int write_conf(FILE *fp, const char *key, unsigned long long value,
                      const char *apply_dt, const char *expire_dt, const char *updated_at)
{
    return fprintf(fp, "%s,%llu,%s,%s,%s\n", key, value, apply_dt, expire_dt, updated_at) < 0 ? -1 : 0;
}

int main(void)
{
    FILE *in;
    FILE *out;
    char line[MIPAY_LINE_MAX];
    char apply_dt[MIPAY_DATE_LEN + 1u];
    char expire_dt[MIPAY_DATE_LEN + 1u];
    char updated_at[MIPAY_TS_LEN + 1u];
    time_t now;
    struct tm *tmv;
    batch_count_t count;
    unsigned long long line_no = 0u;

    memset(&count, 0, sizeof(count));

    now = time(NULL);
    if (now == (time_t)-1) {
        fprintf(stderr, "時刻取得失敗\n");
        return MIPAY_ERR_IO;
    }

    tmv = localtime(&now);
    if (tmv == NULL ||
        yyyymmdd(apply_dt, tmv) != 0 ||
        yyyymmddhhmmss(updated_at, tmv) != 0 ||
        next_day(expire_dt, *tmv) != 0) {
        fprintf(stderr, "検査日時編集失敗\n");
        return MIPAY_ERR_IO;
    }

    in = fopen(MIPAY_PSMERF_PATH, "r");
    if (in == NULL) {
        fprintf(stderr, "PSMERF読込開始失敗:%d\n", errno);
        return MIPAY_ERR_IO;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        psmerf_record_t rec;
        unsigned int flag;

        if (add_count(&line_no) != 0) {
            fclose(in);
            fprintf(stderr, "件数上限超過\n");
            return MIPAY_ERR_OVERFLOW;
        }

        if (strchr(line, '\n') == NULL && !feof(in)) {
            fclose(in);
            fprintf(stderr, "PSMERF行長超過:%llu\n", line_no);
            return MIPAY_ERR_PARSE;
        }

        if (line_no == 1u &&
            strncmp(line, "MERCHANT-CODE,MERCHANT-NAME,MER-STATUS,BANK-ACCT-NO", 52u) == 0) {
            continue;
        }

        if (add_count(&count.total) != 0) {
            fclose(in);
            fprintf(stderr, "件数上限超過\n");
            return MIPAY_ERR_OVERFLOW;
        }

        memset(&rec, 0, sizeof(rec));
        if (parse_csv4(line, &rec) != 0) {
            fclose(in);
            fprintf(stderr, "PSMERF項目解析失敗:%llu\n", line_no);
            return MIPAY_ERR_PARSE;
        }

        flag = validate_record(&rec);

        if ((flag & 0x01u) != 0u && add_count(&count.invalid_code_len) != 0) {
            fclose(in);
            fprintf(stderr, "件数上限超過\n");
            return MIPAY_ERR_OVERFLOW;
        }
        if ((flag & 0x02u) != 0u && add_count(&count.invalid_bank_acct) != 0) {
            fclose(in);
            fprintf(stderr, "件数上限超過\n");
            return MIPAY_ERR_OVERFLOW;
        }
        if ((flag & 0x04u) != 0u && add_count(&count.stopped_merchant) != 0) {
            fclose(in);
            fprintf(stderr, "件数上限超過\n");
            return MIPAY_ERR_OVERFLOW;
        }
        if ((flag & 0x08u) != 0u && add_count(&count.missing_name) != 0) {
            fclose(in);
            fprintf(stderr, "件数上限超過\n");
            return MIPAY_ERR_OVERFLOW;
        }
        if (flag != 0u && add_count(&count.excluded) != 0) {
            fclose(in);
            fprintf(stderr, "件数上限超過\n");
            return MIPAY_ERR_OVERFLOW;
        }
    }

    if (ferror(in)) {
        fclose(in);
        fprintf(stderr, "PSMERF読込失敗:%d\n", errno);
        return MIPAY_ERR_IO;
    }

    if (fclose(in) != 0) {
        fprintf(stderr, "PSMERF終了失敗:%d\n", errno);
        return MIPAY_ERR_IO;
    }

    out = fopen(MIPAY_PSCONF_PATH, "w");
    if (out == NULL) {
        fprintf(stderr, "PSCONF更新開始失敗:%d\n", errno);
        return MIPAY_ERR_IO;
    }

    if (fprintf(out, "CONF-KEY,CONF-VALUE,APPLY-DT,EXPIRE-DT,UPDATED-AT\n") < 0 ||
        write_conf(out, "MIPAY.MERCHK.TOTAL", count.total, apply_dt, expire_dt, updated_at) != 0 ||
        write_conf(out, "MIPAY.MERCHK.EXCLUDED", count.excluded, apply_dt, expire_dt, updated_at) != 0 ||
        write_conf(out, "MIPAY.MERCHK.INVALID_CODE_LEN", count.invalid_code_len, apply_dt, expire_dt, updated_at) != 0 ||
        write_conf(out, "MIPAY.MERCHK.INVALID_BANK_ACCT", count.invalid_bank_acct, apply_dt, expire_dt, updated_at) != 0 ||
        write_conf(out, "MIPAY.MERCHK.STOPPED_MERCHANT", count.stopped_merchant, apply_dt, expire_dt, updated_at) != 0 ||
        write_conf(out, "MIPAY.MERCHK.MISSING_NAME", count.missing_name, apply_dt, expire_dt, updated_at) != 0) {
        fclose(out);
        fprintf(stderr, "PSCONF書込失敗:%d\n", errno);
        return MIPAY_ERR_IO;
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "PSCONF終了失敗:%d\n", errno);
        return MIPAY_ERR_IO;
    }

    return MIPAY_EXIT_OK;
}
