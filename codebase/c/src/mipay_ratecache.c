/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    20240226    みらいペイ システム部    初版作成
 * 1.01    20240617    みらいペイ システム部    MCC別丸め単位の検証を追加
 * 1.02    20250210    みらいペイ システム部    再読込判定とCSV境界検査を追加
 */

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIPAY_RC_OK 0
#define MIPAY_RC_RELOAD 7
#define MIPAY_RC_IOERR 20
#define MIPAY_RC_PARSE 21
#define MIPAY_RC_CAPACITY 22

#define MAX_LINE_LEN 512
#define MAX_FIELD_LEN 64
#define MAX_MERCHANTS 4096
#define MAX_CACHE_ROWS 16384
#define MAX_MERCHANT_CODE 24
#define MAX_STATUS 2
#define MAX_MCC 4
#define MAX_RISK 2
#define MAX_CYCLE 2

typedef struct {
    char merchant_code[MAX_MERCHANT_CODE + 1];
    char merchant_status[MAX_STATUS + 1];
    char mcc[MAX_MCC + 1];
    int64_t daily_limit_amt;
    char risk_rank[MAX_RISK + 1];
    char settle_cycle_kbn[MAX_CYCLE + 1];
} MerchantRecord;

typedef struct {
    char merchant_code[MAX_MERCHANT_CODE + 1];
    char currency[4];
    int minor_digits;
    int rounding_unit;
    int mcc_weight;
    uint32_t cache_hash;
} RateCacheRow;

typedef struct {
    MerchantRecord merchants[MAX_MERCHANTS];
    size_t merchant_count;
    RateCacheRow rows[MAX_CACHE_ROWS];
    size_t row_count;
    int reload_required;
    int invalid_amount_count;
    int active_count;
} RateCacheState;

static void trim_right(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r' || s[n - 1] == ' ' || s[n - 1] == '\t')) {
        s[--n] = '\0';
    }
}

static char *trim_left(char *s)
{
    while (*s == ' ' || *s == '\t') {
        ++s;
    }
    return s;
}

static int is_digits(const char *s, size_t len)
{
    size_t i;
    if (strlen(s) != len) {
        return 0;
    }
    for (i = 0; i < len; ++i) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
    }
    return 1;
}

static int copy_checked(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);
    if (n + 1 > dstsz) {
        return 0;
    }
    memcpy(dst, src, n + 1);
    return 1;
}

static int parse_int64_amount(const char *s, int64_t *out)
{
    int64_t v = 0;
    size_t i;

    if (*s == '\0') {
        return 0;
    }
    for (i = 0; s[i] != '\0'; ++i) {
        int d;
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
        d = s[i] - '0';
        if (v > (INT64_MAX - d) / 10) {
            return 0;
        }
        v = v * 10 + d;
    }
    *out = v;
    return 1;
}

static int split_csv_simple(char *line, char fields[][MAX_FIELD_LEN], size_t expected)
{
    size_t count = 0;
    char *p = line;

    while (count < expected) {
        char *comma = strchr(p, ',');
        size_t n;
        char *v;

        if (comma != NULL) {
            *comma = '\0';
        }

        v = trim_left(p);
        trim_right(v);
        n = strlen(v);
        if (n >= MAX_FIELD_LEN) {
            return 0;
        }
        memcpy(fields[count], v, n + 1);
        ++count;

        if (comma == NULL) {
            break;
        }
        p = comma + 1;
    }

    return count == expected && strchr(p, ',') == NULL;
}

static int parse_merchant_line(char *line, MerchantRecord *rec)
{
    char f[6][MAX_FIELD_LEN];

    if (!split_csv_simple(line, f, 6)) {
        return 0;
    }
    if (!is_digits(f[0], 12) || !is_digits(f[2], 4)) {
        return 0;
    }
    if (!(strcmp(f[1], "A") == 0 || strcmp(f[1], "S") == 0 || strcmp(f[1], "C") == 0)) {
        return 0;
    }
    if (!(strcmp(f[4], "L") == 0 || strcmp(f[4], "M") == 0 || strcmp(f[4], "H") == 0)) {
        return 0;
    }
    if (!(strcmp(f[5], "D0") == 0 || strcmp(f[5], "D1") == 0 || strcmp(f[5], "M1") == 0)) {
        return 0;
    }
    if (!parse_int64_amount(f[3], &rec->daily_limit_amt)) {
        return 0;
    }
    if (rec->daily_limit_amt <= 0 || rec->daily_limit_amt > 1000000000000LL) {
        return 0;
    }

    return copy_checked(rec->merchant_code, sizeof(rec->merchant_code), f[0]) &&
           copy_checked(rec->merchant_status, sizeof(rec->merchant_status), f[1]) &&
           copy_checked(rec->mcc, sizeof(rec->mcc), f[2]) &&
           copy_checked(rec->risk_rank, sizeof(rec->risk_rank), f[4]) &&
           copy_checked(rec->settle_cycle_kbn, sizeof(rec->settle_cycle_kbn), f[5]);
}

static uint32_t fnv1a32(const void *buf, size_t len)
{
    const unsigned char *p = (const unsigned char *)buf;
    uint32_t h = 2166136261u;
    size_t i;

    for (i = 0; i < len; ++i) {
        h ^= (uint32_t)p[i];
        h *= 16777619u;
    }
    return h;
}

static int currency_minor_digits(const char *currency)
{
    if (strcmp(currency, "JPY") == 0 || strcmp(currency, "KRW") == 0) {
        return 0;
    }
    if (strcmp(currency, "KWD") == 0 || strcmp(currency, "BHD") == 0) {
        return 3;
    }
    return 2;
}

static int base_rounding_unit(const char *mcc, const char *currency)
{
    int code = atoi(mcc);

    if (strcmp(currency, "JPY") == 0) {
        if (code == 5812 || code == 5814 || code == 5411) {
            return 1;
        }
        if (code >= 3000 && code <= 3299) {
            return 10;
        }
        return 1;
    }

    if (strcmp(currency, "KRW") == 0) {
        return 1;
    }
    if (strcmp(currency, "KWD") == 0 || strcmp(currency, "BHD") == 0) {
        return 5;
    }
    return 1;
}

static int mcc_weight(const char *mcc)
{
    int code = atoi(mcc);

    if (code >= 3000 && code <= 3299) {
        return 8;
    }
    if (code == 6011 || code == 6051 || code == 6211) {
        return 10;
    }
    if (code == 5812 || code == 5814 || code == 5411) {
        return 3;
    }
    return 5;
}

static int limit_decimal_is_valid(int64_t amount_minor, int minor_digits, int rounding_unit)
{
    int64_t scale = 1;
    int i;

    for (i = 0; i < minor_digits; ++i) {
        if (scale > INT64_MAX / 10) {
            return 0;
        }
        scale *= 10;
    }

    if (rounding_unit <= 0) {
        return 0;
    }
    return amount_minor >= scale && amount_minor % rounding_unit == 0;
}

static int same_merchant_key(const MerchantRecord *a, const MerchantRecord *b)
{
    return strcmp(a->merchant_code, b->merchant_code) == 0;
}

static int merchant_payload_differs(const MerchantRecord *a, const MerchantRecord *b)
{
    return strcmp(a->merchant_status, b->merchant_status) != 0 ||
           strcmp(a->mcc, b->mcc) != 0 ||
           a->daily_limit_amt != b->daily_limit_amt ||
           strcmp(a->risk_rank, b->risk_rank) != 0 ||
           strcmp(a->settle_cycle_kbn, b->settle_cycle_kbn) != 0;
}

static int add_cache_rows(RateCacheState *st, const MerchantRecord *m)
{
    static const char *currencies[] = { "JPY", "USD", "EUR", "KRW", "KWD" };
    size_t i;

    if (strcmp(m->merchant_status, "A") != 0) {
        return 1;
    }

    for (i = 0; i < sizeof(currencies) / sizeof(currencies[0]); ++i) {
        RateCacheRow *r;
        char hashbuf[96];
        int written;

        if (st->row_count >= MAX_CACHE_ROWS) {
            return 0;
        }

        r = &st->rows[st->row_count];
        if (!copy_checked(r->merchant_code, sizeof(r->merchant_code), m->merchant_code) ||
            !copy_checked(r->currency, sizeof(r->currency), currencies[i])) {
            return 0;
        }

        r->minor_digits = currency_minor_digits(r->currency);
        r->rounding_unit = base_rounding_unit(m->mcc, r->currency);
        r->mcc_weight = mcc_weight(m->mcc);

        if (strcmp(m->risk_rank, "H") == 0 && strcmp(r->currency, "JPY") != 0) {
            r->rounding_unit *= 2;
        }
        if (strcmp(m->settle_cycle_kbn, "D0") == 0 && r->mcc_weight >= 8) {
            r->mcc_weight += 2;
        }

        written = snprintf(hashbuf, sizeof(hashbuf), "%s|%s|%s|%lld|%s|%s|%d|%d",
                           m->merchant_code,
                           m->mcc,
                           r->currency,
                           (long long)m->daily_limit_amt,
                           m->risk_rank,
                           m->settle_cycle_kbn,
                           r->minor_digits,
                           r->rounding_unit);
        if (written < 0 || (size_t)written >= sizeof(hashbuf)) {
            return 0;
        }
        r->cache_hash = fnv1a32(hashbuf, (size_t)written);

        if (!limit_decimal_is_valid(m->daily_limit_amt, r->minor_digits, r->rounding_unit)) {
            ++st->invalid_amount_count;
        }

        ++st->row_count;
    }

    ++st->active_count;
    return 1;
}

static int load_pymerf(const char *path, RateCacheState *st)
{
    FILE *fp = fopen(path, "r");
    char line[MAX_LINE_LEN];
    unsigned long line_no = 0;

    if (fp == NULL) {
        fprintf(stderr, "E1001:PYMERFオープン失敗:%s\n", path);
        return MIPAY_RC_IOERR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        MerchantRecord rec;
        size_t len;

        ++line_no;
        len = strlen(line);
        if (len == sizeof(line) - 1 && line[len - 1] != '\n') {
            fprintf(stderr, "E1002:PYMERF行長超過:%lu\n", line_no);
            fclose(fp);
            return MIPAY_RC_PARSE;
        }

        trim_right(line);
        if (line[0] == '\0') {
            continue;
        }
        if (line_no == 1 && strncmp(line, "MERCHANT-CODE,", 14) == 0) {
            continue;
        }

        if (st->merchant_count >= MAX_MERCHANTS) {
            fprintf(stderr, "E1003:PYMERF件数上限超過:%lu\n", line_no);
            fclose(fp);
            return MIPAY_RC_CAPACITY;
        }

        if (!parse_merchant_line(line, &rec)) {
            fprintf(stderr, "E1004:PYMERF形式不正:%lu\n", line_no);
            fclose(fp);
            return MIPAY_RC_PARSE;
        }

        st->merchants[st->merchant_count++] = rec;
    }

    if (ferror(fp)) {
        fprintf(stderr, "E1005:PYMERF読込失敗:%s\n", path);
        fclose(fp);
        return MIPAY_RC_IOERR;
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "E1006:PYMERFクローズ失敗:%s\n", path);
        return MIPAY_RC_IOERR;
    }

    return MIPAY_RC_OK;
}

static int build_cache(RateCacheState *st)
{
    size_t i;
    size_t j;

    for (i = 0; i < st->merchant_count; ++i) {
        for (j = 0; j < i; ++j) {
            if (same_merchant_key(&st->merchants[i], &st->merchants[j])) {
                if (merchant_payload_differs(&st->merchants[i], &st->merchants[j])) {
                    st->reload_required = 1;
                }
                break;
            }
        }

        if (j != i) {
            continue;
        }

        if (!add_cache_rows(st, &st->merchants[i])) {
            fprintf(stderr, "E1007:レートキャッシュ展開上限超過\n");
            return MIPAY_RC_CAPACITY;
        }
    }

    if (st->invalid_amount_count > 0) {
        st->reload_required = 1;
    }

    return MIPAY_RC_OK;
}

static void print_summary(const RateCacheState *st)
{
    uint32_t h = 2166136261u;
    size_t i;

    for (i = 0; i < st->row_count; ++i) {
        h ^= st->rows[i].cache_hash;
        h *= 16777619u;
    }

    printf("I2001:読込加盟店数=%lu\n", (unsigned long)st->merchant_count);
    printf("I2002:有効加盟店数=%d\n", st->active_count);
    printf("I2003:キャッシュ行数=%lu\n", (unsigned long)st->row_count);
    printf("I2004:小数桁不整合数=%d\n", st->invalid_amount_count);
    printf("I2005:キャッシュ検査値=%08X\n", h);
}

int main(void)
{
    const char *path = getenv("PYMERF_PATH");
    RateCacheState st;
    int rc;

    if (path == NULL || *path == '\0') {
        path = "PYMERF.csv";
    }

    memset(&st, 0, sizeof(st));

    rc = load_pymerf(path, &st);
    if (rc != MIPAY_RC_OK) {
        return rc;
    }

    rc = build_cache(&st);
    if (rc != MIPAY_RC_OK) {
        return rc;
    }

    print_summary(&st);

    if (st.reload_required) {
        fprintf(stderr, "W3001:キャッシュ不一致のため再読込要求\n");
        return MIPAY_RC_RELOAD;
    }

    return MIPAY_RC_OK;
}
