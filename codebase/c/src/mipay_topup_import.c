/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240829  みらいペイ システム部  初版作成
 */

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MI_OK 0
#define MI_ERR_IO 12
#define MI_ERR_PARSE 16
#define MI_ERR_NOMEM 20

#define MI_LINE_MAX 2048
#define MI_FIELD_MAX 128
#define MI_ID_MAX 32
#define MI_WALLET_MAX 32
#define MI_METHOD_MAX 24
#define MI_STATUS_MAX 32
#define MI_TS_MAX 32

typedef struct {
    char topup_id[MI_ID_MAX];
    char wallet_id[MI_WALLET_MAX];
    int64_t topup_amt;
    char payment_method[MI_METHOD_MAX];
    char topup_status[MI_STATUS_MAX];
    char request_ts[MI_TS_MAX];
    unsigned long line_no;
} PytopfRecord;

typedef struct {
    PytopfRecord *v;
    size_t n;
    size_t cap;
} RecordVec;

static void trim_ascii(char *s)
{
    size_t b = 0;
    size_t e = strlen(s);

    while (s[b] != '\0' && isspace((unsigned char)s[b])) {
        b++;
    }
    while (e > b && isspace((unsigned char)s[e - 1])) {
        e--;
    }
    if (b > 0) {
        memmove(s, s + b, e - b);
    }
    s[e - b] = '\0';
}

static int copy_field(char *dst, size_t dstsz, const char *src, unsigned long line_no, const char *name)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dstsz) {
        fprintf(stderr, "%lu行目:%s不正\n", line_no, name);
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_amount(const char *s, int64_t *out)
{
    int64_t v = 0;
    const unsigned char *p = (const unsigned char *)s;

    if (*p == '\0') {
        return -1;
    }
    while (*p != '\0') {
        int d;
        if (!isdigit(*p)) {
            return -1;
        }
        d = *p - '0';
        if (v > (INT64_MAX - d) / 10) {
            return -1;
        }
        v = v * 10 + d;
        p++;
    }
    if (v <= 0 || v > 1000000000000LL) {
        return -1;
    }
    *out = v;
    return 0;
}

static int valid_id_text(const char *s)
{
    size_t i;
    size_t n = strlen(s);

    if (n == 0) {
        return 0;
    }
    for (i = 0; i < n; i++) {
        unsigned char c = (unsigned char)s[i];
        if (!(isalnum(c) || c == '-' || c == '_')) {
            return 0;
        }
    }
    return 1;
}

static int valid_ts14(const char *s)
{
    size_t i;

    if (strlen(s) != 14) {
        return 0;
    }
    for (i = 0; i < 14; i++) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
    }
    if (memcmp(s + 4, "00", 2) <= 0 || memcmp(s + 4, "12", 2) > 0) {
        return 0;
    }
    if (memcmp(s + 6, "00", 2) <= 0 || memcmp(s + 6, "31", 2) > 0) {
        return 0;
    }
    if (memcmp(s + 8, "23", 2) > 0) {
        return 0;
    }
    if (memcmp(s + 10, "59", 2) > 0 || memcmp(s + 12, "59", 2) > 0) {
        return 0;
    }
    return 1;
}

static int split_csv6(char *line, char fields[6][MI_FIELD_MAX], unsigned long line_no)
{
    int col = 0;
    char *p = line;
    char *start = p;

    while (1) {
        if (*p == ',' || *p == '\0') {
            size_t len = (size_t)(p - start);
            if (col >= 6 || len >= MI_FIELD_MAX) {
                fprintf(stderr, "%lu行目:項目数不正\n", line_no);
                return -1;
            }
            memcpy(fields[col], start, len);
            fields[col][len] = '\0';
            trim_ascii(fields[col]);
            col++;
            if (*p == '\0') {
                break;
            }
            start = p + 1;
        }
        p++;
    }
    if (col != 6) {
        fprintf(stderr, "%lu行目:項目数不正\n", line_no);
        return -1;
    }
    return 0;
}

static int parse_record(char *line, unsigned long line_no, PytopfRecord *r)
{
    char f[6][MI_FIELD_MAX];

    if (split_csv6(line, f, line_no) != 0) {
        return -1;
    }
    if (copy_field(r->topup_id, sizeof(r->topup_id), f[0], line_no, "TOPUP-ID") != 0 ||
        copy_field(r->wallet_id, sizeof(r->wallet_id), f[1], line_no, "WALLET-ID") != 0 ||
        copy_field(r->payment_method, sizeof(r->payment_method), f[3], line_no, "PAYMENT-METHOD") != 0 ||
        copy_field(r->topup_status, sizeof(r->topup_status), f[4], line_no, "TOPUP-STATUS") != 0 ||
        copy_field(r->request_ts, sizeof(r->request_ts), f[5], line_no, "REQUEST-TS") != 0) {
        return -1;
    }
    if (!valid_id_text(r->topup_id) || !valid_id_text(r->wallet_id)) {
        fprintf(stderr, "%lu行目:ID文字種不正\n", line_no);
        return -1;
    }
    if (parse_amount(f[2], &r->topup_amt) != 0) {
        fprintf(stderr, "%lu行目:金額不正\n", line_no);
        return -1;
    }
    if (strcmp(r->payment_method, "BANK") != 0 && strcmp(r->payment_method, "CCHARGE") != 0) {
        fprintf(stderr, "%lu行目:入金手段不正\n", line_no);
        return -1;
    }
    if (!valid_ts14(r->request_ts)) {
        fprintf(stderr, "%lu行目:受付時刻不正\n", line_no);
        return -1;
    }
    r->line_no = line_no;
    return 0;
}

static int vec_push(RecordVec *vec, const PytopfRecord *r)
{
    PytopfRecord *nv;
    size_t nc;

    if (vec->n == vec->cap) {
        nc = vec->cap == 0 ? 256u : vec->cap * 2u;
        if (nc < vec->cap || nc > SIZE_MAX / sizeof(*vec->v)) {
            return -1;
        }
        nv = (PytopfRecord *)realloc(vec->v, nc * sizeof(*vec->v));
        if (nv == NULL) {
            return -1;
        }
        vec->v = nv;
        vec->cap = nc;
    }
    vec->v[vec->n++] = *r;
    return 0;
}

static const PytopfRecord *find_prior(const RecordVec *vec, const PytopfRecord *r)
{
    size_t i;

    for (i = 0; i < vec->n; i++) {
        if (strcmp(vec->v[i].topup_id, r->topup_id) == 0) {
            return &vec->v[i];
        }
    }
    return NULL;
}

static const char *candidate_status(const PytopfRecord *r, const PytopfRecord *prior)
{
    if (prior == NULL) {
        if (strcmp(r->topup_status, "REQUESTED") == 0 ||
            strcmp(r->topup_status, "BANK_OK") == 0 ||
            strcmp(r->topup_status, "PENDING") == 0) {
            return "TORIKOMI_KOHO";
        }
        return "STATUS_KAKUNIN";
    }

    if (strcmp(prior->wallet_id, r->wallet_id) != 0 ||
        prior->topup_amt != r->topup_amt ||
        strcmp(prior->payment_method, r->payment_method) != 0) {
        return "KINGAKU_FUICHI";
    }
    return "NIJYU_NYUKIN";
}

static int read_all(RecordVec *seen)
{
    char line[MI_LINE_MAX];
    unsigned long line_no = 0;

    while (fgets(line, sizeof(line), stdin) != NULL) {
        PytopfRecord r;
        const PytopfRecord *prior;
        const char *st;
        size_t len;

        line_no++;
        len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[--len] = '\0';
        }
        if (len > 0 && line[len - 1] == '\r') {
            line[--len] = '\0';
        }
        if (len == 0) {
            continue;
        }
        if (line_no == 1 && strncmp(line, "TOPUP-ID,", 9) == 0) {
            continue;
        }

        if (parse_record(line, line_no, &r) != 0) {
            return MI_ERR_PARSE;
        }

        prior = find_prior(seen, &r);
        st = candidate_status(&r, prior);

        if (printf("%s,%s,%lld,%s,%s,%s\n",
                   r.topup_id,
                   r.wallet_id,
                   (long long)r.topup_amt,
                   r.payment_method,
                   st,
                   r.request_ts) < 0) {
            fprintf(stderr, "%lu行目:出力失敗\n", line_no);
            return MI_ERR_IO;
        }

        if (vec_push(seen, &r) != 0) {
            fprintf(stderr, "%lu行目:作業領域不足\n", line_no);
            return MI_ERR_NOMEM;
        }
    }

    if (ferror(stdin)) {
        fprintf(stderr, "入力読込失敗\n");
        return MI_ERR_IO;
    }
    if (fflush(stdout) != 0) {
        fprintf(stderr, "出力確定失敗\n");
        return MI_ERR_IO;
    }
    return MI_OK;
}

int main(void)
{
    RecordVec seen;
    int rc;

    seen.v = NULL;
    seen.n = 0;
    seen.cap = 0;

    rc = read_all(&seen);
    free(seen.v);

    return rc;
}
