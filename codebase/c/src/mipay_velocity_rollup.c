/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20241218  みらいペイ システム部  日次取引ログのウォレット別速度カウンタ集計を作成
 */

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define RC_NORMAL 0
#define RC_IO_ERR 8
#define RC_PARSE_ERR 12
#define RC_NOMEM_ERR 16

#define MAX_LINE 2048
#define MAX_FIELDS 16
#define WALLET_LEN 64
#define REQ_LEN 64
#define TS_LEN 32

#define PYTXNF_PATH "PYTXNF.csv"
#define PYARSPF_PATH "PYARSPF.csv"
#define PYVELF_PATH "PYVELF.csv"

typedef struct {
    char wallet_id[WALLET_LEN];
    char req_id[REQ_LEN];
    char auth_dt[TS_LEN];
    char status[16];
    int64_t req_amt;
} txn_rec_t;

typedef struct {
    char wallet_id[WALLET_LEN];
    char req_id[REQ_LEN];
    char decision_kbn;
    int64_t req_amt;
    char decline_reason[8];
} arsp_rec_t;

typedef struct {
    char wallet_id[WALLET_LEN];
    char window_start_ts[TS_LEN];
    uint64_t auth_count;
    int64_t auth_sum_amt;
    uint64_t deny_count;
    char last_req_ts[TS_LEN];
} rollup_t;

typedef struct {
    rollup_t *v;
    size_t len;
    size_t cap;
} rollup_vec_t;

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
}

static int copy_text(char *dst, size_t dstsz, const char *src, const char *項目名, unsigned long 行番号)
{
    size_t n = strlen(src);

    if (n == 0U || n >= dstsz) {
        fprintf(stderr, "%s:%lu:%sの桁数が不正です\n", 項目名, 行番号, 項目名);
        return -1;
    }
    memcpy(dst, src, n + 1U);
    return 0;
}

static int parse_csv_line(char *line, char **fields, size_t max_fields, size_t *count)
{
    char *r = line;
    char *w = line;
    bool in_quote = false;
    size_t n = 0U;

    if (max_fields == 0U) {
        return -1;
    }

    fields[n++] = w;
    while (*r != '\0') {
        unsigned char c = (unsigned char)*r++;

        if (c == '"') {
            if (in_quote && *r == '"') {
                *w++ = '"';
                r++;
            } else {
                in_quote = !in_quote;
            }
        } else if (c == ',' && !in_quote) {
            *w++ = '\0';
            if (n == max_fields) {
                return -1;
            }
            fields[n++] = w;
        } else if (c == '\n' || c == '\r') {
            if (*r == '\n' && c == '\r') {
                r++;
            }
            break;
        } else {
            *w++ = (char)c;
        }
    }

    if (in_quote) {
        return -1;
    }

    *w = '\0';
    for (size_t i = 0U; i < n; i++) {
        trim_field(fields[i]);
    }
    *count = n;
    return 0;
}

static int parse_amount(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (*s == '\0' || *s == '-') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0' || v < 0) {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static bool looks_header(char **fields, size_t n)
{
    if (n == 0U) {
        return false;
    }
    return strcmp(fields[0], "TXN-ID") == 0 ||
           strcmp(fields[0], "REQ-ID") == 0 ||
           strcmp(fields[0], "WALLET-ID") == 0;
}

static bool is_approved_status(const char *s)
{
    return strcmp(s, "A") == 0 ||
           strcmp(s, "AUTH") == 0 ||
           strcmp(s, "APPROVED") == 0 ||
           strcmp(s, "30") == 0;
}

static int cmp_ts(const char *a, const char *b)
{
    if (a[0] == '\0' && b[0] == '\0') {
        return 0;
    }
    if (a[0] == '\0') {
        return -1;
    }
    if (b[0] == '\0') {
        return 1;
    }
    return strcmp(a, b);
}

static int make_window_start(const char *ts, char *out, size_t outsz)
{
    size_t n = strlen(ts);

    if (n < 16U || outsz <= n) {
        return -1;
    }

    memcpy(out, ts, n + 1U);
    if (n >= 19U && out[16] == ':' && out[13] == ':') {
        out[17] = '0';
        out[18] = '0';
    }
    return 0;
}

static rollup_t *find_rollup(rollup_vec_t *vec, const char *wallet_id)
{
    for (size_t i = 0U; i < vec->len; i++) {
        if (strcmp(vec->v[i].wallet_id, wallet_id) == 0) {
            return &vec->v[i];
        }
    }
    return NULL;
}

static rollup_t *get_rollup(rollup_vec_t *vec, const char *wallet_id, const char *ts)
{
    rollup_t *r = find_rollup(vec, wallet_id);

    if (r != NULL) {
        return r;
    }

    if (vec->len == vec->cap) {
        size_t next = vec->cap == 0U ? 128U : vec->cap * 2U;
        rollup_t *nv;

        if (next < vec->cap) {
            return NULL;
        }
        nv = realloc(vec->v, next * sizeof(*nv));
        if (nv == NULL) {
            return NULL;
        }
        vec->v = nv;
        vec->cap = next;
    }

    r = &vec->v[vec->len++];
    memset(r, 0, sizeof(*r));
    if (copy_text(r->wallet_id, sizeof(r->wallet_id), wallet_id, "WALLET-ID", 0UL) != 0) {
        return NULL;
    }
    if (make_window_start(ts, r->window_start_ts, sizeof(r->window_start_ts)) != 0) {
        return NULL;
    }
    return r;
}

static int add_auth(rollup_vec_t *vec, const txn_rec_t *rec)
{
    rollup_t *r = get_rollup(vec, rec->wallet_id, rec->auth_dt);

    if (r == NULL) {
        return -1;
    }
    if (UINT64_MAX - r->auth_count < 1U || INT64_MAX - r->auth_sum_amt < rec->req_amt) {
        return -1;
    }

    r->auth_count++;
    r->auth_sum_amt += rec->req_amt;
    if (cmp_ts(r->last_req_ts, rec->auth_dt) < 0) {
        memcpy(r->last_req_ts, rec->auth_dt, strlen(rec->auth_dt) + 1U);
    }
    if (cmp_ts(rec->auth_dt, r->window_start_ts) < 0) {
        if (make_window_start(rec->auth_dt, r->window_start_ts, sizeof(r->window_start_ts)) != 0) {
            return -1;
        }
    }
    return 0;
}

static int add_deny(rollup_vec_t *vec, const arsp_rec_t *rec)
{
    rollup_t *r = get_rollup(vec, rec->wallet_id, "0000-00-00 00:00:00");

    (void)rec->req_id;
    (void)rec->req_amt;
    (void)rec->decline_reason;

    if (r == NULL) {
        return -1;
    }
    if (UINT64_MAX - r->deny_count < 1U) {
        return -1;
    }

    r->deny_count++;
    return 0;
}

static int read_pytxnf(rollup_vec_t *vec)
{
    FILE *fp = fopen(PYTXNF_PATH, "r");
    char line[MAX_LINE];
    unsigned long 行番号 = 0UL;

    if (fp == NULL) {
        perror("PYTXNFを開けません");
        return RC_IO_ERR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *fields[MAX_FIELDS];
        size_t n = 0U;
        txn_rec_t rec;

        行番号++;
        if (parse_csv_line(line, fields, MAX_FIELDS, &n) != 0) {
            fprintf(stderr, "PYTXNF:%lu:CSV形式が不正です\n", 行番号);
            fclose(fp);
            return RC_PARSE_ERR;
        }
        if (looks_header(fields, n) || (n == 1U && fields[0][0] == '\0')) {
            continue;
        }
        if (n != 8U) {
            fprintf(stderr, "PYTXNF:%lu:項目数が不正です\n", 行番号);
            fclose(fp);
            return RC_PARSE_ERR;
        }

        memset(&rec, 0, sizeof(rec));
        if (copy_text(rec.req_id, sizeof(rec.req_id), fields[1], "REQ-ID", 行番号) != 0 ||
            copy_text(rec.wallet_id, sizeof(rec.wallet_id), fields[2], "WALLET-ID", 行番号) != 0 ||
            copy_text(rec.status, sizeof(rec.status), fields[5], "TXN-STATUS", 行番号) != 0 ||
            copy_text(rec.auth_dt, sizeof(rec.auth_dt), fields[6], "AUTH-DT", 行番号) != 0 ||
            parse_amount(fields[4], &rec.req_amt) != 0) {
            fprintf(stderr, "PYTXNF:%lu:取引レコードを解釈できません\n", 行番号);
            fclose(fp);
            return RC_PARSE_ERR;
        }

        if (is_approved_status(rec.status) && add_auth(vec, &rec) != 0) {
            fprintf(stderr, "PYTXNF:%lu:承認集計で桁あふれまたは領域不足です\n", 行番号);
            fclose(fp);
            return RC_NOMEM_ERR;
        }
    }

    if (ferror(fp)) {
        perror("PYTXNF読込で障害が発生しました");
        fclose(fp);
        return RC_IO_ERR;
    }

    fclose(fp);
    return RC_NORMAL;
}

static bool valid_decline_reason(const char *s)
{
    return strcmp(s, "LIM") == 0 || strcmp(s, "STS") == 0 || strcmp(s, "CUR") == 0;
}

static int read_pyarspf(rollup_vec_t *vec)
{
    FILE *fp = fopen(PYARSPF_PATH, "r");
    char line[MAX_LINE];
    unsigned long 行番号 = 0UL;

    if (fp == NULL) {
        perror("PYARSPFを開けません");
        return RC_IO_ERR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *fields[MAX_FIELDS];
        size_t n = 0U;
        arsp_rec_t rec;

        行番号++;
        if (parse_csv_line(line, fields, MAX_FIELDS, &n) != 0) {
            fprintf(stderr, "PYARSPF:%lu:CSV形式が不正です\n", 行番号);
            fclose(fp);
            return RC_PARSE_ERR;
        }
        if (looks_header(fields, n) || (n == 1U && fields[0][0] == '\0')) {
            continue;
        }
        if (n != 6U) {
            fprintf(stderr, "PYARSPF:%lu:項目数が不正です\n", 行番号);
            fclose(fp);
            return RC_PARSE_ERR;
        }

        memset(&rec, 0, sizeof(rec));
        if (copy_text(rec.req_id, sizeof(rec.req_id), fields[0], "REQ-ID", 行番号) != 0 ||
            copy_text(rec.wallet_id, sizeof(rec.wallet_id), fields[1], "WALLET-ID", 行番号) != 0 ||
            copy_text(rec.decline_reason, sizeof(rec.decline_reason), fields[5], "AR-DECLINE-REASON", 行番号) != 0 ||
            parse_amount(fields[4], &rec.req_amt) != 0 ||
            strlen(fields[2]) != 1U) {
            fprintf(stderr, "PYARSPF:%lu:応答レコードを解釈できません\n", 行番号);
            fclose(fp);
            return RC_PARSE_ERR;
        }

        rec.decision_kbn = fields[2][0];
        if (rec.decision_kbn != 'A' && rec.decision_kbn != 'D') {
            fprintf(stderr, "PYARSPF:%lu:AR-DECISION-KBNが不正です\n", 行番号);
            fclose(fp);
            return RC_PARSE_ERR;
        }
        if (rec.decision_kbn == 'D' && !valid_decline_reason(rec.decline_reason)) {
            fprintf(stderr, "PYARSPF:%lu:AR-DECLINE-REASONが不正です\n", 行番号);
            fclose(fp);
            return RC_PARSE_ERR;
        }

        if (rec.decision_kbn == 'D' && add_deny(vec, &rec) != 0) {
            fprintf(stderr, "PYARSPF:%lu:否決集計で桁あふれまたは領域不足です\n", 行番号);
            fclose(fp);
            return RC_NOMEM_ERR;
        }
    }

    if (ferror(fp)) {
        perror("PYARSPF読込で障害が発生しました");
        fclose(fp);
        return RC_IO_ERR;
    }

    fclose(fp);
    return RC_NORMAL;
}

static int cmp_rollup(const void *a, const void *b)
{
    const rollup_t *ra = (const rollup_t *)a;
    const rollup_t *rb = (const rollup_t *)b;

    return strcmp(ra->wallet_id, rb->wallet_id);
}

static int write_pyvelf(const rollup_vec_t *vec)
{
    FILE *fp = fopen(PYVELF_PATH, "w");

    if (fp == NULL) {
        perror("PYVELFを開けません");
        return RC_IO_ERR;
    }

    if (fprintf(fp, "WALLET-ID,WINDOW-START-TS,AUTH-COUNT,AUTH-SUM-AMT,DENY-COUNT,LAST-REQ-TS\n") < 0) {
        perror("PYVELF見出しを書けません");
        fclose(fp);
        return RC_IO_ERR;
    }

    for (size_t i = 0U; i < vec->len; i++) {
        const rollup_t *r = &vec->v[i];
        const char *last_ts = r->last_req_ts[0] == '\0' ? r->window_start_ts : r->last_req_ts;

        if (fprintf(fp, "%s,%s,%" PRIu64 ",%" PRId64 ",%" PRIu64 ",%s\n",
                    r->wallet_id,
                    r->window_start_ts,
                    r->auth_count,
                    r->auth_sum_amt,
                    r->deny_count,
                    last_ts) < 0) {
            perror("PYVELF明細を書けません");
            fclose(fp);
            return RC_IO_ERR;
        }
    }

    if (fclose(fp) != 0) {
        perror("PYVELFを閉じられません");
        return RC_IO_ERR;
    }

    return RC_NORMAL;
}

int main(void)
{
    rollup_vec_t vec;
    int rc;

    memset(&vec, 0, sizeof(vec));

    rc = read_pytxnf(&vec);
    if (rc != RC_NORMAL) {
        free(vec.v);
        return rc;
    }

    rc = read_pyarspf(&vec);
    if (rc != RC_NORMAL) {
        free(vec.v);
        return rc;
    }

    qsort(vec.v, vec.len, sizeof(vec.v[0]), cmp_rollup);

    rc = write_pyvelf(&vec);
    free(vec.v);
    if (rc != RC_NORMAL) {
        return rc;
    }

    return RC_NORMAL;
}
