/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240520  決済基盤  初版作成
 * 1.01  20241021  決済基盤  加盟店保留分離と金額検証を追加
 */

#include "mipay_settle.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIPAY_MAX_TXN       200000
#define MIPAY_MAX_ADJ        50000
#define MIPAY_MAX_MERCHANT   20000
#define MIPAY_MAX_LINE      250000
#define MIPAY_FIELD_MAX        256
#define MIPAY_LINE_MAX        1024

#define MIPAY_RC_NORMAL          0
#define MIPAY_RC_IOERR          20
#define MIPAY_RC_PARSEERR       21
#define MIPAY_RC_LIMITERR       22
#define MIPAY_RC_OVERFLOW       23

#define MIPAY_CHARGE_BP         30
#define MIPAY_BP_DENOM       10000

typedef struct {
    char txn_id[33];
    char merchant_code[17];
    char txn_kbn;
    int64_t txn_amt;
    char txn_dt[9];
} prep_txn_t;

typedef struct {
    char adjust_id[33];
    char merchant_code[17];
    char adjust_kbn;
    int64_t adjust_amt;
    char reason_cd[9];
    char apply_dt[9];
    char approval_status[3];
} prep_adj_t;

typedef struct {
    char merchant_code[17];
    char merchant_name[81];
    char mer_status[3];
    char bank_acct_no[33];
} prep_merchant_t;

typedef struct {
    char detail_id[33];
    char settle_id[33];
    char merchant_code[17];
    char txn_id[33];
    int64_t txn_amt;
    int64_t charge_amt;
    char line_kbn;
    int seq;
} prep_detail_t;

typedef struct {
    prep_txn_t *txns;
    size_t txn_count;
    prep_adj_t *adjs;
    size_t adj_count;
    prep_merchant_t *merchants;
    size_t merchant_count;
    prep_detail_t *details;
    size_t detail_count;
} work_t;

static int read_csv_field(char **cur, char *out, size_t outsz)
{
    char *p = *cur;
    size_t n = 0;
    int quoted = 0;

    if (outsz == 0) {
        return -1;
    }

    if (*p == '"') {
        quoted = 1;
        ++p;
        while (*p != '\0') {
            if (*p == '"' && p[1] == '"') {
                if (n + 1 >= outsz) {
                    return -1;
                }
                out[n++] = '"';
                p += 2;
                continue;
            }
            if (*p == '"') {
                ++p;
                break;
            }
            if (n + 1 >= outsz) {
                return -1;
            }
            out[n++] = *p++;
        }
        if (*p != ',' && *p != '\0' && *p != '\n' && *p != '\r') {
            return -1;
        }
    } else {
        while (*p != ',' && *p != '\0' && *p != '\n' && *p != '\r') {
            if (n + 1 >= outsz) {
                return -1;
            }
            out[n++] = *p++;
        }
    }

    out[n] = '\0';

    if (*p == ',') {
        ++p;
    } else if (quoted && *p != '\0' && *p != '\n' && *p != '\r') {
        return -1;
    }

    *cur = p;
    return 0;
}

static int parse_i64(const char *s, int64_t *v)
{
    char *end = NULL;
    long long tmp;

    errno = 0;
    tmp = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }
    *v = (int64_t)tmp;
    return 0;
}

static int valid_yyyymmdd(const char *s)
{
    int i;

    if (strlen(s) != 8) {
        return 0;
    }
    for (i = 0; i < 8; ++i) {
        if (s[i] < '0' || s[i] > '9') {
            return 0;
        }
    }
    return 1;
}

static int copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int is_settleable(const prep_merchant_t *m)
{
    return strcmp(m->mer_status, "01") == 0;
}

static const prep_merchant_t *find_merchant(const work_t *w, const char *merchant_code)
{
    size_t i;

    for (i = 0; i < w->merchant_count; ++i) {
        if (strcmp(w->merchants[i].merchant_code, merchant_code) == 0) {
            return &w->merchants[i];
        }
    }
    return NULL;
}

static int cmp_txn(const void *a, const void *b)
{
    const prep_txn_t *x = (const prep_txn_t *)a;
    const prep_txn_t *y = (const prep_txn_t *)b;
    int c;

    c = strcmp(x->merchant_code, y->merchant_code);
    if (c != 0) {
        return c;
    }
    if (x->txn_kbn != y->txn_kbn) {
        return (x->txn_kbn < y->txn_kbn) ? -1 : 1;
    }
    c = strcmp(x->txn_dt, y->txn_dt);
    if (c != 0) {
        return c;
    }
    return strcmp(x->txn_id, y->txn_id);
}

static int cmp_adj(const void *a, const void *b)
{
    const prep_adj_t *x = (const prep_adj_t *)a;
    const prep_adj_t *y = (const prep_adj_t *)b;
    int c;

    c = strcmp(x->merchant_code, y->merchant_code);
    if (c != 0) {
        return c;
    }
    if (x->adjust_kbn != y->adjust_kbn) {
        return (x->adjust_kbn < y->adjust_kbn) ? -1 : 1;
    }
    c = strcmp(x->apply_dt, y->apply_dt);
    if (c != 0) {
        return c;
    }
    return strcmp(x->adjust_id, y->adjust_id);
}

static int charge_amount(int64_t amt, int64_t *charge)
{
    int64_t abs_amt;
    int64_t q;

    if (amt == INT64_MIN) {
        return -1;
    }
    abs_amt = (amt < 0) ? -amt : amt;
    if (abs_amt > INT64_MAX / MIPAY_CHARGE_BP) {
        return -1;
    }
    q = (abs_amt * MIPAY_CHARGE_BP + MIPAY_BP_DENOM - 1) / MIPAY_BP_DENOM;
    *charge = (amt < 0) ? -q : q;
    return 0;
}

static int make_detail_id(char *dst, size_t dstsz, int seq)
{
    int n = snprintf(dst, dstsz, "DT%014d", seq);
    return (n > 0 && (size_t)n < dstsz) ? 0 : -1;
}

static int make_settle_id(char *dst, size_t dstsz, const char *merchant_code, const char *date8)
{
    int n = snprintf(dst, dstsz, "ST%.16s-%.8s", merchant_code, date8);
    return (n > 0 && (size_t)n < dstsz) ? 0 : -1;
}

static int read_pstxnf(const char *path, work_t *w)
{
    FILE *fp = fopen(path, "r");
    char line[MIPAY_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "E1001 PSTXNFを開けません: %s\n", path);
        return MIPAY_RC_IOERR;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *p = line;
        char f[5][MIPAY_FIELD_MAX];
        prep_txn_t *r;

        if (w->txn_count >= MIPAY_MAX_TXN) {
            fclose(fp);
            fprintf(stderr, "E1002 PSTXNF件数上限超過\n");
            return MIPAY_RC_LIMITERR;
        }
        if (read_csv_field(&p, f[0], sizeof f[0]) != 0 ||
            read_csv_field(&p, f[1], sizeof f[1]) != 0 ||
            read_csv_field(&p, f[2], sizeof f[2]) != 0 ||
            read_csv_field(&p, f[3], sizeof f[3]) != 0 ||
            read_csv_field(&p, f[4], sizeof f[4]) != 0) {
            fclose(fp);
            fprintf(stderr, "E1003 PSTXNF項目解析失敗\n");
            return MIPAY_RC_PARSEERR;
        }
        r = &w->txns[w->txn_count];
        if (copy_field(r->txn_id, sizeof r->txn_id, f[0]) != 0 ||
            copy_field(r->merchant_code, sizeof r->merchant_code, f[1]) != 0 ||
            strlen(f[2]) != 1 ||
            (f[2][0] != 'C' && f[2][0] != 'R') ||
            parse_i64(f[3], &r->txn_amt) != 0 ||
            !valid_yyyymmdd(f[4]) ||
            copy_field(r->txn_dt, sizeof r->txn_dt, f[4]) != 0) {
            fclose(fp);
            fprintf(stderr, "E1004 PSTXNF値検証失敗\n");
            return MIPAY_RC_PARSEERR;
        }
        r->txn_kbn = f[2][0];
        if (r->txn_kbn == 'C' && r->txn_amt <= 0) {
            fclose(fp);
            fprintf(stderr, "E1005 売上金額不正\n");
            return MIPAY_RC_PARSEERR;
        }
        if (r->txn_kbn == 'R' && r->txn_amt >= 0) {
            fclose(fp);
            fprintf(stderr, "E1006 返金金額不正\n");
            return MIPAY_RC_PARSEERR;
        }
        ++w->txn_count;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "E1007 PSTXNF読込失敗\n");
        return MIPAY_RC_IOERR;
    }
    fclose(fp);
    return MIPAY_RC_NORMAL;
}

static int read_psadjf(const char *path, work_t *w)
{
    FILE *fp = fopen(path, "r");
    char line[MIPAY_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "E2001 PSADJFを開けません: %s\n", path);
        return MIPAY_RC_IOERR;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *p = line;
        char f[7][MIPAY_FIELD_MAX];
        prep_adj_t *r;

        if (w->adj_count >= MIPAY_MAX_ADJ) {
            fclose(fp);
            fprintf(stderr, "E2002 PSADJF件数上限超過\n");
            return MIPAY_RC_LIMITERR;
        }
        if (read_csv_field(&p, f[0], sizeof f[0]) != 0 ||
            read_csv_field(&p, f[1], sizeof f[1]) != 0 ||
            read_csv_field(&p, f[2], sizeof f[2]) != 0 ||
            read_csv_field(&p, f[3], sizeof f[3]) != 0 ||
            read_csv_field(&p, f[4], sizeof f[4]) != 0 ||
            read_csv_field(&p, f[5], sizeof f[5]) != 0 ||
            read_csv_field(&p, f[6], sizeof f[6]) != 0) {
            fclose(fp);
            fprintf(stderr, "E2003 PSADJF項目解析失敗\n");
            return MIPAY_RC_PARSEERR;
        }
        r = &w->adjs[w->adj_count];
        if (copy_field(r->adjust_id, sizeof r->adjust_id, f[0]) != 0 ||
            copy_field(r->merchant_code, sizeof r->merchant_code, f[1]) != 0 ||
            strlen(f[2]) != 1 ||
            parse_i64(f[3], &r->adjust_amt) != 0 ||
            copy_field(r->reason_cd, sizeof r->reason_cd, f[4]) != 0 ||
            !valid_yyyymmdd(f[5]) ||
            copy_field(r->apply_dt, sizeof r->apply_dt, f[5]) != 0 ||
            copy_field(r->approval_status, sizeof r->approval_status, f[6]) != 0) {
            fclose(fp);
            fprintf(stderr, "E2004 PSADJF値検証失敗\n");
            return MIPAY_RC_PARSEERR;
        }
        r->adjust_kbn = f[2][0];
        if (r->adjust_amt == 0) {
            fclose(fp);
            fprintf(stderr, "E2005 調整金額不正\n");
            return MIPAY_RC_PARSEERR;
        }
        ++w->adj_count;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "E2006 PSADJF読込失敗\n");
        return MIPAY_RC_IOERR;
    }
    fclose(fp);
    return MIPAY_RC_NORMAL;
}

static int read_psmerf(const char *path, work_t *w)
{
    FILE *fp = fopen(path, "r");
    char line[MIPAY_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "E3001 PSMERFを開けません: %s\n", path);
        return MIPAY_RC_IOERR;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *p = line;
        char f[4][MIPAY_FIELD_MAX];
        prep_merchant_t *r;

        if (w->merchant_count >= MIPAY_MAX_MERCHANT) {
            fclose(fp);
            fprintf(stderr, "E3002 PSMERF件数上限超過\n");
            return MIPAY_RC_LIMITERR;
        }
        if (read_csv_field(&p, f[0], sizeof f[0]) != 0 ||
            read_csv_field(&p, f[1], sizeof f[1]) != 0 ||
            read_csv_field(&p, f[2], sizeof f[2]) != 0 ||
            read_csv_field(&p, f[3], sizeof f[3]) != 0) {
            fclose(fp);
            fprintf(stderr, "E3003 PSMERF項目解析失敗\n");
            return MIPAY_RC_PARSEERR;
        }
        r = &w->merchants[w->merchant_count];
        if (copy_field(r->merchant_code, sizeof r->merchant_code, f[0]) != 0 ||
            copy_field(r->merchant_name, sizeof r->merchant_name, f[1]) != 0 ||
            copy_field(r->mer_status, sizeof r->mer_status, f[2]) != 0 ||
            copy_field(r->bank_acct_no, sizeof r->bank_acct_no, f[3]) != 0) {
            fclose(fp);
            fprintf(stderr, "E3004 PSMERF値検証失敗\n");
            return MIPAY_RC_PARSEERR;
        }
        if (strcmp(r->mer_status, "01") != 0 &&
            strcmp(r->mer_status, "02") != 0 &&
            strcmp(r->mer_status, "09") != 0) {
            fclose(fp);
            fprintf(stderr, "E3005 加盟店状態不正\n");
            return MIPAY_RC_PARSEERR;
        }
        ++w->merchant_count;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "E3006 PSMERF読込失敗\n");
        return MIPAY_RC_IOERR;
    }
    fclose(fp);
    return MIPAY_RC_NORMAL;
}

static int append_detail(work_t *w, const char *merchant_code, const char *src_id,
                         int64_t amt, char line_kbn, const char *date8)
{
    prep_detail_t *d;
    int64_t charge;

    if (w->detail_count >= MIPAY_MAX_LINE) {
        fprintf(stderr, "E4001 PSDTLF件数上限超過\n");
        return MIPAY_RC_LIMITERR;
    }
    if (charge_amount(amt, &charge) != 0) {
        fprintf(stderr, "E4002 手数料計算桁あふれ\n");
        return MIPAY_RC_OVERFLOW;
    }

    d = &w->details[w->detail_count];
    d->seq = (int)w->detail_count + 1;
    if (make_detail_id(d->detail_id, sizeof d->detail_id, d->seq) != 0 ||
        make_settle_id(d->settle_id, sizeof d->settle_id, merchant_code, date8) != 0 ||
        copy_field(d->merchant_code, sizeof d->merchant_code, merchant_code) != 0 ||
        copy_field(d->txn_id, sizeof d->txn_id, src_id) != 0) {
        fprintf(stderr, "E4003 PSDTLFキー生成失敗\n");
        return MIPAY_RC_PARSEERR;
    }
    d->txn_amt = amt;
    d->charge_amt = charge;
    d->line_kbn = line_kbn;
    ++w->detail_count;
    return MIPAY_RC_NORMAL;
}

static int build_details(work_t *w)
{
    size_t i;
    int rc;

    qsort(w->txns, w->txn_count, sizeof w->txns[0], cmp_txn);
    qsort(w->adjs, w->adj_count, sizeof w->adjs[0], cmp_adj);

    for (i = 0; i < w->txn_count; ++i) {
        const prep_merchant_t *m = find_merchant(w, w->txns[i].merchant_code);
        char line_kbn;

        if (m == NULL) {
            fprintf(stderr, "E5001 加盟店未登録: %s\n", w->txns[i].merchant_code);
            return MIPAY_RC_PARSEERR;
        }

        line_kbn = is_settleable(m) ? w->txns[i].txn_kbn : 'H';
        rc = append_detail(w, w->txns[i].merchant_code, w->txns[i].txn_id,
                           w->txns[i].txn_amt, line_kbn, w->txns[i].txn_dt);
        if (rc != MIPAY_RC_NORMAL) {
            return rc;
        }
    }

    for (i = 0; i < w->adj_count; ++i) {
        const prep_merchant_t *m = find_merchant(w, w->adjs[i].merchant_code);
        char line_kbn;

        if (m == NULL) {
            fprintf(stderr, "E5002 調整先加盟店未登録: %s\n", w->adjs[i].merchant_code);
            return MIPAY_RC_PARSEERR;
        }
        if (strcmp(w->adjs[i].approval_status, "01") != 0) {
            continue;
        }

        line_kbn = is_settleable(m) ? 'A' : 'H';
        rc = append_detail(w, w->adjs[i].merchant_code, w->adjs[i].adjust_id,
                           w->adjs[i].adjust_amt, line_kbn, w->adjs[i].apply_dt);
        if (rc != MIPAY_RC_NORMAL) {
            return rc;
        }
    }

    return MIPAY_RC_NORMAL;
}

static int write_psdtlf(const char *path, const work_t *w)
{
    FILE *fp = fopen(path, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "E6001 PSDTLFを作成できません: %s\n", path);
        return MIPAY_RC_IOERR;
    }

    for (i = 0; i < w->detail_count; ++i) {
        const prep_detail_t *d = &w->details[i];
        if (fprintf(fp, "%s,%s,%s,%s,%" PRId64 ",%" PRId64 ",%c\n",
                    d->detail_id, d->settle_id, d->merchant_code, d->txn_id,
                    d->txn_amt, d->charge_amt, d->line_kbn) < 0) {
            fclose(fp);
            fprintf(stderr, "E6002 PSDTLF書込失敗\n");
            return MIPAY_RC_IOERR;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "E6003 PSDTLFクローズ失敗\n");
        return MIPAY_RC_IOERR;
    }
    return MIPAY_RC_NORMAL;
}

static void free_work(work_t *w)
{
    free(w->txns);
    free(w->adjs);
    free(w->merchants);
    free(w->details);
}

int main(void)
{
    work_t w;
    int rc;
    const char *pstxnf = getenv("PSTXNF");
    const char *psadjf = getenv("PSADJF");
    const char *psmerf = getenv("PSMERF");
    const char *psdtlf = getenv("PSDTLF");

    if (pstxnf == NULL) {
        pstxnf = "PSTXNF.csv";
    }
    if (psadjf == NULL) {
        psadjf = "PSADJF.csv";
    }
    if (psmerf == NULL) {
        psmerf = "PSMERF.csv";
    }
    if (psdtlf == NULL) {
        psdtlf = "PSDTLF.dat";
    }

    memset(&w, 0, sizeof w);
    w.txns = calloc(MIPAY_MAX_TXN, sizeof w.txns[0]);
    w.adjs = calloc(MIPAY_MAX_ADJ, sizeof w.adjs[0]);
    w.merchants = calloc(MIPAY_MAX_MERCHANT, sizeof w.merchants[0]);
    w.details = calloc(MIPAY_MAX_LINE, sizeof w.details[0]);
    if (w.txns == NULL || w.adjs == NULL || w.merchants == NULL || w.details == NULL) {
        fprintf(stderr, "E9001 作業領域を確保できません\n");
        free_work(&w);
        return MIPAY_RC_LIMITERR;
    }

    rc = read_psmerf(psmerf, &w);
    if (rc == MIPAY_RC_NORMAL) {
        rc = read_pstxnf(pstxnf, &w);
    }
    if (rc == MIPAY_RC_NORMAL) {
        rc = read_psadjf(psadjf, &w);
    }
    if (rc == MIPAY_RC_NORMAL) {
        rc = build_details(&w);
    }
    if (rc == MIPAY_RC_NORMAL) {
        rc = write_psdtlf(psdtlf, &w);
    }

    if (rc == MIPAY_RC_NORMAL) {
        fprintf(stderr, "I0001 正常終了 明細=%zu\n", w.detail_count);
    }

    free_work(&w);
    return rc;
}
