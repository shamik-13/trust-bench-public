/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20191022  藤田 和也 (E-271)  初版作成
 */
#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define IN_SCEXEC   "SCEXEC.csv"
#define IN_SCPOSF   "SCPOSF.csv"
#define OUT_HFRISKC "HFRISKC.csv"

#define MAX_LINE 1024
#define MAX_CIF  32
#define MAX_INSTR 32
#define MAX_TS 32

typedef struct {
    char cif_no[MAX_CIF];
    char instr_code[MAX_INSTR];
    int64_t net_qty;
    int64_t open_notional_amt;
    int reject_cnt;
    char last_upd_ts[MAX_TS];
} RiskRow;

typedef struct {
    RiskRow *v;
    size_t n;
    size_t cap;
} RiskTab;

static void rstrip(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
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

static int parse_int(const char *s, int *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int abs_i64(int64_t v, int64_t *out)
{
    if (v == INT64_MIN) {
        return -1;
    }
    *out = (v < 0) ? -v : v;
    return 0;
}

static int add_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a != 0 && b != 0) {
        int64_t av;
        int64_t bv;

        if (abs_i64(a, &av) != 0 || abs_i64(b, &bv) != 0 || av > INT64_MAX / bv) {
            return -1;
        }
    }
    *out = a * b;
    return 0;
}

static int ensure_cap(RiskTab *tab)
{
    RiskRow *nv;
    size_t next;

    if (tab->n < tab->cap) {
        return 0;
    }
    next = (tab->cap == 0U) ? 64U : tab->cap * 2U;
    if (next < tab->cap || next > (SIZE_MAX / sizeof(*tab->v))) {
        return -1;
    }
    nv = (RiskRow *)realloc(tab->v, next * sizeof(*tab->v));
    if (nv == NULL) {
        return -1;
    }
    tab->v = nv;
    tab->cap = next;
    return 0;
}

static RiskRow *find_row(RiskTab *tab, const char *cif_no, const char *instr_code)
{
    size_t i;

    for (i = 0U; i < tab->n; i++) {
        if (strcmp(tab->v[i].cif_no, cif_no) == 0 &&
            strcmp(tab->v[i].instr_code, instr_code) == 0) {
            return &tab->v[i];
        }
    }
    return NULL;
}

static RiskRow *upsert_row(RiskTab *tab, const char *cif_no, const char *instr_code)
{
    RiskRow *r;

    r = find_row(tab, cif_no, instr_code);
    if (r != NULL) {
        return r;
    }
    if (strlen(cif_no) >= MAX_CIF || strlen(instr_code) >= MAX_INSTR || ensure_cap(tab) != 0) {
        return NULL;
    }
    r = &tab->v[tab->n++];
    memset(r, 0, sizeof(*r));
    strcpy(r->cif_no, cif_no);
    strcpy(r->instr_code, instr_code);
    strcpy(r->last_upd_ts, "00000000000000");
    return r;
}

static int split_csv(char *line, char **fld, size_t want)
{
    size_t n = 0U;
    char *p = line;

    while (n < want) {
        fld[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return (n == want && strchr(fld[want - 1U], ',') == NULL) ? 0 : -1;
}

static int cif_from_order(const char *order_id, char *out, size_t outsz)
{
    size_t n = strcspn(order_id, "-_/");
    if (n == 0U || n >= outsz) {
        return -1;
    }
    memcpy(out, order_id, n);
    out[n] = '\0';
    return 0;
}

static int read_posf(RiskTab *tab)
{
    FILE *fp = fopen(IN_SCPOSF, "r");
    char line[MAX_LINE];

    if (fp == NULL) {
        fprintf(stderr, "SCPOSFオープン失敗\n");
        return -1;
    }
    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCPOSFヘッダ読込失敗\n");
        return -1;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[5];
        int64_t net_qty;
        int64_t avg_amt;
        int64_t base;
        int64_t abs_qty;
        RiskRow *r;

        rstrip(line);
        if (split_csv(line, f, 5U) != 0 ||
            parse_i64(f[2], &net_qty) != 0 ||
            parse_i64(f[3], &avg_amt) != 0 ||
            avg_amt < 0 ||
            abs_i64(net_qty, &abs_qty) != 0 ||
            mul_i64(abs_qty, avg_amt, &base) != 0) {
            fclose(fp);
            fprintf(stderr, "SCPOSF解析失敗\n");
            return -1;
        }
        r = upsert_row(tab, f[0], f[1]);
        if (r == NULL) {
            fclose(fp);
            fprintf(stderr, "SCPOSF格納失敗\n");
            return -1;
        }
        r->net_qty = net_qty;
        r->open_notional_amt = (net_qty == 0) ? 0 : base;
    }
    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCPOSF読込失敗\n");
        return -1;
    }
    fclose(fp);
    return 0;
}

static int read_existing_risk(RiskTab *tab)
{
    FILE *fp = fopen(OUT_HFRISKC, "r");
    char line[MAX_LINE];

    if (fp == NULL) {
        return (errno == ENOENT) ? 0 : -1;
    }
    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        return 0;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[5];
        int64_t amt;
        int cnt;
        RiskRow *r;

        rstrip(line);
        if (split_csv(line, f, 5U) != 0 ||
            parse_i64(f[2], &amt) != 0 ||
            parse_int(f[3], &cnt) != 0 ||
            strlen(f[4]) >= MAX_TS) {
            fclose(fp);
            fprintf(stderr, "HFRISKC解析失敗\n");
            return -1;
        }
        r = upsert_row(tab, f[0], f[1]);
        if (r == NULL) {
            fclose(fp);
            fprintf(stderr, "HFRISKC格納失敗\n");
            return -1;
        }
        r->open_notional_amt = amt;
        r->reject_cnt = cnt;
        strcpy(r->last_upd_ts, f[4]);
    }
    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "HFRISKC読込失敗\n");
        return -1;
    }
    fclose(fp);
    return 0;
}

static int apply_exec(RiskTab *tab)
{
    FILE *fp = fopen(IN_SCEXEC, "r");
    char line[MAX_LINE];

    if (fp == NULL) {
        fprintf(stderr, "SCEXECオープン失敗\n");
        return -1;
    }
    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "SCEXECヘッダ読込失敗\n");
        return -1;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[7];
        char cif_no[MAX_CIF];
        int64_t fill_qty;
        int64_t fill_amt;
        int64_t signed_qty;
        int64_t next_qty;
        int64_t next_amt;
        RiskRow *r;

        rstrip(line);
        if (split_csv(line, f, 7U) != 0 ||
            cif_from_order(f[1], cif_no, sizeof(cif_no)) != 0 ||
            parse_i64(f[4], &fill_qty) != 0 ||
            parse_i64(f[5], &fill_amt) != 0 ||
            fill_qty <= 0 ||
            fill_amt < 0 ||
            strlen(f[6]) >= MAX_TS ||
            (strcmp(f[3], "B") != 0 && strcmp(f[3], "S") != 0)) {
            fclose(fp);
            fprintf(stderr, "SCEXEC解析失敗\n");
            return -1;
        }
        r = upsert_row(tab, cif_no, f[2]);
        if (r == NULL) {
            fclose(fp);
            fprintf(stderr, "SCEXEC格納失敗\n");
            return -1;
        }
        if (strcmp(f[6], r->last_upd_ts) <= 0) {
            continue;
        }
        signed_qty = (strcmp(f[3], "B") == 0) ? fill_qty : -fill_qty;
        if (add_i64(r->net_qty, signed_qty, &next_qty) != 0) {
            fclose(fp);
            fprintf(stderr, "数量更新失敗\n");
            return -1;
        }
        if (next_qty == 0) {
            next_amt = 0;
        } else {
            int64_t delta_amt = (signed_qty > 0) ? fill_amt : -fill_amt;
            if (add_i64((r->net_qty < 0) ? -r->open_notional_amt : r->open_notional_amt,
                        delta_amt, &next_amt) != 0 ||
                abs_i64(next_amt, &next_amt) != 0) {
                fclose(fp);
                fprintf(stderr, "想定元本更新失敗\n");
                return -1;
            }
        }
        r->net_qty = next_qty;
        r->open_notional_amt = next_amt;
        strcpy(r->last_upd_ts, f[6]);
        if (r->open_notional_amt > MIHFT_MAX_NOTIONAL && r->reject_cnt < INT_MAX) {
            r->reject_cnt++;
        }
    }
    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCEXEC読込失敗\n");
        return -1;
    }
    fclose(fp);
    return 0;
}

static int write_risk(const RiskTab *tab)
{
    FILE *fp = fopen(OUT_HFRISKC, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "HFRISKCオープン失敗\n");
        return -1;
    }
    if (fprintf(fp, "CIF-NO,INSTR-CODE,OPEN-NOTIONAL-AMT,REJECT-CNT,LAST-UPD-TS\n") < 0) {
        fclose(fp);
        fprintf(stderr, "HFRISKC書込失敗\n");
        return -1;
    }
    for (i = 0U; i < tab->n; i++) {
        const RiskRow *r = &tab->v[i];
        if (fprintf(fp, "%s,%s,%lld,%d,%s\n",
                    r->cif_no,
                    r->instr_code,
                    (long long)r->open_notional_amt,
                    r->reject_cnt,
                    r->last_upd_ts) < 0) {
            fclose(fp);
            fprintf(stderr, "HFRISKC書込失敗\n");
            return -1;
        }
    }
    if (fclose(fp) != 0) {
        fprintf(stderr, "HFRISKCクローズ失敗\n");
        return -1;
    }
    return 0;
}

int main(void)
{
    RiskTab tab;
    int rc = 0;

    memset(&tab, 0, sizeof(tab));
    if (read_posf(&tab) != 0 ||
        read_existing_risk(&tab) != 0 ||
        apply_exec(&tab) != 0 ||
        write_risk(&tab) != 0) {
        rc = 16;
    }
    free(tab.v);
    return rc;
}
