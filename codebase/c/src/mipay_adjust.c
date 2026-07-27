/*
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  20250218  SATO        初版作成。承認済み調整入力を精算明細へ反映。
 */

#include "mipay_settle.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MI_PATH_PSADJF "PSADJF.csv"
#define MI_PATH_PSMERF "PSMERF.csv"
#define MI_PATH_PSDTLF "PSDTLF.csv"

#define MI_MAX_LINE 1024
#define MI_MAX_ADJUST_ID 32
#define MI_MAX_MERCHANT_CODE 32
#define MI_MAX_KBN 8
#define MI_MAX_REASON 16
#define MI_MAX_DATE 16
#define MI_MAX_STATUS 8
#define MI_MAX_NAME 128
#define MI_MAX_BANK 64
#define MI_MAX_SETTLE_ID 48
#define MI_MAX_DETAIL_ID 64
#define MI_CHARGE_BP 30
#define MI_BP_DENOM 10000
#define MI_SETTLEABLE_STATUS "01"
#define MI_APPROVED_STATUS "01"
#define MI_DECISION_NORMAL 0
#define MI_DECISION_IO_ERROR 12
#define MI_DECISION_DATA_ERROR 16

typedef struct {
    char adjust_id[MI_MAX_ADJUST_ID];
    char merchant_code[MI_MAX_MERCHANT_CODE];
    char adjust_kbn[MI_MAX_KBN];
    int64_t adjust_amt;
    char reason_cd[MI_MAX_REASON];
    char apply_dt[MI_MAX_DATE];
    char approval_status[MI_MAX_STATUS];
} MiAdjustRec;

typedef struct {
    char merchant_code[MI_MAX_MERCHANT_CODE];
    char merchant_name[MI_MAX_NAME];
    char mer_status[MI_MAX_STATUS];
    char bank_acct_no[MI_MAX_BANK];
} MiMerchantRec;

typedef struct {
    MiMerchantRec *rows;
    size_t used;
    size_t cap;
} MiMerchantTable;

static void mi_trim_right(char *s)
{
    size_t n = strlen(s);

    while (n > 0) {
        unsigned char c = (unsigned char)s[n - 1];

        if (c != ' ' && c != '\t' && c != '\r' && c != '\n') {
            break;
        }
        s[--n] = '\0';
    }
}

static char *mi_trim_left(char *s)
{
    while (*s == ' ' || *s == '\t') {
        ++s;
    }
    return s;
}

static void mi_trim_field(char *s)
{
    char *p = mi_trim_left(s);

    if (p != s) {
        memmove(s, p, strlen(p) + 1);
    }
    mi_trim_right(s);
}

static int mi_copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (dstsz == 0 || n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int mi_split_csv(char *line, char *field[], size_t need)
{
    size_t idx = 0;
    char *p = line;

    while (idx < need) {
        char *start = p;
        char *w = p;

        if (*p == '"') {
            int closed = 0;

            ++p;
            start = p;
            while (*p != '\0') {
                if (*p == '"' && p[1] == '"') {
                    *w++ = '"';
                    p += 2;
                } else if (*p == '"') {
                    ++p;
                    closed = 1;
                    break;
                } else {
                    *w++ = *p++;
                }
            }
            if (!closed) {
                return -1;
            }
            while (*p == ' ' || *p == '\t') {
                ++p;
            }
            if (*p != ',' && *p != '\0' && *p != '\r' && *p != '\n') {
                return -1;
            }
        } else {
            while (*p != ',' && *p != '\0' && *p != '\r' && *p != '\n') {
                *w++ = *p++;
            }
        }

        *w = '\0';
        field[idx++] = start;
        mi_trim_field(start);

        if (*p == ',') {
            ++p;
            continue;
        }
        break;
    }

    if (idx != need) {
        return -1;
    }
    while (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n') {
        ++p;
    }
    return *p == '\0' ? 0 : -1;
}

static int mi_parse_i64(const char *s, int64_t *out)
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

static int mi_valid_yyyymmdd(const char *s)
{
    int y;
    int m;
    int d;
    static const int mdays[] = {
        31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    };

    if (strlen(s) != 8) {
        return 0;
    }
    for (size_t i = 0; i < 8; ++i) {
        if (s[i] < '0' || s[i] > '9') {
            return 0;
        }
    }

    y = (s[0] - '0') * 1000 + (s[1] - '0') * 100 +
        (s[2] - '0') * 10 + (s[3] - '0');
    m = (s[4] - '0') * 10 + (s[5] - '0');
    d = (s[6] - '0') * 10 + (s[7] - '0');

    if (y < 2000 || y > 2099 || m < 1 || m > 12 || d < 1) {
        return 0;
    }

    if (m == 2 && ((y % 400 == 0) || (y % 4 == 0 && y % 100 != 0))) {
        return d <= 29;
    }
    return d <= mdays[m - 1];
}

static int mi_append_merchant(MiMerchantTable *table, const MiMerchantRec *rec)
{
    MiMerchantRec *p;
    size_t next;

    if (table->used == table->cap) {
        next = table->cap == 0 ? 256U : table->cap * 2U;
        if (next < table->cap || next > SIZE_MAX / sizeof(*table->rows)) {
            return -1;
        }

        p = (MiMerchantRec *)realloc(table->rows, next * sizeof(*table->rows));
        if (p == NULL) {
            return -1;
        }
        table->rows = p;
        table->cap = next;
    }

    table->rows[table->used++] = *rec;
    return 0;
}

static int mi_cmp_merchant(const void *a, const void *b)
{
    const MiMerchantRec *ma = (const MiMerchantRec *)a;
    const MiMerchantRec *mb = (const MiMerchantRec *)b;

    return strcmp(ma->merchant_code, mb->merchant_code);
}

static const MiMerchantRec *mi_find_merchant(const MiMerchantTable *table,
                                             const char *merchant_code)
{
    size_t lo = 0;
    size_t hi = table->used;

    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2U;
        int cmp = strcmp(table->rows[mid].merchant_code, merchant_code);

        if (cmp == 0) {
            return &table->rows[mid];
        }
        if (cmp < 0) {
            lo = mid + 1U;
        } else {
            hi = mid;
        }
    }
    return NULL;
}

static int mi_load_merchants(MiMerchantTable *table)
{
    FILE *fp = fopen(MI_PATH_PSMERF, "r");
    char line[MI_MAX_LINE];
    unsigned long lno = 0;

    if (fp == NULL) {
        fprintf(stderr, "E001 PSMERFオープン失敗\n");
        return MI_DECISION_IO_ERROR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[4];
        MiMerchantRec rec;

        ++lno;
        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fprintf(stderr, "E002 PSMERF行長超過:%lu\n", lno);
            fclose(fp);
            return MI_DECISION_DATA_ERROR;
        }
        if (lno == 1 && strstr(line, "MERCHANT-CODE") != NULL) {
            continue;
        }

        if (mi_split_csv(line, f, 4) != 0 ||
            mi_copy_field(rec.merchant_code, sizeof(rec.merchant_code), f[0]) != 0 ||
            mi_copy_field(rec.merchant_name, sizeof(rec.merchant_name), f[1]) != 0 ||
            mi_copy_field(rec.mer_status, sizeof(rec.mer_status), f[2]) != 0 ||
            mi_copy_field(rec.bank_acct_no, sizeof(rec.bank_acct_no), f[3]) != 0 ||
            rec.merchant_code[0] == '\0') {
            fprintf(stderr, "E003 PSMERF形式不正:%lu\n", lno);
            fclose(fp);
            return MI_DECISION_DATA_ERROR;
        }

        if (mi_append_merchant(table, &rec) != 0) {
            fprintf(stderr, "E004 PSMERF領域不足\n");
            fclose(fp);
            return MI_DECISION_IO_ERROR;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E005 PSMERF読込失敗\n");
        fclose(fp);
        return MI_DECISION_IO_ERROR;
    }
    fclose(fp);

    qsort(table->rows, table->used, sizeof(*table->rows), mi_cmp_merchant);
    for (size_t i = 1; i < table->used; ++i) {
        if (strcmp(table->rows[i - 1].merchant_code,
                   table->rows[i].merchant_code) == 0) {
            fprintf(stderr, "E006 PSMERF加盟店重複\n");
            return MI_DECISION_DATA_ERROR;
        }
    }

    return MI_DECISION_NORMAL;
}

static int mi_parse_adjust(char *line, unsigned long lno, MiAdjustRec *rec)
{
    char *f[7];

    if (mi_split_csv(line, f, 7) != 0 ||
        mi_copy_field(rec->adjust_id, sizeof(rec->adjust_id), f[0]) != 0 ||
        mi_copy_field(rec->merchant_code, sizeof(rec->merchant_code), f[1]) != 0 ||
        mi_copy_field(rec->adjust_kbn, sizeof(rec->adjust_kbn), f[2]) != 0 ||
        mi_copy_field(rec->reason_cd, sizeof(rec->reason_cd), f[4]) != 0 ||
        mi_copy_field(rec->apply_dt, sizeof(rec->apply_dt), f[5]) != 0 ||
        mi_copy_field(rec->approval_status, sizeof(rec->approval_status), f[6]) != 0 ||
        mi_parse_i64(f[3], &rec->adjust_amt) != 0) {
        fprintf(stderr, "E011 PSADJF形式不正:%lu\n", lno);
        return -1;
    }

    if (rec->adjust_id[0] == '\0' ||
        rec->merchant_code[0] == '\0' ||
        rec->adjust_amt <= 0 ||
        mi_valid_yyyymmdd(rec->apply_dt) == 0) {
        fprintf(stderr, "E012 PSADJF値不正:%lu\n", lno);
        return -1;
    }

    if (strcmp(rec->adjust_kbn, "A") != 0 &&
        strcmp(rec->adjust_kbn, "S") != 0) {
        fprintf(stderr, "E013 PSADJF調整区分不正:%lu\n", lno);
        return -1;
    }

    return 0;
}

static int mi_charge_amount(int64_t amount, int64_t *charge)
{
    if (amount > INT64_MAX / MI_CHARGE_BP) {
        return -1;
    }

    *charge = (amount * MI_CHARGE_BP + MI_BP_DENOM - 1) / MI_BP_DENOM;
    return 0;
}

static int mi_make_ids(const MiAdjustRec *adj,
                       char *settle_id,
                       size_t settle_sz,
                       char *detail_id,
                       size_t detail_sz)
{
    int n1;
    int n2;

    n1 = snprintf(settle_id, settle_sz, "ST%s%s",
                  adj->apply_dt, adj->merchant_code);
    n2 = snprintf(detail_id, detail_sz, "ADJ%s", adj->adjust_id);

    if (n1 < 0 || n2 < 0 ||
        (size_t)n1 >= settle_sz || (size_t)n2 >= detail_sz) {
        return -1;
    }
    return 0;
}

static int mi_write_detail(FILE *out, const MiAdjustRec *adj)
{
    char settle_id[MI_MAX_SETTLE_ID];
    char detail_id[MI_MAX_DETAIL_ID];
    int64_t signed_amt;
    int64_t charge_amt;

    if (mi_make_ids(adj, settle_id, sizeof(settle_id),
                    detail_id, sizeof(detail_id)) != 0) {
        fprintf(stderr, "E021 PSDTLF識別子長超過\n");
        return MI_DECISION_DATA_ERROR;
    }

    signed_amt = strcmp(adj->adjust_kbn, "S") == 0
        ? -adj->adjust_amt
        : adj->adjust_amt;

    if (mi_charge_amount(adj->adjust_amt, &charge_amt) != 0) {
        fprintf(stderr, "E022 PSDTLF手数料算出桁超過\n");
        return MI_DECISION_DATA_ERROR;
    }

    if (fprintf(out, "%s,%s,%s,%s,%" PRId64 ",%" PRId64 ",%s\n",
                detail_id,
                settle_id,
                adj->merchant_code,
                adj->adjust_id,
                signed_amt,
                charge_amt,
                adj->adjust_kbn) < 0) {
        fprintf(stderr, "E023 PSDTLF書込失敗\n");
        return MI_DECISION_IO_ERROR;
    }

    return MI_DECISION_NORMAL;
}

int main(void)
{
    MiMerchantTable merchants;
    FILE *in;
    FILE *out;
    char line[MI_MAX_LINE];
    unsigned long lno = 0;
    unsigned long wrote = 0;
    int rc;

    memset(&merchants, 0, sizeof(merchants));

    rc = mi_load_merchants(&merchants);
    if (rc != MI_DECISION_NORMAL) {
        free(merchants.rows);
        return rc;
    }

    in = fopen(MI_PATH_PSADJF, "r");
    if (in == NULL) {
        fprintf(stderr, "E031 PSADJFオープン失敗\n");
        free(merchants.rows);
        return MI_DECISION_IO_ERROR;
    }

    out = fopen(MI_PATH_PSDTLF, "w");
    if (out == NULL) {
        fprintf(stderr, "E032 PSDTLFオープン失敗\n");
        fclose(in);
        free(merchants.rows);
        return MI_DECISION_IO_ERROR;
    }

    rc = MI_DECISION_NORMAL;

    while (fgets(line, sizeof(line), in) != NULL) {
        MiAdjustRec adj;
        const MiMerchantRec *mer;

        ++lno;
        if (strchr(line, '\n') == NULL && !feof(in)) {
            fprintf(stderr, "E033 PSADJF行長超過:%lu\n", lno);
            rc = MI_DECISION_DATA_ERROR;
            break;
        }
        if (lno == 1 && strstr(line, "ADJUST-ID") != NULL) {
            continue;
        }

        if (mi_parse_adjust(line, lno, &adj) != 0) {
            rc = MI_DECISION_DATA_ERROR;
            break;
        }

        if (strcmp(adj.approval_status, MI_APPROVED_STATUS) != 0) {
            continue;
        }

        mer = mi_find_merchant(&merchants, adj.merchant_code);
        if (mer == NULL) {
            fprintf(stderr, "W001 加盟店未登録:%s\n", adj.merchant_code);
            continue;
        }

        if (strcmp(mer->mer_status, MI_SETTLEABLE_STATUS) != 0) {
            fprintf(stderr, "W002 加盟店精算対象外:%s\n", adj.merchant_code);
            continue;
        }

        rc = mi_write_detail(out, &adj);
        if (rc != MI_DECISION_NORMAL) {
            break;
        }
        ++wrote;
    }

    if (rc == MI_DECISION_NORMAL && ferror(in)) {
        fprintf(stderr, "E034 PSADJF読込失敗\n");
        rc = MI_DECISION_IO_ERROR;
    }

    if (fclose(out) != 0 && rc == MI_DECISION_NORMAL) {
        fprintf(stderr, "E035 PSDTLFクローズ失敗\n");
        rc = MI_DECISION_IO_ERROR;
    }

    fclose(in);
    free(merchants.rows);

    if (rc == MI_DECISION_NORMAL) {
        fprintf(stderr, "I001 精算調整反映件数:%lu\n", wrote);
    }

    return rc;
}
