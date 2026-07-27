/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240311  決済基盤  初版作成
 * 1.01  20240909  決済基盤  適用日抽出と停止復帰時の口座再検査を追加
 */

#include "mipay_settle.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIPAY_OK 0
#define MIPAY_ERR_IO 2
#define MIPAY_ERR_PARSE 3
#define MIPAY_ERR_LIMIT 4
#define MIPAY_ERR_CHECK 5

#define FIELD_MAX 256
#define LINE_MAX_LEN 2048
#define MERCHANT_CODE_MAX 32
#define MERCHANT_NAME_MAX 128
#define STATUS_MAX 3
#define BANK_ACCT_MAX 32
#define CONF_KEY_MAX 64
#define CONF_VALUE_MAX 256
#define DATE_MAX 9

struct merchant_row {
    char code[MERCHANT_CODE_MAX];
    char name[MERCHANT_NAME_MAX];
    char status[STATUS_MAX];
    char bank_acct[BANK_ACCT_MAX];
};

struct conf_row {
    char key[CONF_KEY_MAX];
    char value[CONF_VALUE_MAX];
    char apply_dt[DATE_MAX];
    char expire_dt[DATE_MAX];
    char updated_at[32];
};

struct diff_row {
    char code[MERCHANT_CODE_MAX];
    char name[MERCHANT_NAME_MAX];
    char status[STATUS_MAX];
    char bank_acct[BANK_ACCT_MAX];
    char apply_dt[DATE_MAX];
};

struct merchant_table {
    struct merchant_row *rows;
    size_t len;
    size_t cap;
};

struct diff_table {
    struct diff_row *rows;
    size_t len;
    size_t cap;
};

static int is_ymd8(const char *s)
{
    int y;
    int m;
    int d;
    static const int mdays[] = { 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

    if (strlen(s) != 8) {
        return 0;
    }
    for (size_t i = 0; i < 8; i++) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
    }

    y = (s[0] - '0') * 1000 + (s[1] - '0') * 100 + (s[2] - '0') * 10 + (s[3] - '0');
    m = (s[4] - '0') * 10 + (s[5] - '0');
    d = (s[6] - '0') * 10 + (s[7] - '0');

    if (y < 2000 || y > 2099 || m < 1 || m > 12) {
        return 0;
    }

    if (m == 2 && ((y % 400 == 0) || (y % 4 == 0 && y % 100 != 0))) {
        return d >= 1 && d <= 29;
    }
    return d >= 1 && d <= mdays[m];
}

static int copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t n = strlen(src);

    if (n >= dst_sz) {
        return 0;
    }
    memcpy(dst, src, n + 1);
    return 1;
}

static char *trim(char *s)
{
    char *e;

    while (*s != '\0' && isspace((unsigned char)*s)) {
        s++;
    }
    e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) {
        *--e = '\0';
    }
    return s;
}

static int split_csv(char *line, char **out, size_t out_cap, size_t *out_len)
{
    char *p = line;
    size_t n = 0;

    while (*p != '\0') {
        char *field;

        if (n == out_cap) {
            return 0;
        }

        if (*p == '"') {
            field = ++p;
            out[n++] = field;
            while (*p != '\0') {
                if (*p == '"' && p[1] == '"') {
                    memmove(p, p + 1, strlen(p));
                    p++;
                } else if (*p == '"') {
                    *p++ = '\0';
                    break;
                } else {
                    p++;
                }
            }
            if (*p == ',') {
                *p++ = '\0';
            } else if (*p != '\0' && *p != '\n' && *p != '\r') {
                return 0;
            }
        } else {
            field = p;
            while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
                p++;
            }
            if (*p == ',') {
                *p++ = '\0';
            } else {
                *p = '\0';
            }
            out[n++] = trim(field);
        }
    }

    *out_len = n;
    return 1;
}

static int valid_code(const char *s)
{
    size_t n = strlen(s);

    if (n == 0 || n >= MERCHANT_CODE_MAX) {
        return 0;
    }
    for (size_t i = 0; i < n; i++) {
        unsigned char c = (unsigned char)s[i];
        if (!isalnum(c) && c != '-' && c != '_') {
            return 0;
        }
    }
    return 1;
}

static int valid_status(const char *s)
{
    return strcmp(s, "01") == 0 || strcmp(s, "02") == 0 || strcmp(s, "09") == 0;
}

static int valid_bank_acct(const char *s)
{
    size_t n = strlen(s);

    if (n < 7 || n >= BANK_ACCT_MAX) {
        return 0;
    }
    for (size_t i = 0; i < n; i++) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
    }
    return 1;
}

static int merchant_push(struct merchant_table *t, const struct merchant_row *r)
{
    struct merchant_row *p;
    size_t next;

    if (t->len == t->cap) {
        next = t->cap == 0 ? 128u : t->cap * 2u;
        if (next <= t->cap || next > (SIZE_MAX / sizeof(*t->rows))) {
            return 0;
        }
        p = (struct merchant_row *)realloc(t->rows, next * sizeof(*t->rows));
        if (p == NULL) {
            return 0;
        }
        t->rows = p;
        t->cap = next;
    }

    t->rows[t->len++] = *r;
    return 1;
}

static int diff_push(struct diff_table *t, const struct diff_row *r)
{
    struct diff_row *p;
    size_t next;

    if (t->len == t->cap) {
        next = t->cap == 0 ? 128u : t->cap * 2u;
        if (next <= t->cap || next > (SIZE_MAX / sizeof(*t->rows))) {
            return 0;
        }
        p = (struct diff_row *)realloc(t->rows, next * sizeof(*t->rows));
        if (p == NULL) {
            return 0;
        }
        t->rows = p;
        t->cap = next;
    }

    t->rows[t->len++] = *r;
    return 1;
}

static int load_merchants(const char *path, struct merchant_table *t)
{
    FILE *fp = fopen(path, "r");
    char line[LINE_MAX_LEN];
    unsigned long lno = 0;

    if (fp == NULL) {
        fprintf(stderr, "E001:PSMERF入力オープン失敗:%s\n", path);
        return MIPAY_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[4];
        size_t nf = 0;
        struct merchant_row r;

        lno++;
        if (lno == 1 && strstr(line, "MERCHANT-CODE") != NULL) {
            continue;
        }
        if (!split_csv(line, f, 4, &nf) || nf != 4) {
            fprintf(stderr, "E101:PSMERF項目数不正:%lu\n", lno);
            fclose(fp);
            return MIPAY_ERR_PARSE;
        }
        if (!valid_code(f[0]) || !valid_status(f[2]) || !valid_bank_acct(f[3])) {
            fprintf(stderr, "E102:PSMERF値不正:%lu\n", lno);
            fclose(fp);
            return MIPAY_ERR_PARSE;
        }
        if (!copy_field(r.code, sizeof(r.code), f[0]) ||
            !copy_field(r.name, sizeof(r.name), f[1]) ||
            !copy_field(r.status, sizeof(r.status), f[2]) ||
            !copy_field(r.bank_acct, sizeof(r.bank_acct), f[3])) {
            fprintf(stderr, "E103:PSMERF桁数超過:%lu\n", lno);
            fclose(fp);
            return MIPAY_ERR_PARSE;
        }
        if (!merchant_push(t, &r)) {
            fprintf(stderr, "E104:PSMERF領域不足:%lu\n", lno);
            fclose(fp);
            return MIPAY_ERR_LIMIT;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E002:PSMERF読込失敗:%s\n", path);
        fclose(fp);
        return MIPAY_ERR_IO;
    }

    fclose(fp);
    return MIPAY_OK;
}

static int load_apply_date(const char *path, char out[DATE_MAX])
{
    FILE *fp = fopen(path, "r");
    char line[LINE_MAX_LEN];
    char best_updated[32] = "";
    unsigned long lno = 0;
    int found = 0;

    if (fp == NULL) {
        fprintf(stderr, "E011:PSCONF入力オープン失敗:%s\n", path);
        return MIPAY_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[5];
        size_t nf = 0;
        struct conf_row r;

        lno++;
        if (lno == 1 && strstr(line, "CONF-KEY") != NULL) {
            continue;
        }
        if (!split_csv(line, f, 5, &nf) || nf != 5) {
            fprintf(stderr, "E111:PSCONF項目数不正:%lu\n", lno);
            fclose(fp);
            return MIPAY_ERR_PARSE;
        }
        if (!copy_field(r.key, sizeof(r.key), f[0]) ||
            !copy_field(r.value, sizeof(r.value), f[1]) ||
            !copy_field(r.apply_dt, sizeof(r.apply_dt), f[2]) ||
            !copy_field(r.expire_dt, sizeof(r.expire_dt), f[3]) ||
            !copy_field(r.updated_at, sizeof(r.updated_at), f[4])) {
            fprintf(stderr, "E112:PSCONF桁数超過:%lu\n", lno);
            fclose(fp);
            return MIPAY_ERR_PARSE;
        }
        if (!is_ymd8(r.apply_dt) || (r.expire_dt[0] != '\0' && !is_ymd8(r.expire_dt))) {
            fprintf(stderr, "E113:PSCONF日付不正:%lu\n", lno);
            fclose(fp);
            return MIPAY_ERR_PARSE;
        }
        if (strcmp(r.key, "MERGSYNC_APPLY_DT") == 0 &&
            (r.expire_dt[0] == '\0' || strcmp(r.apply_dt, r.expire_dt) <= 0) &&
            (!found || strcmp(r.updated_at, best_updated) > 0)) {
            if (!is_ymd8(r.value)) {
                fprintf(stderr, "E114:PSCONF適用日不正:%lu\n", lno);
                fclose(fp);
                return MIPAY_ERR_PARSE;
            }
            copy_field(out, DATE_MAX, r.value);
            copy_field(best_updated, sizeof(best_updated), r.updated_at);
            found = 1;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E012:PSCONF読込失敗:%s\n", path);
        fclose(fp);
        return MIPAY_ERR_IO;
    }

    fclose(fp);
    if (!found) {
        fprintf(stderr, "E115:PSCONF適用日未設定\n");
        return MIPAY_ERR_PARSE;
    }
    return MIPAY_OK;
}

static int load_diffs(const char *path, const char apply_dt[DATE_MAX], struct diff_table *t)
{
    FILE *fp = fopen(path, "r");
    char line[LINE_MAX_LEN];
    unsigned long lno = 0;

    if (fp == NULL) {
        fprintf(stderr, "E021:差分入力オープン失敗:%s\n", path);
        return MIPAY_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[5];
        size_t nf = 0;
        struct diff_row r;

        lno++;
        if (lno == 1 && strstr(line, "MERCHANT-CODE") != NULL) {
            continue;
        }
        if (!split_csv(line, f, 5, &nf) || nf != 5) {
            fprintf(stderr, "E121:差分項目数不正:%lu\n", lno);
            fclose(fp);
            return MIPAY_ERR_PARSE;
        }
        if (!valid_code(f[0]) || !valid_status(f[2]) || !valid_bank_acct(f[3]) || !is_ymd8(f[4])) {
            fprintf(stderr, "E122:差分値不正:%lu\n", lno);
            fclose(fp);
            return MIPAY_ERR_PARSE;
        }
        if (strcmp(f[4], apply_dt) > 0) {
            continue;
        }
        if (!copy_field(r.code, sizeof(r.code), f[0]) ||
            !copy_field(r.name, sizeof(r.name), f[1]) ||
            !copy_field(r.status, sizeof(r.status), f[2]) ||
            !copy_field(r.bank_acct, sizeof(r.bank_acct), f[3]) ||
            !copy_field(r.apply_dt, sizeof(r.apply_dt), f[4])) {
            fprintf(stderr, "E123:差分桁数超過:%lu\n", lno);
            fclose(fp);
            return MIPAY_ERR_PARSE;
        }
        if (!diff_push(t, &r)) {
            fprintf(stderr, "E124:差分領域不足:%lu\n", lno);
            fclose(fp);
            return MIPAY_ERR_LIMIT;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "E022:差分読込失敗:%s\n", path);
        fclose(fp);
        return MIPAY_ERR_IO;
    }

    fclose(fp);
    return MIPAY_OK;
}

static struct merchant_row *find_merchant(struct merchant_table *t, const char *code)
{
    size_t lo = 0;
    size_t hi = t->len;

    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        int c = strcmp(t->rows[mid].code, code);

        if (c == 0) {
            return &t->rows[mid];
        }
        if (c < 0) {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    return NULL;
}

static int cmp_merchant(const void *a, const void *b)
{
    const struct merchant_row *ma = (const struct merchant_row *)a;
    const struct merchant_row *mb = (const struct merchant_row *)b;

    return strcmp(ma->code, mb->code);
}

static int cmp_diff(const void *a, const void *b)
{
    const struct diff_row *da = (const struct diff_row *)a;
    const struct diff_row *db = (const struct diff_row *)b;
    int c = strcmp(da->code, db->code);

    if (c != 0) {
        return c;
    }
    return strcmp(da->apply_dt, db->apply_dt);
}

static int shell_safe_token(const char *s)
{
    for (size_t i = 0; s[i] != '\0'; i++) {
        unsigned char c = (unsigned char)s[i];
        if (!isalnum(c) && c != '-' && c != '_') {
            return 0;
        }
    }
    return 1;
}

static int run_merchk(const char *code, const char *acct)
{
    char cmd[128];
    int n;
    int rc;

    if (!shell_safe_token(code) || !shell_safe_token(acct)) {
        fprintf(stderr, "E131:口座検査引数不正:%s\n", code);
        return MIPAY_ERR_CHECK;
    }

    n = snprintf(cmd, sizeof(cmd), "mipay_merchk %s %s", code, acct);
    if (n < 0 || (size_t)n >= sizeof(cmd)) {
        fprintf(stderr, "E132:口座検査コマンド超過:%s\n", code);
        return MIPAY_ERR_CHECK;
    }

    rc = system(cmd);
    if (rc != 0) {
        fprintf(stderr, "E133:口座検査否認:%s\n", code);
        return MIPAY_ERR_CHECK;
    }
    return MIPAY_OK;
}

static int apply_diffs(struct merchant_table *mt, struct diff_table *dt)
{
    const char *last_code = NULL;

    qsort(mt->rows, mt->len, sizeof(mt->rows[0]), cmp_merchant);
    qsort(dt->rows, dt->len, sizeof(dt->rows[0]), cmp_diff);

    for (size_t i = 0; i < dt->len; i++) {
        struct merchant_row *m;

        if (last_code != NULL && strcmp(last_code, dt->rows[i].code) == 0) {
            continue;
        }
        last_code = dt->rows[i].code;

        m = find_merchant(mt, dt->rows[i].code);
        if (m == NULL) {
            fprintf(stderr, "W201:加盟店未登録:%s\n", dt->rows[i].code);
            continue;
        }

        if (strcmp(m->status, "02") == 0 && strcmp(dt->rows[i].status, "01") == 0) {
            int rc = run_merchk(dt->rows[i].code, dt->rows[i].bank_acct);
            if (rc != MIPAY_OK) {
                return rc;
            }
        }

        if (!copy_field(m->name, sizeof(m->name), dt->rows[i].name) ||
            !copy_field(m->status, sizeof(m->status), dt->rows[i].status) ||
            !copy_field(m->bank_acct, sizeof(m->bank_acct), dt->rows[i].bank_acct)) {
            fprintf(stderr, "E141:加盟店更新桁数超過:%s\n", dt->rows[i].code);
            return MIPAY_ERR_PARSE;
        }
    }

    return MIPAY_OK;
}

static int write_csv_field(FILE *fp, const char *s)
{
    int quote = 0;

    for (size_t i = 0; s[i] != '\0'; i++) {
        if (s[i] == ',' || s[i] == '"' || s[i] == '\n' || s[i] == '\r') {
            quote = 1;
            break;
        }
    }

    if (quote && fputc('"', fp) == EOF) {
        return 0;
    }
    for (size_t i = 0; s[i] != '\0'; i++) {
        if (s[i] == '"' && fputc('"', fp) == EOF) {
            return 0;
        }
        if (fputc((unsigned char)s[i], fp) == EOF) {
            return 0;
        }
    }
    if (quote && fputc('"', fp) == EOF) {
        return 0;
    }
    return 1;
}

static int write_merchants(const char *path, const struct merchant_table *t)
{
    FILE *fp = fopen(path, "w");

    if (fp == NULL) {
        fprintf(stderr, "E031:PSMERF出力オープン失敗:%s\n", path);
        return MIPAY_ERR_IO;
    }

    if (fputs("MERCHANT-CODE,MERCHANT-NAME,MER-STATUS,BANK-ACCT-NO\n", fp) == EOF) {
        fprintf(stderr, "E032:PSMERFヘッダ出力失敗:%s\n", path);
        fclose(fp);
        return MIPAY_ERR_IO;
    }

    for (size_t i = 0; i < t->len; i++) {
        if (!write_csv_field(fp, t->rows[i].code) || fputc(',', fp) == EOF ||
            !write_csv_field(fp, t->rows[i].name) || fputc(',', fp) == EOF ||
            !write_csv_field(fp, t->rows[i].status) || fputc(',', fp) == EOF ||
            !write_csv_field(fp, t->rows[i].bank_acct) || fputc('\n', fp) == EOF) {
            fprintf(stderr, "E033:PSMERF明細出力失敗:%s\n", path);
            fclose(fp);
            return MIPAY_ERR_IO;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "E034:PSMERFクローズ失敗:%s\n", path);
        return MIPAY_ERR_IO;
    }

    return MIPAY_OK;
}

int main(int argc, char **argv)
{
    struct merchant_table merchants = { NULL, 0, 0 };
    struct diff_table diffs = { NULL, 0, 0 };
    char apply_dt[DATE_MAX] = "";
    int rc;

    if (argc != 5) {
        fprintf(stderr, "E900:起動引数不正\n");
        return MIPAY_ERR_PARSE;
    }

    rc = load_apply_date(argv[2], apply_dt);
    if (rc == MIPAY_OK) {
        rc = load_merchants(argv[1], &merchants);
    }
    if (rc == MIPAY_OK) {
        rc = load_diffs(argv[3], apply_dt, &diffs);
    }
    if (rc == MIPAY_OK) {
        rc = apply_diffs(&merchants, &diffs);
    }
    if (rc == MIPAY_OK) {
        rc = write_merchants(argv[4], &merchants);
    }

    free(merchants.rows);
    free(diffs.rows);

    return rc;
}
