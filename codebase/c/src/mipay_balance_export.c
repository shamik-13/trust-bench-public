/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240520  みらいペイ システム部  初版作成
 * 1.01  20241105  みらいペイ システム部  残高照会キー範囲の分割上限を追加
 */

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define 入力行最大 1024
#define ウォレットID最大 32
#define 利用者ID最大 32
#define 状態最大 3
#define 階層最大 8
#define カナ最大 96
#define 日付最大 9
#define 残高件数上限 200000
#define 範囲内件数上限 500
#define 照会下限残高 0LL
#define 正常終了 0
#define 異常終了 12

typedef struct {
    char wallet_id[ウォレットID最大];
    char user_id[利用者ID最大];
    char wallet_status[状態最大];
    char wallet_tier[階層最大];
    char user_name_kana[カナ最大];
} PYWALF_RECORD;

typedef struct {
    char wallet_id[ウォレットID最大];
    int64_t ledger_bal_amt;
    int64_t last_topup_amt;
    char bal_as_of_dt[日付最大];
} PYBALF_RECORD;

typedef struct {
    PYWALF_RECORD wallet;
    PYBALF_RECORD balance;
} EXPORT_CANDIDATE;

static int trim_field(char *s)
{
    size_t n;
    char *p;

    while (*s != '\0' && isspace((unsigned char)*s)) {
        memmove(s, s + 1, strlen(s));
    }

    n = strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1])) {
        s[--n] = '\0';
    }

    if (n >= 2 && s[0] == '"' && s[n - 1] == '"') {
        memmove(s, s + 1, n - 2);
        s[n - 2] = '\0';
        for (p = s; *p != '\0'; ++p) {
            if (*p == '"' && p[1] == '"') {
                memmove(p, p + 1, strlen(p));
            }
        }
    }

    return 0;
}

static int split_csv(char *line, char *field[], size_t max_field, size_t *count)
{
    char *p;
    size_t n = 0;
    int quoted = 0;

    if (line == NULL || field == NULL || count == NULL) {
        return -1;
    }

    field[n++] = line;
    for (p = line; *p != '\0'; ++p) {
        if (*p == '"') {
            quoted = !quoted;
        } else if (*p == ',' && !quoted) {
            if (n >= max_field) {
                return -1;
            }
            *p = '\0';
            field[n++] = p + 1;
        }
    }

    if (quoted) {
        return -1;
    }

    for (size_t i = 0; i < n; ++i) {
        trim_field(field[i]);
    }

    *count = n;
    return 0;
}

static int copy_checked(char *dst, size_t dst_size, const char *src)
{
    size_t n;

    if (dst == NULL || src == NULL || dst_size == 0) {
        return -1;
    }

    n = strlen(src);
    if (n == 0 || n >= dst_size) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_amount(const char *s, int64_t *out)
{
    char *end;
    long long v;

    if (s == NULL || *s == '\0' || out == NULL) {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno == ERANGE || *end != '\0') {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int valid_date_yyyymmdd(const char *s)
{
    if (s == NULL || strlen(s) != 8) {
        return 0;
    }

    for (size_t i = 0; i < 8; ++i) {
        if (!isdigit((unsigned char)s[i])) {
            return 0;
        }
    }

    return 1;
}

static int read_wallets(PYWALF_RECORD **records, size_t *count)
{
    FILE *fp;
    char line[入力行最大];
    size_t used = 0;
    size_t cap = 4096;
    PYWALF_RECORD *rows;

    fp = fopen("PYWALF.csv", "r");
    if (fp == NULL) {
        fprintf(stderr, "PYWALFオープン失敗\n");
        return -1;
    }

    rows = calloc(cap, sizeof(*rows));
    if (rows == NULL) {
        fclose(fp);
        fprintf(stderr, "領域確保失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *field[5];
        size_t n = 0;
        line[strcspn(line, "\r\n")] = '\0';

        if (line[0] == '\0') {
            continue;
        }

        if (split_csv(line, field, 5, &n) != 0 || n != 5) {
            free(rows);
            fclose(fp);
            fprintf(stderr, "PYWALF形式不正\n");
            return -1;
        }

        if (used == cap) {
            PYWALF_RECORD *next;
            if (cap > SIZE_MAX / 2) {
                free(rows);
                fclose(fp);
                fprintf(stderr, "PYWALF件数過大\n");
                return -1;
            }
            next = realloc(rows, cap * 2 * sizeof(*rows));
            if (next == NULL) {
                free(rows);
                fclose(fp);
                fprintf(stderr, "領域拡張失敗\n");
                return -1;
            }
            rows = next;
            cap *= 2;
        }

        if (copy_checked(rows[used].wallet_id, sizeof(rows[used].wallet_id), field[0]) != 0 ||
            copy_checked(rows[used].user_id, sizeof(rows[used].user_id), field[1]) != 0 ||
            copy_checked(rows[used].wallet_status, sizeof(rows[used].wallet_status), field[2]) != 0 ||
            copy_checked(rows[used].wallet_tier, sizeof(rows[used].wallet_tier), field[3]) != 0 ||
            copy_checked(rows[used].user_name_kana, sizeof(rows[used].user_name_kana), field[4]) != 0) {
            free(rows);
            fclose(fp);
            fprintf(stderr, "PYWALF項目長不正\n");
            return -1;
        }

        ++used;
    }

    if (ferror(fp)) {
        free(rows);
        fclose(fp);
        fprintf(stderr, "PYWALF読込失敗\n");
        return -1;
    }

    fclose(fp);
    *records = rows;
    *count = used;
    return 0;
}

static int read_balances(PYBALF_RECORD **records, size_t *count)
{
    FILE *fp;
    char line[入力行最大];
    size_t used = 0;
    size_t cap = 4096;
    PYBALF_RECORD *rows;

    fp = fopen("PYBALF.csv", "r");
    if (fp == NULL) {
        fprintf(stderr, "PYBALFオープン失敗\n");
        return -1;
    }

    rows = calloc(cap, sizeof(*rows));
    if (rows == NULL) {
        fclose(fp);
        fprintf(stderr, "領域確保失敗\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *field[4];
        size_t n = 0;
        line[strcspn(line, "\r\n")] = '\0';

        if (line[0] == '\0') {
            continue;
        }

        if (split_csv(line, field, 4, &n) != 0 || n != 4) {
            free(rows);
            fclose(fp);
            fprintf(stderr, "PYBALF形式不正\n");
            return -1;
        }

        if (used >= 残高件数上限) {
            free(rows);
            fclose(fp);
            fprintf(stderr, "PYBALF件数上限超過\n");
            return -1;
        }

        if (used == cap) {
            PYBALF_RECORD *next = realloc(rows, cap * 2 * sizeof(*rows));
            if (next == NULL) {
                free(rows);
                fclose(fp);
                fprintf(stderr, "領域拡張失敗\n");
                return -1;
            }
            rows = next;
            cap *= 2;
        }

        if (copy_checked(rows[used].wallet_id, sizeof(rows[used].wallet_id), field[0]) != 0 ||
            parse_amount(field[1], &rows[used].ledger_bal_amt) != 0 ||
            parse_amount(field[2], &rows[used].last_topup_amt) != 0 ||
            copy_checked(rows[used].bal_as_of_dt, sizeof(rows[used].bal_as_of_dt), field[3]) != 0 ||
            !valid_date_yyyymmdd(rows[used].bal_as_of_dt)) {
            free(rows);
            fclose(fp);
            fprintf(stderr, "PYBALF項目不正\n");
            return -1;
        }

        ++used;
    }

    if (ferror(fp)) {
        free(rows);
        fclose(fp);
        fprintf(stderr, "PYBALF読込失敗\n");
        return -1;
    }

    fclose(fp);
    *records = rows;
    *count = used;
    return 0;
}

static int cmp_wallet_record(const void *a, const void *b)
{
    const PYWALF_RECORD *x = a;
    const PYWALF_RECORD *y = b;
    return strcmp(x->wallet_id, y->wallet_id);
}

static int cmp_balance_record(const void *a, const void *b)
{
    const PYBALF_RECORD *x = a;
    const PYBALF_RECORD *y = b;
    return strcmp(x->wallet_id, y->wallet_id);
}

static int cmp_candidate(const void *a, const void *b)
{
    const EXPORT_CANDIDATE *x = a;
    const EXPORT_CANDIDATE *y = b;
    int c = strcmp(x->wallet.wallet_id, y->wallet.wallet_id);

    if (c != 0) {
        return c;
    }

    return strcmp(x->balance.bal_as_of_dt, y->balance.bal_as_of_dt);
}

static const PYWALF_RECORD *find_wallet(const PYWALF_RECORD *rows, size_t count, const char *wallet_id)
{
    size_t lo = 0;
    size_t hi = count;

    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        int c = strcmp(rows[mid].wallet_id, wallet_id);

        if (c == 0) {
            return &rows[mid];
        }

        if (c < 0) {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }

    return NULL;
}

static int build_candidates(const PYWALF_RECORD *wallets, size_t wallet_count,
                            const PYBALF_RECORD *balances, size_t balance_count,
                            EXPORT_CANDIDATE **out, size_t *out_count)
{
    EXPORT_CANDIDATE *rows;
    size_t used = 0;

    rows = calloc(balance_count == 0 ? 1 : balance_count, sizeof(*rows));
    if (rows == NULL) {
        fprintf(stderr, "候補領域確保失敗\n");
        return -1;
    }

    for (size_t i = 0; i < balance_count; ++i) {
        const PYWALF_RECORD *w = find_wallet(wallets, wallet_count, balances[i].wallet_id);

        if (w == NULL) {
            continue;
        }

        if (strcmp(w->wallet_status, "01") != 0) {
            continue;
        }

        if (balances[i].ledger_bal_amt < 照会下限残高) {
            continue;
        }

        rows[used].wallet = *w;
        rows[used].balance = balances[i];
        ++used;
    }

    qsort(rows, used, sizeof(*rows), cmp_candidate);
    *out = rows;
    *out_count = used;
    return 0;
}

static int write_ranges(const EXPORT_CANDIDATE *rows, size_t count)
{
    FILE *fp;
    size_t pos = 0;
    unsigned long range_no = 1;

    fp = fopen("MIPAY_BALANCE_RANGE.csv", "w");
    if (fp == NULL) {
        fprintf(stderr, "範囲ファイル作成失敗\n");
        return -1;
    }

    while (pos < count) {
        size_t end = pos + 1;

        while (end < count &&
               end - pos < 範囲内件数上限 &&
               strcmp(rows[end - 1].balance.bal_as_of_dt, rows[end].balance.bal_as_of_dt) == 0) {
            ++end;
        }

        if (fprintf(fp, "%lu,%s,%s,%s,%zu,A\n",
                    range_no,
                    rows[pos].wallet.wallet_id,
                    rows[end - 1].wallet.wallet_id,
                    rows[pos].balance.bal_as_of_dt,
                    end - pos) < 0) {
            fclose(fp);
            fprintf(stderr, "範囲ファイル書込失敗\n");
            return -1;
        }

        pos = end;
        ++range_no;
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "範囲ファイルクローズ失敗\n");
        return -1;
    }

    return 0;
}

int main(void)
{
    PYWALF_RECORD *wallets = NULL;
    PYBALF_RECORD *balances = NULL;
    EXPORT_CANDIDATE *candidates = NULL;
    size_t wallet_count = 0;
    size_t balance_count = 0;
    size_t candidate_count = 0;
    int rc = 異常終了;

    if (read_wallets(&wallets, &wallet_count) != 0) {
        goto cleanup;
    }

    if (read_balances(&balances, &balance_count) != 0) {
        goto cleanup;
    }

    qsort(wallets, wallet_count, sizeof(*wallets), cmp_wallet_record);
    qsort(balances, balance_count, sizeof(*balances), cmp_balance_record);

    if (build_candidates(wallets, wallet_count, balances, balance_count,
                         &candidates, &candidate_count) != 0) {
        goto cleanup;
    }

    if (write_ranges(candidates, candidate_count) != 0) {
        goto cleanup;
    }

    rc = 正常終了;

cleanup:
    free(candidates);
    free(balances);
    free(wallets);
    return rc;
}
