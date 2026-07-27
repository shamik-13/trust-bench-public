/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20240709  市場基盤部  初版作成、HFKILL高速照会の事前判定を実装
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_DECISION_ACCEPT 0
#define MIHFT_DECISION_REJECT_NOTIONAL 8
#define MIHFT_ERR_IO 64
#define MIHFT_ERR_PARSE 65

#define MIHFT_LINE_MAX 1024
#define MIHFT_FIELD_MAX 128
#define MIHFT_SCOPE_MAX 96
#define MIHFT_REASON_MAX 32
#define MIHFT_USER_MAX 32
#define MIHFT_ID_MAX 40
#define MIHFT_REJECT_ID_MAX 48
#define MIHFT_TS_MAX 32

struct kill_record {
    char scope_key[MIHFT_SCOPE_MAX];
    int kill_flg;
    char reason_cd[MIHFT_REASON_MAX];
    char updated_ts[MIHFT_TS_MAX];
    char updated_by[MIHFT_USER_MAX];
};

struct order_record {
    char order_id[MIHFT_ID_MAX];
    char cif_no[MIHFT_ID_MAX];
    char instr_code[MIHFT_ID_MAX];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
};

struct reject_record {
    char reject_id[MIHFT_REJECT_ID_MAX];
    char order_id[MIHFT_ID_MAX];
    char cif_no[MIHFT_ID_MAX];
    char instr_code[MIHFT_ID_MAX];
    char reject_cd[MIHFT_REASON_MAX];
    char detail_cd[MIHFT_REASON_MAX];
    char reject_ts[MIHFT_TS_MAX];
};

static void trim_field(char *s)
{
    size_t len;
    char *p = s;

    while (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n') {
        ++p;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1U);
    }

    len = strlen(s);
    while (len > 0U &&
           (s[len - 1U] == ' ' || s[len - 1U] == '\t' ||
            s[len - 1U] == '\r' || s[len - 1U] == '\n')) {
        s[--len] = '\0';
    }
}

static int split_csv_line(char *line, char fields[][MIHFT_FIELD_MAX], size_t want)
{
    size_t col = 0U;
    char *p = line;

    while (col < want) {
        char *start = p;
        size_t len;

        while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
            ++p;
        }

        len = (size_t)(p - start);
        if (len >= MIHFT_FIELD_MAX) {
            return -1;
        }

        memcpy(fields[col], start, len);
        fields[col][len] = '\0';
        trim_field(fields[col]);
        ++col;

        if (*p == ',') {
            ++p;
        } else {
            break;
        }
    }

    return col == want ? 0 : -1;
}

static int parse_i64(const char *s, int64_t min_value, int64_t max_value, int64_t *out)
{
    char *end = NULL;
    intmax_t v;

    if (s[0] == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoimax(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0' || v < min_value || v > max_value) {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int parse_int(const char *s, int min_value, int max_value, int *out)
{
    int64_t v;

    if (parse_i64(s, min_value, max_value, &v) != 0) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int copy_text(char *dst, size_t dst_size, const char *src)
{
    size_t len = strlen(src);

    if (len == 0U || len >= dst_size) {
        return -1;
    }

    memcpy(dst, src, len + 1U);
    return 0;
}

static int parse_order_line(char *line, struct order_record *order)
{
    char f[9][MIHFT_FIELD_MAX];

    if (split_csv_line(line, f, 9U) != 0) {
        return -1;
    }

    if (copy_text(order->order_id, sizeof(order->order_id), f[0]) != 0 ||
        copy_text(order->cif_no, sizeof(order->cif_no), f[1]) != 0 ||
        copy_text(order->instr_code, sizeof(order->instr_code), f[2]) != 0) {
        return -1;
    }

    if ((strcmp(f[3], "B") != 0 && strcmp(f[3], "S") != 0) ||
        (strcmp(f[4], "L") != 0 && strcmp(f[4], "M") != 0) ||
        (strcmp(f[5], "DAY") != 0 && strcmp(f[5], "IOC") != 0 && strcmp(f[5], "FOK") != 0)) {
        return -1;
    }

    order->side_kbn = f[3][0];
    order->ord_type = f[4][0];
    if (copy_text(order->tif_code, sizeof(order->tif_code), f[5]) != 0) {
        return -1;
    }

    if (parse_i64(f[6], 1, INT64_MAX, &order->ord_qty) != 0 ||
        parse_i64(f[7], 0, INT64_MAX, &order->price_amt) != 0 ||
        parse_int(f[8], 1, 3, &order->instr_tier) != 0) {
        return -1;
    }

    if (order->ord_type == 'L' && order->price_amt == 0) {
        return -1;
    }

    return 0;
}

static int parse_kill_line(char *line, struct kill_record *kill)
{
    char f[5][MIHFT_FIELD_MAX];

    if (split_csv_line(line, f, 5U) != 0) {
        return -1;
    }

    if (copy_text(kill->scope_key, sizeof(kill->scope_key), f[0]) != 0 ||
        copy_text(kill->reason_cd, sizeof(kill->reason_cd), f[2]) != 0 ||
        copy_text(kill->updated_ts, sizeof(kill->updated_ts), f[3]) != 0 ||
        copy_text(kill->updated_by, sizeof(kill->updated_by), f[4]) != 0 ||
        parse_int(f[1], 0, 1, &kill->kill_flg) != 0) {
        return -1;
    }

    return 0;
}

static void make_scope_keys(const struct order_record *order,
                            char org_key[MIHFT_SCOPE_MAX],
                            char cif_key[MIHFT_SCOPE_MAX],
                            char instr_key[MIHFT_SCOPE_MAX],
                            char cif_instr_key[MIHFT_SCOPE_MAX])
{
    snprintf(org_key, MIHFT_SCOPE_MAX, "ORG");
    snprintf(cif_key, MIHFT_SCOPE_MAX, "CIF:%s", order->cif_no);
    snprintf(instr_key, MIHFT_SCOPE_MAX, "INSTR:%s", order->instr_code);
    snprintf(cif_instr_key, MIHFT_SCOPE_MAX, "CIFINSTR:%s:%s", order->cif_no, order->instr_code);
}

static int scope_rank(const char *scope_key,
                      const char *org_key,
                      const char *cif_key,
                      const char *instr_key,
                      const char *cif_instr_key)
{
    if (strcmp(scope_key, org_key) == 0) {
        return 1;
    }
    if (strcmp(scope_key, cif_key) == 0) {
        return 2;
    }
    if (strcmp(scope_key, instr_key) == 0) {
        return 3;
    }
    if (strcmp(scope_key, cif_instr_key) == 0) {
        return 4;
    }
    return 0;
}

static int read_first_order(const char *path, struct order_record *order)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "注文ファイルを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        if (strncmp(line, "ORDER-ID,", 9U) == 0 || line[0] == '\n' || line[0] == '\r') {
            continue;
        }
        if (parse_order_line(line, order) != 0) {
            fclose(fp);
            fprintf(stderr, "注文レコード形式不正: %s\n", path);
            return MIHFT_ERR_PARSE;
        }
        fclose(fp);
        return MIHFT_DECISION_ACCEPT;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "注文ファイル読込失敗: %s\n", path);
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    fprintf(stderr, "注文レコードがありません: %s\n", path);
    return MIHFT_ERR_PARSE;
}

static int find_kill(const char *path, const struct order_record *order, struct kill_record *selected)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];
    char org_key[MIHFT_SCOPE_MAX];
    char cif_key[MIHFT_SCOPE_MAX];
    char instr_key[MIHFT_SCOPE_MAX];
    char cif_instr_key[MIHFT_SCOPE_MAX];
    int best_rank = 0;

    if (fp == NULL) {
        fprintf(stderr, "キルスイッチファイルを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    make_scope_keys(order, org_key, cif_key, instr_key, cif_instr_key);

    while (fgets(line, sizeof(line), fp) != NULL) {
        struct kill_record current;
        int rank;

        if (strncmp(line, "SCOPE-KEY,", 10U) == 0 || line[0] == '\n' || line[0] == '\r') {
            continue;
        }

        if (parse_kill_line(line, &current) != 0) {
            fclose(fp);
            fprintf(stderr, "キルスイッチレコード形式不正: %s\n", path);
            return MIHFT_ERR_PARSE;
        }

        rank = scope_rank(current.scope_key, org_key, cif_key, instr_key, cif_instr_key);
        if (rank > best_rank && current.kill_flg == 1) {
            *selected = current;
            best_rank = rank;
        }
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "キルスイッチファイル読込失敗: %s\n", path);
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    return best_rank == 0 ? MIHFT_DECISION_ACCEPT : MIHFT_DECISION_REJECT_NOTIONAL;
}

static void current_ts(char out[MIHFT_TS_MAX])
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_POSIX_VERSION)
    localtime_r(&now, &tmv);
#else
    {
        struct tm *tmp = localtime(&now);
        if (tmp != NULL) {
            tmv = *tmp;
        } else {
            memset(&tmv, 0, sizeof(tmv));
        }
    }
#endif

    if (strftime(out, MIHFT_TS_MAX, "%Y%m%d%H%M%S", &tmv) == 0U) {
        snprintf(out, MIHFT_TS_MAX, "00000000000000");
    }
}

static int write_reject(const char *path, const struct order_record *order, const struct kill_record *kill)
{
    FILE *fp = fopen(path, "a");
    struct reject_record reject;

    if (fp == NULL) {
        fprintf(stderr, "拒否ファイルを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    snprintf(reject.reject_id, sizeof(reject.reject_id), "RJ%s", order->order_id);
    snprintf(reject.order_id, sizeof(reject.order_id), "%s", order->order_id);
    snprintf(reject.cif_no, sizeof(reject.cif_no), "%s", order->cif_no);
    snprintf(reject.instr_code, sizeof(reject.instr_code), "%s", order->instr_code);
    snprintf(reject.reject_cd, sizeof(reject.reject_cd), "HFKILL");
    snprintf(reject.detail_cd, sizeof(reject.detail_cd), "%s", kill->reason_cd);
    current_ts(reject.reject_ts);

    if (fprintf(fp, "%s,%s,%s,%s,%s,%s,%s\n",
                reject.reject_id,
                reject.order_id,
                reject.cif_no,
                reject.instr_code,
                reject.reject_cd,
                reject.detail_cd,
                reject.reject_ts) < 0) {
        fclose(fp);
        fprintf(stderr, "拒否ファイル書込失敗: %s\n", path);
        return MIHFT_ERR_IO;
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "拒否ファイル確定失敗: %s\n", path);
        return MIHFT_ERR_IO;
    }

    return MIHFT_DECISION_REJECT_NOTIONAL;
}

static int notional_is_over_limit(const struct order_record *order)
{
    if (order->price_amt == 0) {
        return 0;
    }
    if (order->ord_qty > INT64_MAX / order->price_amt) {
        return 1;
    }
    return order->ord_qty * order->price_amt > MIHFT_MAX_NOTIONAL;
}

int main(void)
{
    struct order_record order;
    struct kill_record kill;
    int rc;

    rc = read_first_order("SCORDF.csv", &order);
    if (rc != MIHFT_DECISION_ACCEPT) {
        return rc;
    }

    rc = find_kill("HFKILL.csv", &order, &kill);
    if (rc != MIHFT_DECISION_ACCEPT) {
        if (rc == MIHFT_DECISION_REJECT_NOTIONAL) {
            return write_reject("HFRJCT.csv", &order, &kill);
        }
        return rc;
    }

    if (notional_is_over_limit(&order)) {
        return MIHFT_DECISION_REJECT_NOTIONAL;
    }

    return MIHFT_DECISION_ACCEPT;
}
