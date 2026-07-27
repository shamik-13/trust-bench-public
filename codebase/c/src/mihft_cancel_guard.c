/*
 * 変更履歴
 * 版数  年月日    担当    概要
 * 1.00  20240213  西村 亮 (E-204)    取消・訂正ガードの判定前成果物を作成
 */

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mihft_types.h"

enum {
    MIHFT_DEC_ACCEPT = 0,
    MIHFT_DEC_REJECT_MARGIN = 4,
    MIHFT_DEC_REJECT_NOTIONAL = 8,
    MIHFT_DEC_REJECT_TICK = 12,
    MIHFT_PARSE_ERROR = 99
};

#define MIHFT_MAX_LINE 1024
#define MIHFT_MAX_FIELD 10
#define MIHFT_MAX_ORDERS 20000
#define MIHFT_MAX_EXEC_COUNTS 20000
#define MIHFT_RATE_LIMIT_AMT 2000000000LL

typedef struct {
    char order_id[40];
    char cif_no[32];
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[8];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} OrderRow;

typedef struct {
    char order_id[40];
    char instr_code[32];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
} ExecSum;

typedef struct {
    char decision_id[40];
    char order_id[40];
    char cif_no[32];
    char instr_code[32];
    int decision_cd;
    char reason_cd[32];
    int64_t notional_amt;
    int64_t limit_used_amt;
    char decision_ts[32];
} DecisionRow;

static size_t trim_copy(char *dst, size_t dst_len, const char *src)
{
    const unsigned char *head = (const unsigned char *)src;
    const unsigned char *tail;
    size_t len;

    while (*head != '\0' && isspace(*head)) {
        ++head;
    }

    tail = head + strlen((const char *)head);
    while (tail > head && isspace(*(tail - 1))) {
        --tail;
    }

    len = (size_t)(tail - head);
    if (dst_len == 0) {
        return len;
    }
    if (len >= dst_len) {
        len = dst_len - 1;
    }

    memcpy(dst, head, len);
    dst[len] = '\0';
    return len;
}

static int split_csv(char *line, char *fields[], size_t max_fields, size_t *count)
{
    size_t n = 0;
    char *p = line;

    while (*p != '\0') {
        char *out;

        if (n >= max_fields) {
            return -1;
        }

        fields[n++] = p;
        out = p;

        if (*p == '"') {
            ++p;
            fields[n - 1] = out;
            while (*p != '\0') {
                if (*p == '"' && p[1] == '"') {
                    *out++ = '"';
                    p += 2;
                } else if (*p == '"') {
                    ++p;
                    break;
                } else {
                    *out++ = *p++;
                }
            }
            while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
                if (!isspace((unsigned char)*p)) {
                    return -1;
                }
                ++p;
            }
        } else {
            while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
                *out++ = *p++;
            }
        }

        *out = '\0';

        if (*p == ',') {
            ++p;
            if (*p == '\0' || *p == '\n' || *p == '\r') {
                if (n >= max_fields) {
                    return -1;
                }
                fields[n++] = p;
                *p = '\0';
                break;
            }
        } else {
            break;
        }
    }

    *count = n;
    return 0;
}

static int parse_i64(const char *text, int64_t *value)
{
    char buf[64];
    char *end = NULL;
    long long v;

    trim_copy(buf, sizeof(buf), text);
    if (buf[0] == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(buf, &end, 10);
    if (errno != 0 || end == buf) {
        return -1;
    }
    while (*end != '\0') {
        if (!isspace((unsigned char)*end)) {
            return -1;
        }
        ++end;
    }

    *value = (int64_t)v;
    return 0;
}

static int parse_int(const char *text, int *value)
{
    int64_t v;

    if (parse_i64(text, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }
    *value = (int)v;
    return 0;
}

static int is_header_line(char *fields[], size_t count, const char *first_name)
{
    char buf[64];

    if (count == 0) {
        return 0;
    }
    trim_copy(buf, sizeof(buf), fields[0]);
    return strcmp(buf, first_name) == 0;
}

static int checked_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a < 0 || b < 0) {
        return -1;
    }
    if (a != 0 && b > INT64_MAX / a) {
        return -1;
    }
    *out = a * b;
    return 0;
}

static int tier_rate_bp(int tier, int *rate_bp)
{
    switch (tier) {
    case 1:
        *rate_bp = 1000;
        return 0;
    case 2:
        *rate_bp = 2000;
        return 0;
    case 3:
        *rate_bp = 4000;
        return 0;
    default:
        return -1;
    }
}

static int tier_tick(int tier, int64_t *tick)
{
    switch (tier) {
    case 1:
        *tick = 100;
        return 0;
    case 2:
        *tick = 500;
        return 0;
    case 3:
        *tick = 1000;
        return 0;
    default:
        return -1;
    }
}

static int parse_order_line(char *line, OrderRow *row, int *skip)
{
    char *fields[MIHFT_MAX_FIELD];
    size_t count = 0;
    char side_buf[8];
    char type_buf[8];

    *skip = 0;
    if (split_csv(line, fields, MIHFT_MAX_FIELD, &count) != 0) {
        return -1;
    }
    if (count == 0 || (count == 1 && fields[0][0] == '\0')) {
        *skip = 1;
        return 0;
    }
    if (is_header_line(fields, count, "ORDER-ID")) {
        *skip = 1;
        return 0;
    }
    if (count != 9) {
        return -1;
    }

    trim_copy(row->order_id, sizeof(row->order_id), fields[0]);
    trim_copy(row->cif_no, sizeof(row->cif_no), fields[1]);
    trim_copy(row->instr_code, sizeof(row->instr_code), fields[2]);
    trim_copy(side_buf, sizeof(side_buf), fields[3]);
    trim_copy(type_buf, sizeof(type_buf), fields[4]);
    trim_copy(row->tif_code, sizeof(row->tif_code), fields[5]);

    if (side_buf[1] != '\0' || (side_buf[0] != 'B' && side_buf[0] != 'S')) {
        return -1;
    }
    if (type_buf[1] != '\0' || (type_buf[0] != 'L' && type_buf[0] != 'M')) {
        return -1;
    }

    row->side_kbn = side_buf[0];
    row->ord_type = type_buf[0];

    if (parse_i64(fields[6], &row->ord_qty) != 0 ||
        parse_i64(fields[7], &row->price_amt) != 0 ||
        parse_int(fields[8], &row->instr_tier) != 0) {
        return -1;
    }
    if (row->order_id[0] == '\0' || row->cif_no[0] == '\0' ||
        row->instr_code[0] == '\0' || row->ord_qty <= 0 ||
        row->price_amt < 0 || tier_tick(row->instr_tier, &(int64_t){0}) != 0) {
        return -1;
    }

    return 0;
}

static int parse_exec_line(char *line, ExecSum *row, int *skip)
{
    char *fields[MIHFT_MAX_FIELD];
    size_t count = 0;
    char side_buf[8];

    *skip = 0;
    if (split_csv(line, fields, MIHFT_MAX_FIELD, &count) != 0) {
        return -1;
    }
    if (count == 0 || (count == 1 && fields[0][0] == '\0')) {
        *skip = 1;
        return 0;
    }
    if (is_header_line(fields, count, "EXEC-ID")) {
        *skip = 1;
        return 0;
    }
    if (count != 7) {
        return -1;
    }

    trim_copy(row->order_id, sizeof(row->order_id), fields[1]);
    trim_copy(row->instr_code, sizeof(row->instr_code), fields[2]);
    trim_copy(side_buf, sizeof(side_buf), fields[3]);

    if (side_buf[1] != '\0' || (side_buf[0] != 'B' && side_buf[0] != 'S')) {
        return -1;
    }

    row->side_kbn = side_buf[0];
    if (parse_i64(fields[4], &row->fill_qty) != 0 ||
        parse_i64(fields[5], &row->fill_amt) != 0) {
        return -1;
    }
    if (row->order_id[0] == '\0' || row->instr_code[0] == '\0' ||
        row->fill_qty <= 0 || row->fill_amt < 0) {
        return -1;
    }

    return 0;
}

static int parse_decision_line(char *line, DecisionRow *row, int *skip)
{
    char *fields[MIHFT_MAX_FIELD];
    size_t count = 0;

    *skip = 0;
    if (split_csv(line, fields, MIHFT_MAX_FIELD, &count) != 0) {
        return -1;
    }
    if (count == 0 || (count == 1 && fields[0][0] == '\0')) {
        *skip = 1;
        return 0;
    }
    if (is_header_line(fields, count, "DECISION-ID")) {
        *skip = 1;
        return 0;
    }
    if (count != 9) {
        return -1;
    }

    trim_copy(row->decision_id, sizeof(row->decision_id), fields[0]);
    trim_copy(row->order_id, sizeof(row->order_id), fields[1]);
    trim_copy(row->cif_no, sizeof(row->cif_no), fields[2]);
    trim_copy(row->instr_code, sizeof(row->instr_code), fields[3]);
    trim_copy(row->reason_cd, sizeof(row->reason_cd), fields[5]);
    trim_copy(row->decision_ts, sizeof(row->decision_ts), fields[8]);

    if (parse_int(fields[4], &row->decision_cd) != 0 ||
        parse_i64(fields[6], &row->notional_amt) != 0 ||
        parse_i64(fields[7], &row->limit_used_amt) != 0) {
        return -1;
    }
    if (row->order_id[0] == '\0' || row->cif_no[0] == '\0' ||
        row->instr_code[0] == '\0' || row->notional_amt < 0 ||
        row->limit_used_amt < 0) {
        return -1;
    }

    return 0;
}

static ExecSum *find_exec_sum(ExecSum sums[], size_t count, const char *order_id,
                              const char *instr_code, char side_kbn)
{
    size_t i;

    for (i = 0; i < count; ++i) {
        if (strcmp(sums[i].order_id, order_id) == 0 &&
            strcmp(sums[i].instr_code, instr_code) == 0 &&
            sums[i].side_kbn == side_kbn) {
            return &sums[i];
        }
    }

    return NULL;
}

static int load_exec_sums(const char *path, ExecSum sums[], size_t cap, size_t *count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];

    *count = 0;
    fp = fopen(path, "r");
    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        ExecSum row;
        ExecSum *sum;
        int skip;

        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fclose(fp);
            return -1;
        }
        if (parse_exec_line(line, &row, &skip) != 0) {
            fclose(fp);
            return -1;
        }
        if (skip) {
            continue;
        }

        sum = find_exec_sum(sums, *count, row.order_id, row.instr_code, row.side_kbn);
        if (sum == NULL) {
            if (*count >= cap) {
                fclose(fp);
                return -1;
            }
            sums[*count] = row;
            ++(*count);
        } else {
            if (row.fill_qty > INT64_MAX - sum->fill_qty ||
                row.fill_amt > INT64_MAX - sum->fill_amt) {
                fclose(fp);
                return -1;
            }
            sum->fill_qty += row.fill_qty;
            sum->fill_amt += row.fill_amt;
        }
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    return fclose(fp) == 0 ? 0 : -1;
}

static int load_latest_limit(const char *path, int64_t *limit_used)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];

    *limit_used = 0;
    fp = fopen(path, "r");
    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        DecisionRow row;
        int skip;

        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fclose(fp);
            return -1;
        }
        if (parse_decision_line(line, &row, &skip) != 0) {
            fclose(fp);
            return -1;
        }
        if (skip) {
            continue;
        }
        *limit_used = row.limit_used_amt;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    return fclose(fp) == 0 ? 0 : -1;
}

static int write_reject(FILE *fp, unsigned long long seq, const OrderRow *order,
                        int reject_cd, const char *detail_cd)
{
    if (fprintf(fp, "RJ%012llu,%s,%s,%s,%d,%s,20250115235959\n",
                seq, order->order_id, order->cif_no, order->instr_code,
                reject_cd, detail_cd) < 0) {
        return -1;
    }
    return 0;
}

static int judge_order(const OrderRow *order, const ExecSum *exec_sum,
                       int64_t current_limit, int *decision_cd)
{
    int64_t filled_qty = 0;
    int64_t remaining_qty;
    int64_t tick;
    int64_t notional;
    int64_t margin_amt;
    int rate_bp;

    if (exec_sum != NULL) {
        filled_qty = exec_sum->fill_qty;
    }

    if (filled_qty >= order->ord_qty) {
        *decision_cd = MIHFT_DEC_REJECT_NOTIONAL;
        return 0;
    }

    remaining_qty = order->ord_qty - filled_qty;

    if (tier_tick(order->instr_tier, &tick) != 0 ||
        tier_rate_bp(order->instr_tier, &rate_bp) != 0) {
        *decision_cd = MIHFT_DEC_REJECT_TICK;
        return 0;
    }

    if (order->ord_type == 'L' && (order->price_amt <= 0 || order->price_amt % tick != 0)) {
        *decision_cd = MIHFT_DEC_REJECT_TICK;
        return 0;
    }

    if (order->ord_type == 'M') {
        if (order->price_amt != 0) {
            *decision_cd = MIHFT_DEC_REJECT_TICK;
            return 0;
        }
        notional = 0;
    } else if (checked_mul_i64(remaining_qty, order->price_amt, &notional) != 0) {
        return -1;
    }

    if (notional > MIHFT_MAX_NOTIONAL) {
        *decision_cd = MIHFT_DEC_REJECT_NOTIONAL;
        return 0;
    }

    if (checked_mul_i64(notional, (int64_t)rate_bp, &margin_amt) != 0) {
        return -1;
    }
    margin_amt = (margin_amt + 9999) / 10000;

    if (margin_amt > MIHFT_RATE_LIMIT_AMT ||
        current_limit > MIHFT_RATE_LIMIT_AMT - margin_amt) {
        *decision_cd = MIHFT_DEC_REJECT_MARGIN;
        return 0;
    }

    *decision_cd = MIHFT_DEC_ACCEPT;
    return 0;
}

int main(void)
{
    static ExecSum exec_sums[MIHFT_MAX_EXEC_COUNTS];
    FILE *order_fp;
    FILE *reject_fp;
    char line[MIHFT_MAX_LINE];
    size_t exec_count = 0;
    int64_t limit_used = 0;
    int final_cd = MIHFT_DEC_ACCEPT;
    unsigned long long reject_seq = 1;

    if (load_exec_sums("SCEXEC.csv", exec_sums, MIHFT_MAX_EXEC_COUNTS, &exec_count) != 0) {
        return MIHFT_PARSE_ERROR;
    }
    if (load_latest_limit("HFDEC.dat", &limit_used) != 0) {
        return MIHFT_PARSE_ERROR;
    }

    order_fp = fopen("SCORDF.csv", "r");
    if (order_fp == NULL) {
        return MIHFT_PARSE_ERROR;
    }

    reject_fp = fopen("HFRJCT.dat", "w");
    if (reject_fp == NULL) {
        fclose(order_fp);
        return MIHFT_PARSE_ERROR;
    }

    while (fgets(line, sizeof(line), order_fp) != NULL) {
        OrderRow order;
        ExecSum *exec_sum;
        int skip;
        int decision_cd;

        if (strchr(line, '\n') == NULL && !feof(order_fp)) {
            fclose(reject_fp);
            fclose(order_fp);
            return MIHFT_PARSE_ERROR;
        }

        if (parse_order_line(line, &order, &skip) != 0) {
            fclose(reject_fp);
            fclose(order_fp);
            return MIHFT_PARSE_ERROR;
        }
        if (skip) {
            continue;
        }

        exec_sum = find_exec_sum(exec_sums, exec_count, order.order_id,
                                 order.instr_code, order.side_kbn);
        if (judge_order(&order, exec_sum, limit_used, &decision_cd) != 0) {
            fclose(reject_fp);
            fclose(order_fp);
            return MIHFT_PARSE_ERROR;
        }

        if (decision_cd != MIHFT_DEC_ACCEPT) {
            const char *detail_cd = "CANCEL_GUARD";
            if (exec_sum != NULL && exec_sum->fill_qty >= order.ord_qty) {
                detail_cd = "FILLED_ALL";
            } else if (decision_cd == MIHFT_DEC_REJECT_TICK) {
                detail_cd = "TICK_NG";
            } else if (decision_cd == MIHFT_DEC_REJECT_MARGIN) {
                detail_cd = "RATE_NG";
            } else if (decision_cd == MIHFT_DEC_REJECT_NOTIONAL) {
                detail_cd = "NOTIONAL_NG";
            }

            if (write_reject(reject_fp, reject_seq, &order, decision_cd, detail_cd) != 0) {
                fclose(reject_fp);
                fclose(order_fp);
                return MIHFT_PARSE_ERROR;
            }
            ++reject_seq;
            if (final_cd == MIHFT_DEC_ACCEPT || decision_cd > final_cd) {
                final_cd = decision_cd;
            }
        }
    }

    if (ferror(order_fp) || fclose(order_fp) != 0) {
        fclose(reject_fp);
        return MIHFT_PARSE_ERROR;
    }
    if (fclose(reject_fp) != 0) {
        return MIHFT_PARSE_ERROR;
    }

    return final_cd;
}
