/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240601  精算連携  初版作成
 * 1.01  20240605  精算連携  キー欠落時の明示コード設定を追加
 * 1.02  20240712  精算連携  ヘッダ定義判定と整数書式の不備を修正
 * 1.03  20240920  精算連携  未定義判定名への依存を排除
 */

#include "mipay_trace.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINE_MAX_LEN 512u
#define FIELD_COUNT 6u

#define TRACE_KEY_LEN 32u
#define HOLD_ID_LEN 16u
#define CAP_ID_LEN 16u
#define SETTLE_TXN_ID_LEN 32u
#define MERCHANT_CODE_LEN 24u
#define CHECK_RESULT_LEN 16u
#define DETAIL_ID_LEN 16u

#define MISSING_SETTLE_TXN_ID "MISS-SETTLE-TXN-ID"
#define MISSING_MERCHANT_CODE "MISS-MERCHANT-CODE"
#define MISSING_DETAIL_ID "MISS-DETAIL-ID"
#define DETAIL_PREFIX "DTL"

#define TRACE_DECISION_OK_VALUE 0
#define TRACE_DECISION_REJECT_VALUE 1

#ifndef MIPAY_TRACE_DECISION_PARSE_ERROR
#define MIPAY_TRACE_DECISION_PARSE_ERROR 2
#endif

#ifndef MIPAY_TRACE_DECISION_IO_ERROR
#define MIPAY_TRACE_DECISION_IO_ERROR 3
#endif

struct ptkeyf_record {
    char trace_key[TRACE_KEY_LEN + 1u];
    char hold_id[HOLD_ID_LEN + 1u];
    char cap_id[CAP_ID_LEN + 1u];
    char settle_txn_id[SETTLE_TXN_ID_LEN + 1u];
    char merchant_code[MERCHANT_CODE_LEN + 1u];
    char check_result[CHECK_RESULT_LEN + 1u];
    char detail_id[DETAIL_ID_LEN + 1u];
    int has_missing;
};

static void trim_right(char *value)
{
    size_t len = strlen(value);

    while (len > 0u) {
        unsigned char tail = (unsigned char)value[len - 1u];
        if (tail != ' ' && tail != '\t' && tail != '\r' && tail != '\n') {
            break;
        }
        value[--len] = '\0';
    }
}

static char *trim_left(char *value)
{
    while (*value == ' ' || *value == '\t') {
        ++value;
    }
    return value;
}

static int copy_fixed(char *out, size_t out_len, const char *in)
{
    size_t len;

    if (out_len == 0u) {
        return -1;
    }

    len = strlen(in);
    if (len >= out_len) {
        return -1;
    }

    memcpy(out, in, len + 1u);
    return 0;
}

static int split_csv(char *line, char *fields[], size_t field_limit, size_t *field_count)
{
    size_t count = 0u;
    char *scan = line;

    while (*scan != '\0' && count < field_limit) {
        char *start;
        char *write;

        if (*scan == '"') {
            ++scan;
            start = scan;
            write = scan;

            for (;;) {
                if (*scan == '\0') {
                    return -1;
                }
                if (*scan == '"') {
                    if (scan[1] == '"') {
                        *write++ = '"';
                        scan += 2;
                        continue;
                    }
                    ++scan;
                    break;
                }
                *write++ = *scan++;
            }

            *write = '\0';
            if (*scan == ',') {
                ++scan;
            } else if (*scan != '\0' && *scan != '\n' && *scan != '\r') {
                return -1;
            }
        } else {
            start = scan;
            while (*scan != '\0' && *scan != ',' && *scan != '\n' && *scan != '\r') {
                ++scan;
            }
            if (*scan == ',') {
                *scan++ = '\0';
            } else {
                *scan = '\0';
            }
        }

        start = trim_left(start);
        trim_right(start);
        fields[count++] = start;
    }

    *field_count = count;
    return (*scan == '\0' || count == field_limit) ? 0 : -1;
}

static int is_safe_token(const char *value)
{
    const unsigned char *scan = (const unsigned char *)value;

    if (*scan == '\0') {
        return 0;
    }

    while (*scan != '\0') {
        unsigned char c = *scan++;
        if ((c >= (unsigned char)'0' && c <= (unsigned char)'9') ||
            (c >= (unsigned char)'A' && c <= (unsigned char)'Z') ||
            (c >= (unsigned char)'a' && c <= (unsigned char)'z') ||
            c == (unsigned char)'-' ||
            c == (unsigned char)'_' ||
            c == (unsigned char)'.') {
            continue;
        }
        return 0;
    }

    return 1;
}

static uint32_t fnv1a32(const char *value)
{
    uint32_t hash = UINT32_C(2166136261);

    while (*value != '\0') {
        hash ^= (unsigned char)*value++;
        hash *= UINT32_C(16777619);
    }

    return hash;
}

static int make_detail_id(char *out, size_t out_len, const struct ptkeyf_record *rec)
{
    uint32_t hash;
    int written;

    if (rec->trace_key[0] == '\0' || rec->hold_id[0] == '\0' || rec->cap_id[0] == '\0') {
        return copy_fixed(out, out_len, MISSING_DETAIL_ID);
    }

    hash = fnv1a32(rec->trace_key);
    hash ^= fnv1a32(rec->hold_id) << 1;
    hash ^= fnv1a32(rec->cap_id) >> 1;

    written = snprintf(out, out_len, "%s%08" PRIX32, DETAIL_PREFIX, hash);
    return (written > 0 && (size_t)written < out_len) ? 0 : -1;
}

static int read_record(char *line, struct ptkeyf_record *rec)
{
    char *fields[FIELD_COUNT];
    size_t count = 0u;

    memset(rec, 0, sizeof(*rec));

    if (split_csv(line, fields, FIELD_COUNT, &count) != 0 || count != FIELD_COUNT) {
        return -1;
    }

    if (copy_fixed(rec->trace_key, sizeof(rec->trace_key), fields[0]) != 0 ||
        copy_fixed(rec->hold_id, sizeof(rec->hold_id), fields[1]) != 0 ||
        copy_fixed(rec->cap_id, sizeof(rec->cap_id), fields[2]) != 0 ||
        copy_fixed(rec->settle_txn_id, sizeof(rec->settle_txn_id), fields[3]) != 0 ||
        copy_fixed(rec->merchant_code, sizeof(rec->merchant_code), fields[4]) != 0 ||
        copy_fixed(rec->check_result, sizeof(rec->check_result), fields[5]) != 0) {
        return -1;
    }

    if (rec->settle_txn_id[0] == '\0') {
        if (copy_fixed(rec->settle_txn_id, sizeof(rec->settle_txn_id), MISSING_SETTLE_TXN_ID) != 0) {
            return -1;
        }
        rec->has_missing = 1;
    }

    if (rec->merchant_code[0] == '\0') {
        if (copy_fixed(rec->merchant_code, sizeof(rec->merchant_code), MISSING_MERCHANT_CODE) != 0) {
            return -1;
        }
        rec->has_missing = 1;
    }

    if (make_detail_id(rec->detail_id, sizeof(rec->detail_id), rec) != 0) {
        return -1;
    }

    if (strcmp(rec->detail_id, MISSING_DETAIL_ID) == 0) {
        rec->has_missing = 1;
    }

    return 0;
}

static int inspect_record(const struct ptkeyf_record *rec)
{
    if (!is_safe_token(rec->trace_key) ||
        !is_safe_token(rec->hold_id) ||
        !is_safe_token(rec->cap_id) ||
        !is_safe_token(rec->settle_txn_id) ||
        !is_safe_token(rec->merchant_code) ||
        !is_safe_token(rec->detail_id)) {
        return TRACE_DECISION_REJECT_VALUE;
    }

    if (rec->has_missing) {
        return TRACE_DECISION_REJECT_VALUE;
    }

    if (strcmp(rec->check_result, "OK") != 0 &&
        strcmp(rec->check_result, "PASS") != 0 &&
        strcmp(rec->check_result, "ACCEPT") != 0) {
        return TRACE_DECISION_REJECT_VALUE;
    }

    return TRACE_DECISION_OK_VALUE;
}

static int write_csv_field(FILE *fp, const char *value)
{
    const unsigned char *scan = (const unsigned char *)value;
    int quoted = 0;

    while (*scan != '\0') {
        if (*scan == (unsigned char)',' ||
            *scan == (unsigned char)'"' ||
            *scan == (unsigned char)'\r' ||
            *scan == (unsigned char)'\n') {
            quoted = 1;
            break;
        }
        ++scan;
    }

    if (!quoted) {
        return fputs(value, fp) < 0 ? -1 : 0;
    }

    if (fputc('"', fp) == EOF) {
        return -1;
    }

    while (*value != '\0') {
        if (*value == '"') {
            if (fputc('"', fp) == EOF) {
                return -1;
            }
        }
        if (fputc((unsigned char)*value++, fp) == EOF) {
            return -1;
        }
    }

    return fputc('"', fp) == EOF ? -1 : 0;
}

static int write_record(FILE *fp, const struct ptkeyf_record *rec, int decision)
{
    char state[32];
    int written = snprintf(state, sizeof(state), "%s:%d",
                           rec->has_missing ? "MISSING" : "PACKED",
                           decision);

    if (written <= 0 || (size_t)written >= sizeof(state)) {
        return -1;
    }

    if (write_csv_field(fp, rec->trace_key) != 0 || fputc(',', fp) == EOF ||
        write_csv_field(fp, rec->hold_id) != 0 || fputc(',', fp) == EOF ||
        write_csv_field(fp, rec->cap_id) != 0 || fputc(',', fp) == EOF ||
        write_csv_field(fp, rec->settle_txn_id) != 0 || fputc(',', fp) == EOF ||
        write_csv_field(fp, rec->merchant_code) != 0 || fputc(',', fp) == EOF ||
        write_csv_field(fp, rec->check_result) != 0 || fputc(',', fp) == EOF ||
        write_csv_field(fp, rec->detail_id) != 0 || fputc(',', fp) == EOF ||
        write_csv_field(fp, state) != 0 || fputc('\n', fp) == EOF) {
        return -1;
    }

    return 0;
}

int main(void)
{
    const char *input_name = getenv("PTKEYF_IN");
    const char *output_name = getenv("PTKEYF_OUT");
    FILE *input;
    FILE *output;
    char line[LINE_MAX_LEN];
    int final_decision = TRACE_DECISION_OK_VALUE;
    unsigned long line_no = 0ul;

    if (input_name == NULL || input_name[0] == '\0') {
        input_name = "ptkeyf.csv";
    }
    if (output_name == NULL || output_name[0] == '\0') {
        output_name = "ptkeyf.out.csv";
    }

    input = fopen(input_name, "r");
    if (input == NULL) {
        fprintf(stderr, "E-PTKEYF-OPEN-IN:%d\n", errno);
        return MIPAY_TRACE_DECISION_IO_ERROR;
    }

    output = fopen(output_name, "w");
    if (output == NULL) {
        fprintf(stderr, "E-PTKEYF-OPEN-OUT:%d\n", errno);
        fclose(input);
        return MIPAY_TRACE_DECISION_IO_ERROR;
    }

    while (fgets(line, sizeof(line), input) != NULL) {
        struct ptkeyf_record rec;
        int decision;

        ++line_no;

        if (strchr(line, '\n') == NULL && !feof(input)) {
            fprintf(stderr, "E-PTKEYF-LINE-LONG:%lu\n", line_no);
            fclose(output);
            fclose(input);
            return MIPAY_TRACE_DECISION_PARSE_ERROR;
        }

        trim_right(line);
        if (line[0] == '\0') {
            continue;
        }

        if (read_record(line, &rec) != 0) {
            fprintf(stderr, "E-PTKEYF-PARSE:%lu\n", line_no);
            fclose(output);
            fclose(input);
            return MIPAY_TRACE_DECISION_PARSE_ERROR;
        }

        decision = inspect_record(&rec);
        if (decision != TRACE_DECISION_OK_VALUE && final_decision == TRACE_DECISION_OK_VALUE) {
            final_decision = decision;
        }

        if (write_record(output, &rec, decision) != 0) {
            fprintf(stderr, "E-PTKEYF-WRITE:%lu\n", line_no);
            fclose(output);
            fclose(input);
            return MIPAY_TRACE_DECISION_IO_ERROR;
        }
    }

    if (ferror(input)) {
        fprintf(stderr, "E-PTKEYF-READ:%d\n", errno);
        fclose(output);
        fclose(input);
        return MIPAY_TRACE_DECISION_IO_ERROR;
    }

    if (fclose(output) != 0) {
        fprintf(stderr, "E-PTKEYF-CLOSE-OUT:%d\n", errno);
        fclose(input);
        return MIPAY_TRACE_DECISION_IO_ERROR;
    }

    if (fclose(input) != 0) {
        fprintf(stderr, "E-PTKEYF-CLOSE-IN:%d\n", errno);
        return MIPAY_TRACE_DECISION_IO_ERROR;
    }

    return final_decision;
}
