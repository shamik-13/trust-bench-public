/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20200310  市場基盤部  初版作成
 * 1.01  20200810  市場基盤部  遅延区間別の抽出条件を追加
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_LINE_MAX 1024
#define MIHFT_FIELD_MAX 128
#define MIHFT_DECISION_ID_MAX 32
#define MIHFT_ORDER_ID_MAX 32
#define MIHFT_INSTR_CODE_MAX 32
#define MIHFT_ACTION_CODE_MAX 16
#define MIHFT_REASON_CODE_MAX 32

#ifndef MIHFT_LATENCY_NS_THRESHOLD
#define MIHFT_LATENCY_NS_THRESHOLD 7500ULL
#endif

#ifndef MIHFT_DECISION_NORMAL
#define MIHFT_DECISION_NORMAL 0
#endif

#ifndef MIHFT_ERR_IO
#define MIHFT_ERR_IO 12
#endif

#ifndef MIHFT_ERR_PARSE
#define MIHFT_ERR_PARSE 16
#endif

typedef struct {
    char decision_id[MIHFT_DECISION_ID_MAX];
    char order_id[MIHFT_ORDER_ID_MAX];
    char instr_code[MIHFT_INSTR_CODE_MAX];
    char action_code[MIHFT_ACTION_CODE_MAX];
    char reason_code[MIHFT_REASON_CODE_MAX];
    uint64_t decision_ts;
} HfdeclogRecord;

typedef struct {
    uint64_t decode_ns;
    uint64_t risk_ns;
    uint64_t route_ns;
    uint64_t presend_ns;
    uint64_t total_ns;
} LatencyProbe;

static int copy_field(char *dst, size_t dst_size, const char *src, size_t src_len)
{
    size_t left;
    size_t right;
    size_t out_len;

    left = 0U;
    right = src_len;

    while (left < right && isspace((unsigned char)src[left])) {
        left++;
    }
    while (right > left && isspace((unsigned char)src[right - 1U])) {
        right--;
    }

    if (right > left && src[left] == '"' && src[right - 1U] == '"') {
        left++;
        right--;
    }

    out_len = right - left;
    if (out_len >= dst_size) {
        return -1;
    }

    memcpy(dst, src + left, out_len);
    dst[out_len] = '\0';
    return 0;
}

static int next_csv_field(const char **cursor, char *dst, size_t dst_size)
{
    const char *p;
    const char *start;
    int quoted;

    p = *cursor;
    quoted = 0;

    while (*p == ' ' || *p == '\t') {
        p++;
    }

    start = p;
    if (*p == '"') {
        quoted = 1;
        p++;
        while (*p != '\0') {
            if (*p == '"' && p[1] == '"') {
                p += 2;
            } else if (*p == '"') {
                p++;
                break;
            } else {
                p++;
            }
        }
        while (*p == ' ' || *p == '\t') {
            p++;
        }
    } else {
        while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
            p++;
        }
    }

    if (copy_field(dst, dst_size, start, (size_t)(p - start)) != 0) {
        return -1;
    }

    if (quoted) {
        char compact[MIHFT_FIELD_MAX];
        size_t w;
        const char *q;

        w = 0U;
        q = dst;
        if (*q == '"') {
            q++;
        }
        while (*q != '\0') {
            if (*q == '"' && q[1] == '"') {
                if (w + 1U >= sizeof(compact)) {
                    return -1;
                }
                compact[w++] = '"';
                q += 2;
            } else if (*q == '"') {
                q++;
            } else {
                if (w + 1U >= sizeof(compact)) {
                    return -1;
                }
                compact[w++] = *q++;
            }
        }
        compact[w] = '\0';
        if (strlen(compact) >= dst_size) {
            return -1;
        }
        strcpy(dst, compact);
    }

    if (*p == ',') {
        p++;
    } else if (*p != '\0' && *p != '\n' && *p != '\r') {
        return -1;
    }

    *cursor = p;
    return 0;
}

static int parse_u64(const char *s, uint64_t *value)
{
    char *endp;
    unsigned long long v;

    if (*s == '\0' || *s == '-') {
        return -1;
    }

    errno = 0;
    v = strtoull(s, &endp, 10);
    if (errno != 0 || endp == s) {
        return -1;
    }

    while (*endp != '\0') {
        if (!isspace((unsigned char)*endp)) {
            return -1;
        }
        endp++;
    }

    *value = (uint64_t)v;
    return 0;
}

static int parse_hfdeclog(const char *line, HfdeclogRecord *rec)
{
    const char *cursor;
    char ts_buf[MIHFT_FIELD_MAX];

    cursor = line;

    if (next_csv_field(&cursor, rec->decision_id, sizeof(rec->decision_id)) != 0) {
        return -1;
    }
    if (next_csv_field(&cursor, rec->order_id, sizeof(rec->order_id)) != 0) {
        return -1;
    }
    if (next_csv_field(&cursor, rec->instr_code, sizeof(rec->instr_code)) != 0) {
        return -1;
    }
    if (next_csv_field(&cursor, rec->action_code, sizeof(rec->action_code)) != 0) {
        return -1;
    }
    if (next_csv_field(&cursor, rec->reason_code, sizeof(rec->reason_code)) != 0) {
        return -1;
    }
    if (next_csv_field(&cursor, ts_buf, sizeof(ts_buf)) != 0) {
        return -1;
    }
    if (parse_u64(ts_buf, &rec->decision_ts) != 0) {
        return -1;
    }

    if (rec->decision_id[0] == '\0' || rec->order_id[0] == '\0' ||
        rec->instr_code[0] == '\0' || rec->action_code[0] == '\0') {
        return -1;
    }

    return 0;
}

static uint64_t fnv1a64(const char *s, uint64_t seed)
{
    uint64_t h;

    h = seed ^ UINT64_C(1469598103934665603);
    while (*s != '\0') {
        h ^= (unsigned char)*s;
        h *= UINT64_C(1099511628211);
        s++;
    }
    return h;
}

static uint64_t bounded_jitter(uint64_t key, uint64_t width)
{
    if (width == 0U) {
        return 0U;
    }
    return key % width;
}

static void probe_latency(const HfdeclogRecord *rec, LatencyProbe *probe)
{
    uint64_t k1;
    uint64_t k2;
    uint64_t k3;
    uint64_t action_bias;
    uint64_t reason_bias;

    k1 = fnv1a64(rec->decision_id, rec->decision_ts);
    k2 = fnv1a64(rec->order_id, k1);
    k3 = fnv1a64(rec->instr_code, k2);

    action_bias = (strcmp(rec->action_code, "REJ") == 0) ? 900U : 120U;
    reason_bias = (rec->reason_code[0] == 'R') ? 450U : 80U;

    probe->decode_ns = 320U + bounded_jitter(k1, 2200U);
    probe->risk_ns = 700U + action_bias + bounded_jitter(k2, 4200U);
    probe->route_ns = 260U + bounded_jitter(k3, 1800U);
    probe->presend_ns = 180U + reason_bias + bounded_jitter(k1 ^ k3, 2400U);

    if (UINT64_MAX - probe->decode_ns < probe->risk_ns ||
        UINT64_MAX - probe->decode_ns - probe->risk_ns < probe->route_ns ||
        UINT64_MAX - probe->decode_ns - probe->risk_ns - probe->route_ns < probe->presend_ns) {
        probe->total_ns = UINT64_MAX;
        return;
    }

    probe->total_ns = probe->decode_ns + probe->risk_ns + probe->route_ns + probe->presend_ns;
}

static int should_sample(const LatencyProbe *probe)
{
    return probe->decode_ns > MIHFT_LATENCY_NS_THRESHOLD ||
           probe->risk_ns > MIHFT_LATENCY_NS_THRESHOLD ||
           probe->route_ns > MIHFT_LATENCY_NS_THRESHOLD ||
           probe->presend_ns > MIHFT_LATENCY_NS_THRESHOLD ||
           probe->total_ns > (MIHFT_LATENCY_NS_THRESHOLD * 2ULL);
}

static int write_hfdeclog(FILE *out, const HfdeclogRecord *rec)
{
    if (fprintf(out, "%s,%s,%s,%s,%s,%" PRIu64 "\n",
                rec->decision_id,
                rec->order_id,
                rec->instr_code,
                rec->action_code,
                rec->reason_code,
                rec->decision_ts) < 0) {
        return -1;
    }
    return 0;
}

static int is_header_line(const char *line)
{
    char field[MIHFT_FIELD_MAX];
    const char *cursor;

    cursor = line;
    if (next_csv_field(&cursor, field, sizeof(field)) != 0) {
        return 0;
    }

    return strcmp(field, "DECISION-ID") == 0 || strcmp(field, "DECISION_ID") == 0;
}

int main(void)
{
    const char *in_path;
    const char *out_path;
    FILE *in;
    FILE *out;
    char line[MIHFT_LINE_MAX];
    unsigned long line_no;
    int rc;

    in_path = getenv("MIHFT_HFDECLOG_IN");
    out_path = getenv("MIHFT_HFDECLOG_OUT");

    if (in_path == NULL || *in_path == '\0') {
        in_path = "HFDECLOG.csv";
    }
    if (out_path == NULL || *out_path == '\0') {
        out_path = "HFDECLOG.out.csv";
    }

    in = fopen(in_path, "r");
    if (in == NULL) {
        fprintf(stderr, "E1001:HFDECLOG入力オープン失敗:%s\n", in_path);
        return MIHFT_ERR_IO;
    }

    out = fopen(out_path, "a");
    if (out == NULL) {
        fprintf(stderr, "E1002:HFDECLOG出力オープン失敗:%s\n", out_path);
        fclose(in);
        return MIHFT_ERR_IO;
    }

    rc = MIHFT_DECISION_NORMAL;
    line_no = 0UL;

    while (fgets(line, sizeof(line), in) != NULL) {
        HfdeclogRecord rec;
        LatencyProbe probe;

        line_no++;

        if (strchr(line, '\n') == NULL && !feof(in)) {
            fprintf(stderr, "E2001:HFDECLOG行長超過:%lu\n", line_no);
            rc = MIHFT_ERR_PARSE;
            break;
        }

        if (line[0] == '\n' || line[0] == '\r' || is_header_line(line)) {
            continue;
        }

        if (parse_hfdeclog(line, &rec) != 0) {
            fprintf(stderr, "E2002:HFDECLOG解析失敗:%lu\n", line_no);
            rc = MIHFT_ERR_PARSE;
            break;
        }

        probe_latency(&rec, &probe);

        if (should_sample(&probe)) {
            if (write_hfdeclog(out, &rec) != 0) {
                fprintf(stderr, "E1003:HFDECLOG書込失敗:%lu\n", line_no);
                rc = MIHFT_ERR_IO;
                break;
            }
        }
    }

    if (ferror(in)) {
        fprintf(stderr, "E1004:HFDECLOG読込失敗\n");
        rc = MIHFT_ERR_IO;
    }

    if (fflush(out) != 0) {
        fprintf(stderr, "E1005:HFDECLOG同期失敗\n");
        rc = MIHFT_ERR_IO;
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "E1006:HFDECLOG出力クローズ失敗\n");
        rc = MIHFT_ERR_IO;
    }

    if (fclose(in) != 0) {
        fprintf(stderr, "E1007:HFDECLOG入力クローズ失敗\n");
        rc = MIHFT_ERR_IO;
    }

    return rc;
}
