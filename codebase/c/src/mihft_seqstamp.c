/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210128  渡辺 隆 (E-260)  初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <stdalign.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MIHFT_DECISION_OK
#define MIHFT_DECISION_OK 0
#endif
#ifndef MIHFT_DECISION_REJECT
#define MIHFT_DECISION_REJECT 10
#endif
#ifndef MIHFT_DECISION_WRAP
#define MIHFT_DECISION_WRAP 21
#endif
#ifndef MIHFT_DECISION_TS_REGRESSION
#define MIHFT_DECISION_TS_REGRESSION 22
#endif
#ifndef MIHFT_DECISION_PARSE_ERROR
#define MIHFT_DECISION_PARSE_ERROR 64
#endif
#ifndef MIHFT_EVENT_KBN_ORDER
#define MIHFT_EVENT_KBN_ORDER 1
#endif
#ifndef MIHFT_SEQ_INITIAL
#define MIHFT_SEQ_INITIAL 0ULL
#endif
#ifndef MIHFT_SCJRNF_MAX_LINE
#define MIHFT_SCJRNF_MAX_LINE 512
#endif
#ifndef MIHFT_INSTR_CODE_MAX
#define MIHFT_INSTR_CODE_MAX 32
#endif
#ifndef MIHFT_ORDER_ID_MAX
#define MIHFT_ORDER_ID_MAX 32
#endif

typedef struct {
    uint64_t event_ts;
    int event_kbn;
    char order_id[MIHFT_ORDER_ID_MAX];
    char instr_code[MIHFT_INSTR_CODE_MAX];
    char payload[256];
} staged_event_t;

alignas(64) static uint64_t g_seq_counter = MIHFT_SEQ_INITIAL;
static uint64_t g_last_event_ts = 0ULL;

static void strip_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t n;

    if (dst_len == 0U || src == NULL) {
        return -1;
    }

    n = strlen(src);
    if (n >= dst_len) {
        return -1;
    }

    memcpy(dst, src, n + 1U);
    return 0;
}

static int parse_u64_field(const char *s, uint64_t *out)
{
    char *end = NULL;
    unsigned long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoull(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }

    *out = (uint64_t)v;
    return 0;
}

static int parse_i32_field(const char *s, int *out)
{
    char *end = NULL;
    long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0' || v < 0L || v > 9999L) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static uint64_t hash_payload(const char *s)
{
    uint64_t h = 1469598103934665603ULL;
    const unsigned char *p = (const unsigned char *)s;

    while (*p != '\0') {
        h ^= (uint64_t)*p++;
        h *= 1099511628211ULL;
    }

    return h;
}

static int parse_staged_event(char *line, staged_event_t *event)
{
    char *field[5];
    char *p = line;
    size_t i;

    for (i = 0U; i < 5U; i++) {
        field[i] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            if (i == 4U) {
                break;
            }
            return -1;
        }
        *p++ = '\0';
    }

    if (parse_u64_field(field[0], &event->event_ts) != 0) {
        return -1;
    }
    if (parse_i32_field(field[1], &event->event_kbn) != 0) {
        return -1;
    }
    if (event->event_kbn == 0) {
        event->event_kbn = MIHFT_EVENT_KBN_ORDER;
    }
    if (copy_field(event->order_id, sizeof(event->order_id), field[2]) != 0) {
        return -1;
    }
    if (copy_field(event->instr_code, sizeof(event->instr_code), field[3]) != 0) {
        return -1;
    }
    if (copy_field(event->payload, sizeof(event->payload), field[4]) != 0) {
        return -1;
    }

    return 0;
}

static int allocate_sequence(uint64_t event_ts, uint64_t *seq_no)
{
    if (event_ts < g_last_event_ts) {
        return MIHFT_DECISION_TS_REGRESSION;
    }

    if (g_seq_counter == UINT64_MAX) {
        return MIHFT_DECISION_WRAP;
    }

    g_seq_counter++;
    g_last_event_ts = event_ts;
    *seq_no = g_seq_counter;
    return MIHFT_DECISION_OK;
}

static int write_scjrnf(FILE *out, const staged_event_t *event, uint64_t seq_no)
{
    uint64_t ph = hash_payload(event->payload);

    if (fprintf(out,
                "%" PRIu64 ",%" PRIu64 ",%d,%s,%s,%016" PRIx64 "\n",
                seq_no,
                event->event_ts,
                event->event_kbn,
                event->order_id,
                event->instr_code,
                ph) < 0) {
        return -1;
    }

    return 0;
}

int main(void)
{
    char line[MIHFT_SCJRNF_MAX_LINE];
    int decision = MIHFT_DECISION_OK;

    while (fgets(line, sizeof(line), stdin) != NULL) {
        staged_event_t event;
        uint64_t seq_no = 0ULL;

        strip_eol(line);
        if (line[0] == '\0') {
            continue;
        }

        if (parse_staged_event(line, &event) != 0) {
            fputs("E_PARSE:入力レコード不正\n", stderr);
            return MIHFT_DECISION_PARSE_ERROR;
        }

        decision = allocate_sequence(event.event_ts, &seq_no);
        if (decision != MIHFT_DECISION_OK) {
            fprintf(stderr, "E_DECISION:%d\n", decision);
            return decision;
        }

        if (write_scjrnf(stdout, &event, seq_no) != 0) {
            fputs("E_IO:出力失敗\n", stderr);
            return MIHFT_DECISION_REJECT;
        }
    }

    if (ferror(stdin) != 0) {
        fputs("E_IO:入力失敗\n", stderr);
        return MIHFT_DECISION_REJECT;
    }

    if (fflush(stdout) != 0) {
        fputs("E_IO:出力確定失敗\n", stderr);
        return MIHFT_DECISION_REJECT;
    }

    return decision;
}
