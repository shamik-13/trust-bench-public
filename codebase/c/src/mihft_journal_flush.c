/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20200310  市場基盤部  初版作成
 * 1.01  20200810  市場基盤部  セッション単位の境界判定を追加
 * 1.02  20210110  市場基盤部  永続化前の連番穴検証を追加
 */
#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef MIHFT_DECISION_OK
#define MIHFT_DECISION_OK 0
#endif

#ifndef MIHFT_DECISION_WARN
#define MIHFT_DECISION_WARN 1
#endif

#define 局所_RING_CAPACITY 4096u
#define 局所_FIELD_MAX 96u
#define 局所_LINE_MAX 512u
#define 局所_PATH_MAX 256u
#define 局所_SESS_KEY_MAX 32u
#define 局所_DATE_MAX 16u
#define 局所_BOARD_MAX 16u
#define 局所_STATE_MAX 8u
#define 局所_TS_MAX 32u

typedef struct {
    char sess_key[局所_SESS_KEY_MAX];
    char sess_dt[局所_DATE_MAX];
    char board_code[局所_BOARD_MAX];
    char state_kbn[局所_STATE_MAX];
    unsigned long long seq_no;
    char event_ts[局所_TS_MAX];
} 局所_order_event_t;

typedef struct {
    局所_order_event_t rows[局所_RING_CAPACITY];
    size_t head;
    size_t count;
} 局所_event_ring_t;

typedef struct {
    char sess_key[局所_SESS_KEY_MAX];
    char sess_dt[局所_DATE_MAX];
    char board_code[局所_BOARD_MAX];
    char state_kbn[局所_STATE_MAX];
    unsigned long long first_seq;
    unsigned long long last_seq;
    size_t events;
} 局所_session_flush_t;

static int 局所_copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t len;

    if (dst == NULL || src == NULL || dst_sz == 0u) {
        return -1;
    }

    len = strlen(src);
    if (len == 0u || len >= dst_sz) {
        return -1;
    }

    memcpy(dst, src, len + 1u);
    return 0;
}

static int 局所_parse_u64(const char *text, unsigned long long *out)
{
    char *endp;
    unsigned long long value;

    if (text == NULL || *text == '\0' || out == NULL) {
        return -1;
    }

    errno = 0;
    value = strtoull(text, &endp, 10);
    if (errno == ERANGE || endp == text || *endp != '\0') {
        return -1;
    }

    *out = value;
    return 0;
}

static int 局所_next_csv_field(char **cursor, char *field, size_t field_sz)
{
    char *p;
    size_t n;

    if (cursor == NULL || *cursor == NULL || field == NULL || field_sz == 0u) {
        return -1;
    }

    p = *cursor;
    n = 0u;

    while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
        if (n + 1u >= field_sz) {
            return -1;
        }
        field[n++] = *p++;
    }
    field[n] = '\0';

    if (*p == ',') {
        p++;
    } else {
        while (*p == '\n' || *p == '\r') {
            p++;
        }
    }

    *cursor = p;
    return 0;
}

static int 局所_parse_event_line(char *line, 局所_order_event_t *event)
{
    char *cursor;
    char seq_text[局所_FIELD_MAX];

    if (line == NULL || event == NULL) {
        return -1;
    }

    cursor = line;
    memset(event, 0, sizeof(*event));

    if (局所_next_csv_field(&cursor, event->sess_key, sizeof(event->sess_key)) != 0) {
        return -1;
    }
    if (局所_next_csv_field(&cursor, event->sess_dt, sizeof(event->sess_dt)) != 0) {
        return -1;
    }
    if (局所_next_csv_field(&cursor, event->board_code, sizeof(event->board_code)) != 0) {
        return -1;
    }
    if (局所_next_csv_field(&cursor, event->state_kbn, sizeof(event->state_kbn)) != 0) {
        return -1;
    }
    if (局所_next_csv_field(&cursor, seq_text, sizeof(seq_text)) != 0) {
        return -1;
    }
    if (局所_next_csv_field(&cursor, event->event_ts, sizeof(event->event_ts)) != 0) {
        return -1;
    }
    if (局所_parse_u64(seq_text, &event->seq_no) != 0 || event->seq_no == 0ULL) {
        return -1;
    }

    return 0;
}

static int 局所_ring_push(局所_event_ring_t *ring, const 局所_order_event_t *event)
{
    size_t pos;

    if (ring == NULL || event == NULL || ring->count >= 局所_RING_CAPACITY) {
        return -1;
    }

    pos = (ring->head + ring->count) % 局所_RING_CAPACITY;
    ring->rows[pos] = *event;
    ring->count++;
    return 0;
}

static int 局所_ring_pop(局所_event_ring_t *ring, 局所_order_event_t *event)
{
    if (ring == NULL || event == NULL || ring->count == 0u) {
        return -1;
    }

    *event = ring->rows[ring->head];
    ring->head = (ring->head + 1u) % 局所_RING_CAPACITY;
    ring->count--;
    return 0;
}

static int 局所_same_session(const 局所_order_event_t *a, const 局所_order_event_t *b)
{
    if (a == NULL || b == NULL) {
        return 0;
    }

    return strcmp(a->sess_key, b->sess_key) == 0
        && strcmp(a->sess_dt, b->sess_dt) == 0
        && strcmp(a->board_code, b->board_code) == 0
        && strcmp(a->state_kbn, b->state_kbn) == 0;
}

static int 局所_flush_init(局所_session_flush_t *flush, const 局所_order_event_t *event)
{
    if (flush == NULL || event == NULL) {
        return -1;
    }

    memset(flush, 0, sizeof(*flush));
    if (局所_copy_field(flush->sess_key, sizeof(flush->sess_key), event->sess_key) != 0) {
        return -1;
    }
    if (局所_copy_field(flush->sess_dt, sizeof(flush->sess_dt), event->sess_dt) != 0) {
        return -1;
    }
    if (局所_copy_field(flush->board_code, sizeof(flush->board_code), event->board_code) != 0) {
        return -1;
    }
    if (局所_copy_field(flush->state_kbn, sizeof(flush->state_kbn), event->state_kbn) != 0) {
        return -1;
    }

    flush->first_seq = event->seq_no;
    flush->last_seq = event->seq_no;
    flush->events = 1u;
    return 0;
}

static int 局所_seq_gap_guard(const 局所_session_flush_t *flush, const 局所_order_event_t *event)
{
    if (flush == NULL || event == NULL) {
        return -1;
    }
    if (flush->events == 0u) {
        return 0;
    }
    if (flush->last_seq == ULLONG_MAX) {
        return -1;
    }
    if (event->seq_no != flush->last_seq + 1ULL) {
        return -1;
    }

    return 0;
}

static int 局所_flush_append(局所_session_flush_t *flush, const 局所_order_event_t *event)
{
    if (flush == NULL || event == NULL) {
        return -1;
    }

    if (局所_seq_gap_guard(flush, event) != 0) {
        return -1;
    }

    flush->last_seq = event->seq_no;
    flush->events++;
    return 0;
}

static int 局所_now_ts(char *dst, size_t dst_sz)
{
    time_t now;
    struct tm tmv;

    if (dst == NULL || dst_sz < 20u) {
        return -1;
    }

    now = time(NULL);
    if (now == (time_t)-1) {
        return -1;
    }

#if defined(_POSIX_VERSION)
    if (localtime_r(&now, &tmv) == NULL) {
        return -1;
    }
#else
    {
        struct tm *tmp = localtime(&now);
        if (tmp == NULL) {
            return -1;
        }
        tmv = *tmp;
    }
#endif

    if (strftime(dst, dst_sz, "%Y%m%d%H%M%S", &tmv) == 0u) {
        return -1;
    }

    return 0;
}

static int 局所_write_scsessf(FILE *out, const 局所_session_flush_t *flush)
{
    char updated_ts[局所_TS_MAX];

    if (out == NULL || flush == NULL || flush->events == 0u) {
        return -1;
    }

    if (局所_now_ts(updated_ts, sizeof(updated_ts)) != 0) {
        return -1;
    }

    if (fprintf(out, "%s,%s,%s,%s,%llu,%s\n",
                flush->sess_key,
                flush->sess_dt,
                flush->board_code,
                flush->state_kbn,
                flush->last_seq,
                updated_ts) < 0) {
        return -1;
    }

    if (fflush(out) != 0) {
        return -1;
    }

    return 0;
}

static int 局所_load_ring(FILE *in, 局所_event_ring_t *ring)
{
    char line[局所_LINE_MAX];
    unsigned long line_no;

    if (in == NULL || ring == NULL) {
        return -1;
    }

    line_no = 0UL;
    memset(ring, 0, sizeof(*ring));

    while (fgets(line, sizeof(line), in) != NULL) {
        局所_order_event_t event;

        line_no++;
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '#') {
            continue;
        }
        if (line_no == 1UL && strncmp(line, "SESS-KEY,", 9u) == 0) {
            continue;
        }
        if (strchr(line, '\n') == NULL && !feof(in)) {
            fprintf(stderr, "行長超過:%lu\n", line_no);
            return -1;
        }
        if (局所_parse_event_line(line, &event) != 0) {
            fprintf(stderr, "入力不正:%lu\n", line_no);
            return -1;
        }
        if (局所_ring_push(ring, &event) != 0) {
            fprintf(stderr, "リング満杯:%lu\n", line_no);
            return -1;
        }
    }

    if (ferror(in)) {
        fprintf(stderr, "入力読取失敗\n");
        return -1;
    }

    return 0;
}

static int 局所_process_ring(局所_event_ring_t *ring, FILE *out)
{
    局所_session_flush_t flush;
    局所_order_event_t event;
    int active;

    if (ring == NULL || out == NULL) {
        return -1;
    }

    memset(&flush, 0, sizeof(flush));
    active = 0;

    while (局所_ring_pop(ring, &event) == 0) {
        if (!active) {
            if (局所_flush_init(&flush, &event) != 0) {
                fprintf(stderr, "境界初期化失敗\n");
                return -1;
            }
            active = 1;
            continue;
        }

        if (局所_same_session((const 局所_order_event_t *)&flush, &event)) {
            if (局所_flush_append(&flush, &event) != 0) {
                fprintf(stderr, "連番穴検出:%s:%llu:%llu\n",
                        flush.sess_key, flush.last_seq, event.seq_no);
                return -1;
            }
            continue;
        }

        if (局所_write_scsessf(out, &flush) != 0) {
            fprintf(stderr, "SCSESSF書込失敗:%s\n", flush.sess_key);
            return -1;
        }
        if (局所_flush_init(&flush, &event) != 0) {
            fprintf(stderr, "境界再初期化失敗\n");
            return -1;
        }
    }

    if (active && 局所_write_scsessf(out, &flush) != 0) {
        fprintf(stderr, "SCSESSF最終書込失敗:%s\n", flush.sess_key);
        return -1;
    }

    return 0;
}

int main(void)
{
    const char *input_path;
    const char *output_path;
    FILE *in;
    FILE *out;
    局所_event_ring_t ring;
    int rc;

    input_path = getenv("MIHFT_ORDER_EVENT_CSV");
    output_path = getenv("MIHFT_SCSESSF_CSV");

    if (input_path == NULL || *input_path == '\0') {
        input_path = "SCORDEVT.csv";
    }
    if (output_path == NULL || *output_path == '\0') {
        output_path = "SCSESSF.csv";
    }

    in = fopen(input_path, "r");
    if (in == NULL) {
        fprintf(stderr, "入力オープン失敗:%s\n", input_path);
        return 20;
    }

    out = fopen(output_path, "w");
    if (out == NULL) {
        fprintf(stderr, "SCSESSFオープン失敗:%s\n", output_path);
        fclose(in);
        return 21;
    }

    if (fprintf(out, "SESS-KEY,SESS-DT,BOARD-CODE,STATE-KBN,LAST-SEQ-NO,UPDATED-TS\n") < 0) {
        fprintf(stderr, "SCSESSF見出し書込失敗\n");
        fclose(out);
        fclose(in);
        return 22;
    }

    rc = 局所_load_ring(in, &ring);
    if (fclose(in) != 0 && rc == 0) {
        fprintf(stderr, "入力クローズ失敗\n");
        rc = -1;
    }
    if (rc != 0) {
        fclose(out);
        return 23;
    }

    rc = 局所_process_ring(&ring, out);
    if (fclose(out) != 0 && rc == 0) {
        fprintf(stderr, "SCSESSFクローズ失敗\n");
        rc = -1;
    }
    if (rc != 0) {
        return 24;
    }

    return MIHFT_DECISION_OK;
}
