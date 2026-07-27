/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    20190416    大野 修 (E-225)    初版作成
 * 1.01    20190916    渡辺 隆 (E-260)    CSV検証と欠番判定を強化
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

#define MIHFT_IO_ERROR_EXIT 70
#define MIHFT_PARSE_ERROR_EXIT 71
#define MIHFT_MAX_LINE 1024
#define MIHFT_MAX_FIELD 128
#define MIHFT_FIELD_COUNT 6

#define MIHFT_DECISION_MATCHING_CONTINUE 0
#define MIHFT_DECISION_DUPLICATE_STOP 1
#define MIHFT_DECISION_REVERSE_STOP 2
#define MIHFT_DECISION_GAP_STOP 3

struct scsessf_row {
    char sess_key[MIHFT_MAX_FIELD];
    char sess_dt[MIHFT_MAX_FIELD];
    char board_code[MIHFT_MAX_FIELD];
    char state_kbn[MIHFT_MAX_FIELD];
    uint64_t last_seq_no;
    char updated_ts[MIHFT_MAX_FIELD];
};

static int is_space_char(int c)
{
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

static char *trim_field(char *s)
{
    char *e;

    while (*s != '\0' && is_space_char((unsigned char)*s)) {
        ++s;
    }

    e = s + strlen(s);
    while (e > s && is_space_char((unsigned char)e[-1])) {
        --e;
    }
    *e = '\0';

    return s;
}

static int copy_field(char *dst, size_t dst_sz, char *src)
{
    size_t n;
    char *v = trim_field(src);

    if (*v == '\0') {
        return -1;
    }

    n = strlen(v);
    if (n >= dst_sz) {
        return -1;
    }

    memcpy(dst, v, n + 1U);
    return 0;
}

static int parse_u64_field(const char *s, uint64_t *out)
{
    char *end = NULL;
    unsigned long long v;

    if (s == NULL || *s == '\0' || *s == '-') {
        return -1;
    }

    errno = 0;
    v = strtoull(s, &end, 10);
    if (errno == ERANGE || end == s || *end != '\0') {
        return -1;
    }

    *out = (uint64_t)v;
    return 0;
}

static int split_csv_line(char *line, char *field[MIHFT_FIELD_COUNT])
{
    size_t i = 0;
    char *p = line;

    while (i < MIHFT_FIELD_COUNT) {
        field[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    if (i != MIHFT_FIELD_COUNT || strchr(field[MIHFT_FIELD_COUNT - 1U], ',') != NULL) {
        return -1;
    }

    return 0;
}

static int parse_scsessf(char *line, struct scsessf_row *row)
{
    char *field[MIHFT_FIELD_COUNT];
    char *seq_text;

    if (split_csv_line(line, field) != 0) {
        return -1;
    }

    if (copy_field(row->sess_key, sizeof(row->sess_key), field[0]) != 0 ||
        copy_field(row->sess_dt, sizeof(row->sess_dt), field[1]) != 0 ||
        copy_field(row->board_code, sizeof(row->board_code), field[2]) != 0 ||
        copy_field(row->state_kbn, sizeof(row->state_kbn), field[3]) != 0 ||
        copy_field(row->updated_ts, sizeof(row->updated_ts), field[5]) != 0) {
        return -1;
    }

    seq_text = trim_field(field[4]);
    if (parse_u64_field(seq_text, &row->last_seq_no) != 0) {
        return -1;
    }

    return 0;
}

static int make_updated_ts(char out[MIHFT_MAX_FIELD])
{
    time_t now = time(NULL);
    struct tm tmv;

    if (now == (time_t)-1) {
        return -1;
    }

#if defined(_POSIX_THREAD_SAFE_FUNCTIONS) || defined(__APPLE__)
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

    if (strftime(out, MIHFT_MAX_FIELD, "%Y%m%d%H%M%S", &tmv) != 14U) {
        return -1;
    }

    return 0;
}

static int same_session(const struct scsessf_row *a, const struct scsessf_row *b)
{
    return strcmp(a->sess_key, b->sess_key) == 0 &&
           strcmp(a->sess_dt, b->sess_dt) == 0 &&
           strcmp(a->board_code, b->board_code) == 0;
}

static int emit_scsessf(FILE *fp, const struct scsessf_row *row)
{
    if (fprintf(fp, "%s,%s,%s,%s,%" PRIu64 ",%s\n",
                row->sess_key,
                row->sess_dt,
                row->board_code,
                row->state_kbn,
                row->last_seq_no,
                row->updated_ts) < 0) {
        return -1;
    }

    return 0;
}

static int read_next_data(FILE *fp, struct scsessf_row *row)
{
    char line[MIHFT_MAX_LINE];

    while (fgets(line, sizeof(line), fp) != NULL) {
        char work[MIHFT_MAX_LINE];

        if (strchr(line, '\n') == NULL && !feof(fp)) {
            return -1;
        }

        memcpy(work, line, strlen(line) + 1U);
        if (parse_scsessf(work, row) != 0) {
            if (strncmp(trim_field(line), "SESS-KEY,", 9U) == 0) {
                continue;
            }
            return -1;
        }

        return 1;
    }

    if (ferror(fp)) {
        return -1;
    }

    return 0;
}

static int judge_sequence(uint64_t last_seq_no, uint64_t new_seq_no)
{
    if (new_seq_no == last_seq_no) {
        return MIHFT_DECISION_DUPLICATE_STOP;
    }

    if (new_seq_no < last_seq_no) {
        return MIHFT_DECISION_REVERSE_STOP;
    }

    if (last_seq_no != UINT64_MAX && new_seq_no == last_seq_no + 1U) {
        return MIHFT_DECISION_MATCHING_CONTINUE;
    }

    return MIHFT_DECISION_GAP_STOP;
}

int main(void)
{
    struct scsessf_row stored;
    struct scsessf_row incoming;
    int rc;
    int decision;

    rc = read_next_data(stdin, &stored);
    if (rc <= 0) {
        fputs("SCSESSF読込異常\n", stderr);
        return rc == 0 ? MIHFT_PARSE_ERROR_EXIT : MIHFT_IO_ERROR_EXIT;
    }

    rc = read_next_data(stdin, &incoming);
    if (rc <= 0) {
        fputs("注文連番入力異常\n", stderr);
        return rc == 0 ? MIHFT_PARSE_ERROR_EXIT : MIHFT_IO_ERROR_EXIT;
    }

    if (!same_session(&stored, &incoming)) {
        fputs("セッションキー不一致\n", stderr);
        return MIHFT_PARSE_ERROR_EXIT;
    }

    decision = judge_sequence(stored.last_seq_no, incoming.last_seq_no);
    if (decision == MIHFT_DECISION_MATCHING_CONTINUE) {
        stored.last_seq_no = incoming.last_seq_no;
        if (make_updated_ts(stored.updated_ts) != 0) {
            fputs("更新時刻生成異常\n", stderr);
            return MIHFT_IO_ERROR_EXIT;
        }
    } else {
        strncpy(stored.state_kbn, "H", sizeof(stored.state_kbn) - 1U);
        stored.state_kbn[sizeof(stored.state_kbn) - 1U] = '\0';
    }

    if (emit_scsessf(stdout, &stored) != 0 || fflush(stdout) != 0) {
        fputs("SCSESSF出力異常\n", stderr);
        return MIHFT_IO_ERROR_EXIT;
    }

    return decision;
}
