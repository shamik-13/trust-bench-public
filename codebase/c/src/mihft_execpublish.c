/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20210715  市場基盤部  約定配信準備の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_RC_IOERR 2
#define MIHFT_RC_PARSEERR 3
#define MIHFT_RC_NOMEM 5

#define MIHFT_DEC_ACCEPT 0
#define MIHFT_DEC_REJECT_NOTIONAL 8

#define MIHFT_EXEC_FILE "SCEXEC.csv"
#define MIHFT_POS_FILE "SCPOSF.csv"

#define MIHFT_MAX_LINE 512
#define MIHFT_MAX_EXEC 4096
#define MIHFT_MAX_POS 4096
#define MIHFT_ID_LEN 32
#define MIHFT_INSTR_LEN 24
#define MIHFT_SIDE_LEN 2
#define MIHFT_TS_LEN 32

typedef struct {
    char exec_id[MIHFT_ID_LEN];
    char order_id[MIHFT_ID_LEN];
    char instr_code[MIHFT_INSTR_LEN];
    char side_kbn[MIHFT_SIDE_LEN];
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[MIHFT_TS_LEN];
} mihft_exec_rec;

typedef struct {
    char cif_no[MIHFT_ID_LEN];
    char instr_code[MIHFT_INSTR_LEN];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
} mihft_pos_rec;

typedef struct {
    char exec_id[MIHFT_ID_LEN];
    char order_id[MIHFT_ID_LEN];
    char instr_code[MIHFT_INSTR_LEN];
    char side_kbn[MIHFT_SIDE_LEN];
    int64_t fill_qty;
    int64_t fill_amt;
    int64_t after_net_qty;
    int64_t after_avg_amt;
    int64_t after_rlzd_amt;
    int decision_code;
} mihft_publish_event;

static int copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t len;

    if (dst_sz == 0U) {
        return MIHFT_RC_PARSEERR;
    }

    len = strlen(src);
    while (len > 0U && (src[len - 1U] == '\n' || src[len - 1U] == '\r')) {
        len--;
    }
    if (len == 0U || len >= dst_sz) {
        return MIHFT_RC_PARSEERR;
    }

    memcpy(dst, src, len);
    dst[len] = '\0';
    return MIHFT_DEC_ACCEPT;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *endp;
    long long v;

    if (s == NULL || *s == '\0') {
        return MIHFT_RC_PARSEERR;
    }

    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || endp == s) {
        return MIHFT_RC_PARSEERR;
    }
    while (*endp == '\r' || *endp == '\n') {
        endp++;
    }
    if (*endp != '\0') {
        return MIHFT_RC_PARSEERR;
    }

    *out = (int64_t)v;
    return MIHFT_DEC_ACCEPT;
}

static int next_field(char **cursor, char **field)
{
    char *p;
    char *comma;

    if (cursor == NULL || *cursor == NULL || field == NULL) {
        return MIHFT_RC_PARSEERR;
    }

    p = *cursor;
    comma = strchr(p, ',');
    if (comma != NULL) {
        *comma = '\0';
        *cursor = comma + 1;
    } else {
        *cursor = NULL;
    }

    *field = p;
    return MIHFT_DEC_ACCEPT;
}

static int is_header_line(const char *line)
{
    return strncmp(line, "EXEC-ID,", 8U) == 0 ||
           strncmp(line, "CIF-NO,", 7U) == 0;
}

static int parse_exec_line(char *line, mihft_exec_rec *rec)
{
    char *cur = line;
    char *f;

    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || copy_field(rec->exec_id, sizeof(rec->exec_id), f) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }
    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || copy_field(rec->order_id, sizeof(rec->order_id), f) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }
    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || copy_field(rec->instr_code, sizeof(rec->instr_code), f) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }
    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || copy_field(rec->side_kbn, sizeof(rec->side_kbn), f) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }
    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || parse_i64(f, &rec->fill_qty) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }
    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || parse_i64(f, &rec->fill_amt) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }
    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || cur != NULL || copy_field(rec->exec_ts, sizeof(rec->exec_ts), f) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }

    if ((rec->side_kbn[0] != 'B' && rec->side_kbn[0] != 'S') || rec->side_kbn[1] != '\0') {
        return MIHFT_RC_PARSEERR;
    }
    if (rec->fill_qty <= 0 || rec->fill_amt <= 0) {
        return MIHFT_RC_PARSEERR;
    }

    return MIHFT_DEC_ACCEPT;
}

static int parse_pos_line(char *line, mihft_pos_rec *rec)
{
    char *cur = line;
    char *f;

    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || copy_field(rec->cif_no, sizeof(rec->cif_no), f) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }
    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || copy_field(rec->instr_code, sizeof(rec->instr_code), f) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }
    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || parse_i64(f, &rec->net_qty) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }
    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || parse_i64(f, &rec->avg_amt) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }
    if (next_field(&cur, &f) != MIHFT_DEC_ACCEPT || cur != NULL || parse_i64(f, &rec->rlzd_amt) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }

    return MIHFT_DEC_ACCEPT;
}

static int read_execs(mihft_exec_rec *execs, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];

    fp = fopen(MIHFT_EXEC_FILE, "r");
    if (fp == NULL) {
        fprintf(stderr, "約定ファイルを開けません\n");
        return MIHFT_RC_IOERR;
    }

    *count = 0U;
    while (fgets(line, sizeof(line), fp) != NULL) {
        if (is_header_line(line)) {
            continue;
        }
        if (*count >= cap) {
            fclose(fp);
            fprintf(stderr, "約定件数が上限を超過しました\n");
            return MIHFT_RC_PARSEERR;
        }
        if (parse_exec_line(line, &execs[*count]) != MIHFT_DEC_ACCEPT) {
            fclose(fp);
            fprintf(stderr, "約定レコード形式が不正です\n");
            return MIHFT_RC_PARSEERR;
        }
        (*count)++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "約定ファイル読込に失敗しました\n");
        return MIHFT_RC_IOERR;
    }

    fclose(fp);
    return MIHFT_DEC_ACCEPT;
}

static int read_positions(mihft_pos_rec *positions, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];

    fp = fopen(MIHFT_POS_FILE, "r");
    if (fp == NULL) {
        fprintf(stderr, "建玉ファイルを開けません\n");
        return MIHFT_RC_IOERR;
    }

    *count = 0U;
    while (fgets(line, sizeof(line), fp) != NULL) {
        if (is_header_line(line)) {
            continue;
        }
        if (*count >= cap) {
            fclose(fp);
            fprintf(stderr, "建玉件数が上限を超過しました\n");
            return MIHFT_RC_PARSEERR;
        }
        if (parse_pos_line(line, &positions[*count]) != MIHFT_DEC_ACCEPT) {
            fclose(fp);
            fprintf(stderr, "建玉レコード形式が不正です\n");
            return MIHFT_RC_PARSEERR;
        }
        (*count)++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "建玉ファイル読込に失敗しました\n");
        return MIHFT_RC_IOERR;
    }

    fclose(fp);
    return MIHFT_DEC_ACCEPT;
}

static mihft_pos_rec *find_position(mihft_pos_rec *positions, size_t count, const char *instr_code)
{
    size_t i;

    for (i = 0U; i < count; i++) {
        if (strcmp(positions[i].instr_code, instr_code) == 0) {
            return &positions[i];
        }
    }
    return NULL;
}

static int add_i64_checked(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return MIHFT_RC_PARSEERR;
    }
    *out = a + b;
    return MIHFT_DEC_ACCEPT;
}

static int update_position_if_needed(mihft_pos_rec *pos, const mihft_exec_rec *exec)
{
    int64_t signed_qty;
    int64_t new_qty;
    int64_t gross_cost;
    int64_t added_cost;

    signed_qty = (exec->side_kbn[0] == 'B') ? exec->fill_qty : -exec->fill_qty;

    if (exec->side_kbn[0] == 'B') {
        if (pos->net_qty >= exec->fill_qty) {
            return MIHFT_DEC_ACCEPT;
        }
        if (pos->net_qty > 0 && pos->avg_amt > INT64_MAX / pos->net_qty) {
            return MIHFT_RC_PARSEERR;
        }
        gross_cost = pos->net_qty > 0 ? pos->avg_amt * pos->net_qty : 0;
        if (add_i64_checked(gross_cost, exec->fill_amt, &added_cost) != MIHFT_DEC_ACCEPT) {
            return MIHFT_RC_PARSEERR;
        }
        if (add_i64_checked(pos->net_qty, signed_qty, &new_qty) != MIHFT_DEC_ACCEPT || new_qty <= 0) {
            return MIHFT_RC_PARSEERR;
        }
        pos->avg_amt = added_cost / new_qty;
        pos->net_qty = new_qty;
        return MIHFT_DEC_ACCEPT;
    }

    if (pos->net_qty <= -exec->fill_qty) {
        return MIHFT_DEC_ACCEPT;
    }
    if (add_i64_checked(pos->net_qty, signed_qty, &new_qty) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }
    if (pos->avg_amt > 0 && exec->fill_qty > 0) {
        int64_t cost_basis = pos->avg_amt * exec->fill_qty;
        int64_t realized_delta = exec->fill_amt - cost_basis;
        if (pos->avg_amt > INT64_MAX / exec->fill_qty ||
            add_i64_checked(pos->rlzd_amt, realized_delta, &pos->rlzd_amt) != MIHFT_DEC_ACCEPT) {
            return MIHFT_RC_PARSEERR;
        }
    }
    pos->net_qty = new_qty;
    if (pos->net_qty == 0) {
        pos->avg_amt = 0;
    }

    return MIHFT_DEC_ACCEPT;
}

static int build_event(const mihft_exec_rec *exec, const mihft_pos_rec *pos, mihft_publish_event *event)
{
    int decision;

    if (exec->fill_amt > MIHFT_MAX_NOTIONAL) {
        decision = MIHFT_DEC_REJECT_NOTIONAL;
    } else {
        decision = MIHFT_DEC_ACCEPT;
    }

    if (copy_field(event->exec_id, sizeof(event->exec_id), exec->exec_id) != MIHFT_DEC_ACCEPT ||
        copy_field(event->order_id, sizeof(event->order_id), exec->order_id) != MIHFT_DEC_ACCEPT ||
        copy_field(event->instr_code, sizeof(event->instr_code), exec->instr_code) != MIHFT_DEC_ACCEPT ||
        copy_field(event->side_kbn, sizeof(event->side_kbn), exec->side_kbn) != MIHFT_DEC_ACCEPT) {
        return MIHFT_RC_PARSEERR;
    }

    event->fill_qty = exec->fill_qty;
    event->fill_amt = exec->fill_amt;
    event->after_net_qty = pos->net_qty;
    event->after_avg_amt = pos->avg_amt;
    event->after_rlzd_amt = pos->rlzd_amt;
    event->decision_code = decision;

    return decision;
}

static void publish_event(const mihft_publish_event *event)
{
    printf("%s,%s,%s,%s,%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 ",%d\n",
           event->exec_id,
           event->order_id,
           event->instr_code,
           event->side_kbn,
           event->fill_qty,
           event->fill_amt,
           event->after_net_qty,
           event->after_avg_amt,
           event->after_rlzd_amt,
           event->decision_code);
}

int main(void)
{
    mihft_exec_rec *execs;
    mihft_pos_rec *positions;
    size_t exec_count;
    size_t pos_count;
    size_t i;
    int final_decision = MIHFT_DEC_ACCEPT;

    execs = malloc(sizeof(*execs) * MIHFT_MAX_EXEC);
    positions = malloc(sizeof(*positions) * MIHFT_MAX_POS);
    if (execs == NULL || positions == NULL) {
        free(execs);
        free(positions);
        fprintf(stderr, "作業領域を確保できません\n");
        return MIHFT_RC_NOMEM;
    }

    if (read_execs(execs, MIHFT_MAX_EXEC, &exec_count) != MIHFT_DEC_ACCEPT) {
        free(execs);
        free(positions);
        return MIHFT_RC_PARSEERR;
    }
    if (read_positions(positions, MIHFT_MAX_POS, &pos_count) != MIHFT_DEC_ACCEPT) {
        free(execs);
        free(positions);
        return MIHFT_RC_PARSEERR;
    }

    for (i = 0U; i < exec_count; i++) {
        mihft_pos_rec *pos;
        mihft_publish_event event;
        int rc;

        pos = find_position(positions, pos_count, execs[i].instr_code);
        if (pos == NULL) {
            fprintf(stderr, "対象建玉が存在しません\n");
            free(execs);
            free(positions);
            return MIHFT_RC_PARSEERR;
        }

        rc = update_position_if_needed(pos, &execs[i]);
        if (rc != MIHFT_DEC_ACCEPT) {
            fprintf(stderr, "建玉更新に失敗しました\n");
            free(execs);
            free(positions);
            return rc;
        }

        rc = build_event(&execs[i], pos, &event);
        if (rc != MIHFT_DEC_ACCEPT && rc != MIHFT_DEC_REJECT_NOTIONAL) {
            fprintf(stderr, "配信イベント作成に失敗しました\n");
            free(execs);
            free(positions);
            return rc;
        }
        if (rc != MIHFT_DEC_ACCEPT) {
            final_decision = rc;
        }

        publish_event(&event);
    }

    free(execs);
    free(positions);
    return final_decision;
}
