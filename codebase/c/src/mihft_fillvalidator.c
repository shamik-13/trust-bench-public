/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20220906  中川 美和 (E-283)   約定数量および価格単位検証の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_RET_IOERR  96
#define MIHFT_RET_PARSE 97
#define MIHFT_RET_NOMEM 98

#define MIHFT_ACT_ACCEPT 0
#define MIHFT_ACT_REJECT_NOTIONAL 8
#define MIHFT_ACT_REJECT_TICK 12

#define MIHFT_MAX_LINE 1024
#define MIHFT_MAX_INST 4096
#define MIHFT_KEY_LEN 32
#define MIHFT_NAME_LEN 128
#define MIHFT_TS_LEN 32

struct mihft_inst_row {
    char instr_code[MIHFT_KEY_LEN];
    char instr_name[MIHFT_NAME_LEN];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[8];
};

struct mihft_exec_row {
    char exec_id[MIHFT_KEY_LEN];
    char order_id[MIHFT_KEY_LEN];
    char instr_code[MIHFT_KEY_LEN];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[MIHFT_TS_LEN];
};

static void mihft_trim(char *s)
{
    size_t n;

    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n') {
        memmove(s, s + 1, strlen(s));
    }

    n = strlen(s);
    while (n > 0U) {
        char c = s[n - 1U];
        if (c != ' ' && c != '\t' && c != '\r' && c != '\n') {
            break;
        }
        s[--n] = '\0';
    }
}

static int mihft_field_copy(char *dst, size_t dst_sz, const char *src)
{
    size_t n;

    if (dst_sz == 0U) {
        return -1;
    }

    n = strlen(src);
    if (n >= dst_sz) {
        return -1;
    }

    memcpy(dst, src, n + 1U);
    mihft_trim(dst);
    return dst[0] == '\0' ? -1 : 0;
}

static int mihft_parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s) {
        return -1;
    }

    while (*end == ' ' || *end == '\t' || *end == '\r' || *end == '\n') {
        ++end;
    }
    if (*end != '\0') {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int mihft_parse_int(const char *s, int *out)
{
    int64_t v;

    if (mihft_parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int mihft_next_csv(char **cur, char *dst, size_t dst_sz)
{
    char *p = *cur;
    char *start;
    size_t len;

    if (p == NULL || *p == '\0') {
        return -1;
    }

    start = p;
    while (*p != '\0' && *p != ',') {
        ++p;
    }

    len = (size_t)(p - start);
    if (len >= dst_sz) {
        return -1;
    }

    memcpy(dst, start, len);
    dst[len] = '\0';
    mihft_trim(dst);

    *cur = (*p == ',') ? p + 1 : p;
    return 0;
}

static int mihft_tier_tick(int tier, int64_t *tick)
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

static int mihft_parse_inst(char *line, struct mihft_inst_row *row)
{
    char *cur = line;
    char f0[MIHFT_KEY_LEN];
    char f1[MIHFT_NAME_LEN];
    char f2[32];
    char f3[32];
    char f4[32];
    char f5[8];

    if (mihft_next_csv(&cur, f0, sizeof(f0)) != 0 ||
        mihft_next_csv(&cur, f1, sizeof(f1)) != 0 ||
        mihft_next_csv(&cur, f2, sizeof(f2)) != 0 ||
        mihft_next_csv(&cur, f3, sizeof(f3)) != 0 ||
        mihft_next_csv(&cur, f4, sizeof(f4)) != 0 ||
        mihft_next_csv(&cur, f5, sizeof(f5)) != 0) {
        return -1;
    }

    if (mihft_field_copy(row->instr_code, sizeof(row->instr_code), f0) != 0 ||
        mihft_field_copy(row->instr_name, sizeof(row->instr_name), f1) != 0 ||
        mihft_field_copy(row->board_code, sizeof(row->board_code), f5) != 0 ||
        mihft_parse_int(f2, &row->instr_tier) != 0 ||
        mihft_parse_i64(f3, &row->tick_amt) != 0 ||
        mihft_parse_i64(f4, &row->lot_qty) != 0) {
        return -1;
    }

    return 0;
}

static int mihft_parse_exec(char *line, struct mihft_exec_row *row)
{
    char *cur = line;
    char f0[MIHFT_KEY_LEN];
    char f1[MIHFT_KEY_LEN];
    char f2[MIHFT_KEY_LEN];
    char f3[8];
    char f4[32];
    char f5[32];
    char f6[MIHFT_TS_LEN];

    if (mihft_next_csv(&cur, f0, sizeof(f0)) != 0 ||
        mihft_next_csv(&cur, f1, sizeof(f1)) != 0 ||
        mihft_next_csv(&cur, f2, sizeof(f2)) != 0 ||
        mihft_next_csv(&cur, f3, sizeof(f3)) != 0 ||
        mihft_next_csv(&cur, f4, sizeof(f4)) != 0 ||
        mihft_next_csv(&cur, f5, sizeof(f5)) != 0 ||
        mihft_next_csv(&cur, f6, sizeof(f6)) != 0) {
        return -1;
    }

    if (mihft_field_copy(row->exec_id, sizeof(row->exec_id), f0) != 0 ||
        mihft_field_copy(row->order_id, sizeof(row->order_id), f1) != 0 ||
        mihft_field_copy(row->instr_code, sizeof(row->instr_code), f2) != 0 ||
        mihft_field_copy(row->exec_ts, sizeof(row->exec_ts), f6) != 0 ||
        mihft_parse_i64(f4, &row->fill_qty) != 0 ||
        mihft_parse_i64(f5, &row->fill_amt) != 0) {
        return -1;
    }

    if ((strcmp(f3, "B") != 0 && strcmp(f3, "S") != 0) || f3[1] != '\0') {
        return -1;
    }
    row->side_kbn = f3[0];

    return 0;
}

static const struct mihft_inst_row *mihft_find_inst(const struct mihft_inst_row *rows,
                                                    size_t count,
                                                    const char *instr_code)
{
    size_t i;

    for (i = 0U; i < count; ++i) {
        if (strcmp(rows[i].instr_code, instr_code) == 0) {
            return &rows[i];
        }
    }

    return NULL;
}

static int mihft_is_header(const char *line, const char *first_name)
{
    char buf[MIHFT_MAX_LINE];
    char *cur = buf;
    char first[MIHFT_KEY_LEN];

    if (strlen(line) >= sizeof(buf)) {
        return 0;
    }

    strcpy(buf, line);
    if (mihft_next_csv(&cur, first, sizeof(first)) != 0) {
        return 0;
    }

    return strcmp(first, first_name) == 0;
}

static int mihft_load_inst(const char *path,
                           struct mihft_inst_row *rows,
                           size_t cap,
                           size_t *count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];
    size_t n = 0U;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "SCINSTFを開けません\n");
        return MIHFT_RET_IOERR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        int64_t std_tick = 0;

        if (line[0] == '\n' || line[0] == '\r' || mihft_is_header(line, "INSTR-CODE")) {
            continue;
        }
        if (n >= cap) {
            fclose(fp);
            fprintf(stderr, "SCINSTF件数が上限を超過しました\n");
            return MIHFT_RET_NOMEM;
        }
        if (mihft_parse_inst(line, &rows[n]) != 0) {
            fclose(fp);
            fprintf(stderr, "SCINSTF解析に失敗しました\n");
            return MIHFT_RET_PARSE;
        }
        if (rows[n].lot_qty <= 0 || rows[n].tick_amt <= 0 ||
            mihft_tier_tick(rows[n].instr_tier, &std_tick) != 0 ||
            rows[n].tick_amt != std_tick) {
            fclose(fp);
            fprintf(stderr, "SCINSTF定義が不正です\n");
            return MIHFT_RET_PARSE;
        }
        ++n;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCINSTF読込に失敗しました\n");
        return MIHFT_RET_IOERR;
    }

    fclose(fp);
    *count = n;
    return MIHFT_ACT_ACCEPT;
}

static void mihft_now_yyyymmddhhmmss(char *dst, size_t dst_sz)
{
    time_t now = time(NULL);
    struct tm tmv;

    if (dst_sz == 0U) {
        return;
    }

#if defined(_POSIX_VERSION)
    if (localtime_r(&now, &tmv) == NULL) {
        dst[0] = '\0';
        return;
    }
#else
    {
        struct tm *tmp = localtime(&now);
        if (tmp == NULL) {
            dst[0] = '\0';
            return;
        }
        tmv = *tmp;
    }
#endif

    if (strftime(dst, dst_sz, "%Y%m%d%H%M%S", &tmv) == 0U) {
        dst[0] = '\0';
    }
}

static int mihft_decide(const struct mihft_exec_row *exec,
                        const struct mihft_inst_row *inst)
{
    int64_t px;

    if (inst == NULL || exec->fill_qty <= 0 || exec->fill_amt <= 0) {
        return MIHFT_ACT_REJECT_TICK;
    }

    if (exec->fill_qty > 0 && exec->fill_amt > MIHFT_MAX_NOTIONAL) {
        return MIHFT_ACT_REJECT_NOTIONAL;
    }

    if (exec->fill_qty % inst->lot_qty != 0) {
        return MIHFT_ACT_REJECT_TICK;
    }

    if (exec->fill_amt % exec->fill_qty != 0) {
        return MIHFT_ACT_REJECT_TICK;
    }

    px = exec->fill_amt / exec->fill_qty;
    if (px <= 0 || px % inst->tick_amt != 0) {
        return MIHFT_ACT_REJECT_TICK;
    }

    return MIHFT_ACT_ACCEPT;
}

static const char *mihft_reason_code(int action)
{
    switch (action) {
    case MIHFT_ACT_ACCEPT:
        return "OK";
    case MIHFT_ACT_REJECT_NOTIONAL:
        return "NOTIONAL";
    case MIHFT_ACT_REJECT_TICK:
        return "TICK";
    default:
        return "UNKNOWN";
    }
}

static int mihft_write_decision(FILE *out,
                                const struct mihft_exec_row *exec,
                                int action)
{
    char ts[MIHFT_TS_LEN];

    mihft_now_yyyymmddhhmmss(ts, sizeof(ts));
    if (ts[0] == '\0') {
        return MIHFT_RET_IOERR;
    }

    if (fprintf(out, "%s,%s,%s,%d,%s,%s\n",
                exec->exec_id,
                exec->order_id,
                exec->instr_code,
                action,
                mihft_reason_code(action),
                ts) < 0) {
        return MIHFT_RET_IOERR;
    }

    return MIHFT_ACT_ACCEPT;
}

int main(void)
{
    const char *exec_path = getenv("MIHFT_SCEXEC");
    const char *inst_path = getenv("MIHFT_SCINSTF");
    const char *out_path = getenv("MIHFT_HFDECLOG");
    struct mihft_inst_row insts[MIHFT_MAX_INST];
    size_t inst_count = 0U;
    FILE *exec_fp;
    FILE *out_fp;
    char line[MIHFT_MAX_LINE];
    int final_code = MIHFT_ACT_ACCEPT;
    int rc;

    if (exec_path == NULL || exec_path[0] == '\0') {
        exec_path = "SCEXEC.csv";
    }
    if (inst_path == NULL || inst_path[0] == '\0') {
        inst_path = "SCINSTF.csv";
    }
    if (out_path == NULL || out_path[0] == '\0') {
        out_path = "HFDECLOG";
    }

    rc = mihft_load_inst(inst_path, insts, MIHFT_MAX_INST, &inst_count);
    if (rc != MIHFT_ACT_ACCEPT) {
        return rc;
    }

    exec_fp = fopen(exec_path, "r");
    if (exec_fp == NULL) {
        fprintf(stderr, "SCEXECを開けません\n");
        return MIHFT_RET_IOERR;
    }

    out_fp = fopen(out_path, "w");
    if (out_fp == NULL) {
        fclose(exec_fp);
        fprintf(stderr, "HFDECLOGを開けません\n");
        return MIHFT_RET_IOERR;
    }

    while (fgets(line, sizeof(line), exec_fp) != NULL) {
        struct mihft_exec_row exec;
        const struct mihft_inst_row *inst;
        int action;

        if (line[0] == '\n' || line[0] == '\r' || mihft_is_header(line, "EXEC-ID")) {
            continue;
        }

        if (mihft_parse_exec(line, &exec) != 0) {
            fclose(out_fp);
            fclose(exec_fp);
            fprintf(stderr, "SCEXEC解析に失敗しました\n");
            return MIHFT_RET_PARSE;
        }

        inst = mihft_find_inst(insts, inst_count, exec.instr_code);
        action = mihft_decide(&exec, inst);

        rc = mihft_write_decision(out_fp, &exec, action);
        if (rc != MIHFT_ACT_ACCEPT) {
            fclose(out_fp);
            fclose(exec_fp);
            fprintf(stderr, "HFDECLOG書込に失敗しました\n");
            return rc;
        }

        if (action != MIHFT_ACT_ACCEPT) {
            final_code = action;
        }
    }

    if (ferror(exec_fp)) {
        fclose(out_fp);
        fclose(exec_fp);
        fprintf(stderr, "SCEXEC読込に失敗しました\n");
        return MIHFT_RET_IOERR;
    }

    if (fclose(out_fp) != 0) {
        fclose(exec_fp);
        fprintf(stderr, "HFDECLOG終了処理に失敗しました\n");
        return MIHFT_RET_IOERR;
    }

    fclose(exec_fp);
    return final_code;
}
