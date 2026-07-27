/*
 * 変更履歴
 * 版数  年月日    担当    概要
 * 1.00  20230418  今井 彩 (E-230)   初版作成。約定による建玉差分、平均単価、実現損益、リスク中間値を生成。
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
    MIHFT_DECISION_ACCEPT = 0,
    MIHFT_DECISION_REJECT_MARGIN = 4,
    MIHFT_DECISION_REJECT_NOTIONAL = 8,
    MIHFT_DECISION_REJECT_TICK = 12
};

#define MIHFT_EXEC_PATH "SCEXEC.csv"
#define MIHFT_POS_PATH "SCPOSF.csv"
#define MIHFT_RISK_PATH "HFRISKC.csv"
#define MIHFT_LINE_MAX 1024
#define MIHFT_CODE_MAX 64
#define MIHFT_TS_MAX 32

typedef struct {
    char exec_id[MIHFT_CODE_MAX];
    char order_id[MIHFT_CODE_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[MIHFT_TS_MAX];
} ExecRec;

typedef struct {
    char cif_no[MIHFT_CODE_MAX];
    char instr_code[MIHFT_CODE_MAX];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
} PosRec;

typedef struct {
    char cif_no[MIHFT_CODE_MAX];
    char instr_code[MIHFT_CODE_MAX];
    int64_t open_notional_amt;
    int reject_cnt;
    char last_upd_ts[MIHFT_TS_MAX];
} RiskRec;

static int copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t len;

    if (dst_sz == 0) {
        return -1;
    }

    len = strlen(src);
    if (len >= dst_sz) {
        return -1;
    }

    memcpy(dst, src, len + 1);
    return 0;
}

static int split_csv_line(char *line, char **field, size_t need)
{
    size_t n = 0;
    char *p = line;

    line[strcspn(line, "\r\n")] = '\0';

    while (n < need) {
        field[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p = '\0';
        ++p;
    }

    return n == need && strchr(field[need - 1], ',') == NULL ? 0 : -1;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end;
    long long v;

    if (*s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || *end != '\0') {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int abs_i64_checked(int64_t v, int64_t *out)
{
    if (v == INT64_MIN) {
        return -1;
    }
    *out = v < 0 ? -v : v;
    return 0;
}

static int add_i64_checked(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int mul_i64_checked(int64_t a, int64_t b, int64_t *out)
{
    __int128 v = (__int128)a * (__int128)b;

    if (v > INT64_MAX || v < INT64_MIN) {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int div_round_half_away_i64(int64_t num, int64_t den, int64_t *out)
{
    int neg = 0;
    int64_t q;
    int64_t r;

    if (den <= 0 || num == INT64_MIN) {
        return -1;
    }

    if (num < 0) {
        neg = 1;
        num = -num;
    }

    q = num / den;
    r = num % den;

    if (r > (den - 1) / 2) {
        if (q == INT64_MAX) {
            return -1;
        }
        ++q;
    }

    *out = neg ? -q : q;
    return 0;
}

static int parse_exec(const char *path, ExecRec *rec)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    char *f[7];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "約定ファイルを開けません\n");
        return -1;
    }

    if (fgets(line, sizeof line, fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "約定ファイルが空です\n");
        return -1;
    }

    if (strncmp(line, "EXEC-ID,", 8) == 0) {
        if (fgets(line, sizeof line, fp) == NULL) {
            fclose(fp);
            fprintf(stderr, "約定データがありません\n");
            return -1;
        }
    }

    fclose(fp);

    if (split_csv_line(line, f, 7) != 0 ||
        copy_field(rec->exec_id, sizeof rec->exec_id, f[0]) != 0 ||
        copy_field(rec->order_id, sizeof rec->order_id, f[1]) != 0 ||
        copy_field(rec->instr_code, sizeof rec->instr_code, f[2]) != 0 ||
        copy_field(rec->exec_ts, sizeof rec->exec_ts, f[6]) != 0 ||
        parse_i64(f[4], &rec->fill_qty) != 0 ||
        parse_i64(f[5], &rec->fill_amt) != 0) {
        fprintf(stderr, "約定レコード形式が不正です\n");
        return -1;
    }

    if ((f[3][0] != 'B' && f[3][0] != 'S') || f[3][1] != '\0' ||
        rec->fill_qty <= 0 || rec->fill_amt <= 0) {
        fprintf(stderr, "約定値が不正です\n");
        return -1;
    }

    rec->side_kbn = f[3][0];
    return 0;
}

static int parse_pos(const char *path, const char *instr_code, PosRec *rec)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "建玉ファイルを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char work[MIHFT_LINE_MAX];
        char *f[5];

        if (strncmp(line, "CIF-NO,", 7) == 0) {
            continue;
        }

        memcpy(work, line, strlen(line) + 1);
        if (split_csv_line(work, f, 5) != 0) {
            fclose(fp);
            fprintf(stderr, "建玉レコード形式が不正です\n");
            return -1;
        }

        if (strcmp(f[1], instr_code) != 0) {
            continue;
        }

        fclose(fp);
        if (copy_field(rec->cif_no, sizeof rec->cif_no, f[0]) != 0 ||
            copy_field(rec->instr_code, sizeof rec->instr_code, f[1]) != 0 ||
            parse_i64(f[2], &rec->net_qty) != 0 ||
            parse_i64(f[3], &rec->avg_amt) != 0 ||
            parse_i64(f[4], &rec->rlzd_amt) != 0) {
            fprintf(stderr, "建玉値が不正です\n");
            return -1;
        }
        return 0;
    }

    fclose(fp);
    fprintf(stderr, "対象建玉がありません\n");
    return -1;
}

static int calc_position_delta(const PosRec *pos, const ExecRec *exec, PosRec *next)
{
    int64_t signed_fill_qty;
    int64_t next_qty;
    int64_t abs_old_qty;
    int64_t abs_next_qty;
    int64_t old_notional;
    int64_t fill_price_x100;
    int64_t fill_notional_x100;
    int64_t close_qty;
    int old_side;
    int fill_side;

    *next = *pos;

    signed_fill_qty = exec->side_kbn == 'B' ? exec->fill_qty : -exec->fill_qty;
    if (add_i64_checked(pos->net_qty, signed_fill_qty, &next_qty) != 0 ||
        abs_i64_checked(pos->net_qty, &abs_old_qty) != 0 ||
        abs_i64_checked(next_qty, &abs_next_qty) != 0 ||
        mul_i64_checked(exec->fill_amt, 100, &fill_notional_x100) != 0 ||
        div_round_half_away_i64(fill_notional_x100, exec->fill_qty, &fill_price_x100) != 0) {
        return -1;
    }

    old_side = pos->net_qty > 0 ? 1 : pos->net_qty < 0 ? -1 : 0;
    fill_side = signed_fill_qty > 0 ? 1 : -1;
    next->net_qty = next_qty;

    if (old_side == 0 || old_side == fill_side) {
        int64_t weighted_old;
        int64_t weighted_new;
        int64_t weighted_sum;

        if (mul_i64_checked(abs_old_qty, pos->avg_amt, &weighted_old) != 0 ||
            mul_i64_checked(exec->fill_qty, fill_price_x100, &weighted_new) != 0 ||
            add_i64_checked(weighted_old, weighted_new, &weighted_sum) != 0 ||
            div_round_half_away_i64(weighted_sum, abs_next_qty, &next->avg_amt) != 0) {
            return -1;
        }
        return 0;
    }

    close_qty = abs_old_qty < exec->fill_qty ? abs_old_qty : exec->fill_qty;
    if (close_qty > 0) {
        int64_t price_diff;
        int64_t pnl_unit;
        int64_t pnl_delta;

        price_diff = fill_price_x100 - pos->avg_amt;
        pnl_unit = old_side > 0 ? price_diff : -price_diff;
        if (mul_i64_checked(close_qty, pnl_unit, &pnl_delta) != 0 ||
            add_i64_checked(pos->rlzd_amt, pnl_delta, &next->rlzd_amt) != 0) {
            return -1;
        }
    }

    if (next_qty == 0) {
        next->avg_amt = 0;
    } else if ((next_qty > 0 ? 1 : -1) != old_side) {
        next->avg_amt = fill_price_x100;
    } else {
        next->avg_amt = pos->avg_amt;
    }

    if (mul_i64_checked(abs_next_qty, next->avg_amt, &old_notional) != 0) {
        return -1;
    }

    return old_notional < 0 ? -1 : 0;
}

static int build_risk(const PosRec *pos, const ExecRec *exec, RiskRec *risk)
{
    int64_t abs_qty;

    if (copy_field(risk->cif_no, sizeof risk->cif_no, pos->cif_no) != 0 ||
        copy_field(risk->instr_code, sizeof risk->instr_code, pos->instr_code) != 0 ||
        copy_field(risk->last_upd_ts, sizeof risk->last_upd_ts, exec->exec_ts) != 0 ||
        abs_i64_checked(pos->net_qty, &abs_qty) != 0 ||
        mul_i64_checked(abs_qty, pos->avg_amt, &risk->open_notional_amt) != 0) {
        return -1;
    }

    risk->reject_cnt = risk->open_notional_amt > MIHFT_MAX_NOTIONAL ? 1 : 0;
    return 0;
}

static int write_risk(const char *path, const RiskRec *risk)
{
    FILE *fp = fopen(path, "w");

    if (fp == NULL) {
        fprintf(stderr, "リスクファイルを開けません\n");
        return -1;
    }

    if (fprintf(fp, "CIF-NO,INSTR-CODE,OPEN-NOTIONAL-AMT,REJECT-CNT,LAST-UPD-TS\n") < 0 ||
        fprintf(fp, "%s,%s,%lld,%d,%s\n",
                risk->cif_no,
                risk->instr_code,
                (long long)risk->open_notional_amt,
                risk->reject_cnt,
                risk->last_upd_ts) < 0) {
        fclose(fp);
        fprintf(stderr, "リスクファイルを書けません\n");
        return -1;
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "リスクファイルを閉じられません\n");
        return -1;
    }

    return 0;
}

int main(void)
{
    ExecRec exec;
    PosRec pos;
    PosRec next;
    RiskRec risk;
    const char *exec_path = getenv("MIHFT_SCEXEC");
    const char *pos_path = getenv("MIHFT_SCPOSF");
    const char *risk_path = getenv("MIHFT_HFRISKC");

    if (exec_path == NULL || *exec_path == '\0') {
        exec_path = MIHFT_EXEC_PATH;
    }
    if (pos_path == NULL || *pos_path == '\0') {
        pos_path = MIHFT_POS_PATH;
    }
    if (risk_path == NULL || *risk_path == '\0') {
        risk_path = MIHFT_RISK_PATH;
    }

    if (parse_exec(exec_path, &exec) != 0 ||
        parse_pos(pos_path, exec.instr_code, &pos) != 0 ||
        calc_position_delta(&pos, &exec, &next) != 0 ||
        build_risk(&next, &exec, &risk) != 0 ||
        write_risk(risk_path, &risk) != 0) {
        return 99;
    }

    if (risk.open_notional_amt > MIHFT_MAX_NOTIONAL) {
        return MIHFT_DECISION_REJECT_NOTIONAL;
    }

    return MIHFT_DECISION_ACCEPT;
}
