/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220222  開発一課  初版作成
 * 1.01  20220722  開発一課  実現損益増分計算と日次損益反映を追加
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_DEC_ACCEPT 0
#define MIHFT_DEC_REJECT_MARGIN 4
#define MIHFT_DEC_REJECT_NOTIONAL 8
#define MIHFT_DEC_REJECT_TICK 12

#define MIHFT_EXEC_IN "SCEXEC.csv"
#define MIHFT_POS_IN "SCPOSF.csv"
#define MIHFT_POS_OUT "SCPOSF.out.csv"
#define MIHFT_PNL_OUT "SCPNLF.dat"

#define MIHFT_LINE_MAX 1024
#define MIHFT_ID_MAX 64
#define MIHFT_CODE_MAX 32
#define MIHFT_TS_MAX 32
#define MIHFT_DATE_MAX 9

typedef struct {
    char exec_id[MIHFT_ID_MAX];
    char order_id[MIHFT_ID_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[MIHFT_TS_MAX];
} mihft_exec_rec;

typedef struct {
    char cif_no[MIHFT_ID_MAX];
    char instr_code[MIHFT_CODE_MAX];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
} mihft_pos_rec;

typedef struct {
    mihft_pos_rec *v;
    size_t n;
    size_t cap;
} mihft_pos_vec;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static char *mihft_trim(char *s)
{
    char *e;

    while (*s != '\0' && isspace((unsigned char)*s)) {
        ++s;
    }

    e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) {
        *--e = '\0';
    }

    return s;
}

static int mihft_next_field(char **cursor, char *out, size_t out_sz)
{
    char *p = *cursor;
    char *q;
    size_t len;

    if (out_sz == 0 || p == NULL) {
        return -1;
    }

    q = strchr(p, ',');
    if (q != NULL) {
        len = (size_t)(q - p);
        *cursor = q + 1;
    } else {
        len = strlen(p);
        *cursor = NULL;
    }

    if (len >= out_sz) {
        return -1;
    }

    memcpy(out, p, len);
    out[len] = '\0';
    memmove(out, mihft_trim(out), strlen(mihft_trim(out)) + 1);

    return 0;
}

static int mihft_parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *mihft_trim(end) != '\0') {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int mihft_parse_exec_line(char *line, mihft_exec_rec *rec)
{
    char *cur = line;
    char qty[32];
    char amt[32];
    char side[8];

    if (mihft_next_field(&cur, rec->exec_id, sizeof(rec->exec_id)) != 0 ||
        mihft_next_field(&cur, rec->order_id, sizeof(rec->order_id)) != 0 ||
        mihft_next_field(&cur, rec->instr_code, sizeof(rec->instr_code)) != 0 ||
        mihft_next_field(&cur, side, sizeof(side)) != 0 ||
        mihft_next_field(&cur, qty, sizeof(qty)) != 0 ||
        mihft_next_field(&cur, amt, sizeof(amt)) != 0 ||
        mihft_next_field(&cur, rec->exec_ts, sizeof(rec->exec_ts)) != 0 ||
        cur != NULL) {
        return -1;
    }

    if ((side[0] != 'B' && side[0] != 'S') || side[1] != '\0') {
        return -1;
    }
    rec->side_kbn = side[0];

    if (mihft_parse_i64(qty, &rec->fill_qty) != 0 ||
        mihft_parse_i64(amt, &rec->fill_amt) != 0 ||
        rec->fill_qty <= 0 ||
        rec->fill_amt <= 0) {
        return -1;
    }

    return 0;
}

static int mihft_parse_pos_line(char *line, mihft_pos_rec *rec)
{
    char *cur = line;
    char net[32];
    char avg[32];
    char rlzd[32];

    if (mihft_next_field(&cur, rec->cif_no, sizeof(rec->cif_no)) != 0 ||
        mihft_next_field(&cur, rec->instr_code, sizeof(rec->instr_code)) != 0 ||
        mihft_next_field(&cur, net, sizeof(net)) != 0 ||
        mihft_next_field(&cur, avg, sizeof(avg)) != 0 ||
        mihft_next_field(&cur, rlzd, sizeof(rlzd)) != 0 ||
        cur != NULL) {
        return -1;
    }

    if (mihft_parse_i64(net, &rec->net_qty) != 0 ||
        mihft_parse_i64(avg, &rec->avg_amt) != 0 ||
        mihft_parse_i64(rlzd, &rec->rlzd_amt) != 0 ||
        rec->avg_amt < 0) {
        return -1;
    }

    return 0;
}

static int mihft_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }

    *out = a + b;
    return 0;
}

static int mihft_sub_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b < 0 && a > INT64_MAX + b) || (b > 0 && a < INT64_MIN + b)) {
        return -1;
    }

    *out = a - b;
    return 0;
}

static int mihft_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a != 0 && b != 0) {
        if (a == -1 && b == INT64_MIN) {
            return -1;
        }
        if (b == -1 && a == INT64_MIN) {
            return -1;
        }
        if (a > 0) {
            if ((b > 0 && a > INT64_MAX / b) || (b < 0 && b < INT64_MIN / a)) {
                return -1;
            }
        } else {
            if ((b > 0 && a < INT64_MIN / b) || (b < 0 && a < INT64_MAX / b)) {
                return -1;
            }
        }
    }

    *out = a * b;
    return 0;
}

static int mihft_vec_push(mihft_pos_vec *vec, const mihft_pos_rec *rec)
{
    mihft_pos_rec *nv;
    size_t nc;

    if (vec->n == vec->cap) {
        nc = vec->cap == 0 ? 16u : vec->cap * 2u;
        if (nc < vec->cap) {
            return -1;
        }
        nv = (mihft_pos_rec *)realloc(vec->v, nc * sizeof(*vec->v));
        if (nv == NULL) {
            return -1;
        }
        vec->v = nv;
        vec->cap = nc;
    }

    vec->v[vec->n++] = *rec;
    return 0;
}

static mihft_pos_rec *mihft_find_pos(mihft_pos_vec *vec, const char *instr_code)
{
    size_t i;

    for (i = 0; i < vec->n; ++i) {
        if (strcmp(vec->v[i].instr_code, instr_code) == 0) {
            return &vec->v[i];
        }
    }

    return NULL;
}

static int mihft_read_positions(mihft_pos_vec *vec)
{
    FILE *fp = fopen(MIHFT_POS_IN, "r");
    char line[MIHFT_LINE_MAX];
    mihft_pos_rec rec;
    unsigned long row = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCPOSF入力を開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        ++row;
        mihft_chomp(line);
        if (row == 1 && strncmp(line, "CIF-NO,", 7) == 0) {
            continue;
        }
        if (line[0] == '\0') {
            continue;
        }
        if (mihft_parse_pos_line(line, &rec) != 0 || mihft_vec_push(vec, &rec) != 0) {
            fprintf(stderr, "SCPOSF形式異常 行=%lu\n", row);
            fclose(fp);
            return -1;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCPOSF読込異常\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int mihft_write_positions(const mihft_pos_vec *vec)
{
    FILE *fp = fopen(MIHFT_POS_OUT, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "SCPOSF出力を開けません\n");
        return -1;
    }

    if (fprintf(fp, "CIF-NO,INSTR-CODE,NET-QTY,AVG-AMT,RLZD-AMT\n") < 0) {
        fclose(fp);
        return -1;
    }

    for (i = 0; i < vec->n; ++i) {
        if (fprintf(fp, "%s,%s,%lld,%lld,%lld\n",
                    vec->v[i].cif_no,
                    vec->v[i].instr_code,
                    (long long)vec->v[i].net_qty,
                    (long long)vec->v[i].avg_amt,
                    (long long)vec->v[i].rlzd_amt) < 0) {
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        return -1;
    }

    return 0;
}

static void mihft_session_date(const char *exec_ts, char sess_dt[MIHFT_DATE_MAX])
{
    size_t i;
    size_t j = 0;

    for (i = 0; exec_ts[i] != '\0' && j < 8; ++i) {
        if (isdigit((unsigned char)exec_ts[i])) {
            sess_dt[j++] = exec_ts[i];
        }
    }

    while (j < 8) {
        sess_dt[j++] = '0';
    }
    sess_dt[j] = '\0';
}

static int mihft_append_pnl(FILE *fp, const mihft_pos_rec *pos, int64_t inc_rlzd, const char *exec_ts)
{
    char sess_dt[MIHFT_DATE_MAX];

    mihft_session_date(exec_ts, sess_dt);

    if (fprintf(fp, "%s,%s,%s,%lld,0,0,%s\n",
                pos->cif_no,
                pos->instr_code,
                sess_dt,
                (long long)inc_rlzd,
                exec_ts) < 0) {
        return -1;
    }

    return 0;
}

/*
 * 当ホット計算は反対売買の実現損益 (RLZD-AMT) の増分だけを担う。平均取得単価
 * (AVG-AMT) は mihft_pos 本体が確定した単価基準を所与とし、ここでは再算定しない
 * (SCPOSF の AVG-AMT をそのまま単価として用い、建玉ゼロ時のみ明示的にゼロへ戻す)。
 * 実現損益 = (約定単価 − 取得単価) × クローズ数量 を符号整合のうえ積み上げる。
 */
static int mihft_apply_exec(mihft_pos_rec *pos, const mihft_exec_rec *exe, int64_t *inc_rlzd)
{
    int64_t abs_net;
    int64_t close_qty;
    int64_t avg_unit;
    int64_t fill_unit;
    int64_t unit_diff;
    int64_t signed_diff;
    int64_t calc_rlzd;
    int64_t next_rlzd;
    int64_t signed_fill_qty;
    int64_t next_net;

    *inc_rlzd = 0;

    if (exe->fill_amt > MIHFT_MAX_NOTIONAL) {
        return MIHFT_DEC_REJECT_NOTIONAL;
    }

    if (pos->net_qty == 0 ||
        (pos->net_qty > 0 && exe->side_kbn == 'B') ||
        (pos->net_qty < 0 && exe->side_kbn == 'S')) {
        close_qty = 0;
    } else {
        abs_net = pos->net_qty < 0 ? -pos->net_qty : pos->net_qty;
        close_qty = exe->fill_qty < abs_net ? exe->fill_qty : abs_net;
    }

    if (close_qty > 0) {
        /* AVG-AMT は本体確定の取得単価を所与とする (ここでは導出しない)。 */
        avg_unit = pos->avg_amt;
        fill_unit = exe->fill_amt / exe->fill_qty;

        if (mihft_sub_i64(fill_unit, avg_unit, &unit_diff) != 0) {
            return -1;
        }
        signed_diff = pos->net_qty > 0 ? unit_diff : -unit_diff;

        if (mihft_mul_i64(signed_diff, close_qty, &calc_rlzd) != 0 ||
            mihft_add_i64(pos->rlzd_amt, calc_rlzd, &next_rlzd) != 0) {
            return -1;
        }

        pos->rlzd_amt = next_rlzd;
        *inc_rlzd = calc_rlzd;
    }

    signed_fill_qty = exe->side_kbn == 'B' ? exe->fill_qty : -exe->fill_qty;
    if (mihft_add_i64(pos->net_qty, signed_fill_qty, &next_net) != 0) {
        return -1;
    }

    /* AVG-AMT は本体に従い保持する。建玉がゼロになったときだけゼロへ戻す。 */
    if (next_net == 0) {
        pos->avg_amt = 0;
    }

    pos->net_qty = next_net;
    return MIHFT_DEC_ACCEPT;
}

int main(void)
{
    mihft_pos_vec positions;
    FILE *exec_fp = NULL;
    FILE *pnl_fp = NULL;
    char line[MIHFT_LINE_MAX];
    mihft_exec_rec exe;
    mihft_pos_rec *pos;
    unsigned long row = 0;
    int rc = MIHFT_DEC_ACCEPT;
    int decision;
    int64_t inc_rlzd;

    positions.v = NULL;
    positions.n = 0;
    positions.cap = 0;

    if (mihft_read_positions(&positions) != 0) {
        free(positions.v);
        return 20;
    }

    exec_fp = fopen(MIHFT_EXEC_IN, "r");
    if (exec_fp == NULL) {
        fprintf(stderr, "SCEXEC入力を開けません\n");
        free(positions.v);
        return 21;
    }

    pnl_fp = fopen(MIHFT_PNL_OUT, "w");
    if (pnl_fp == NULL) {
        fprintf(stderr, "SCPNLF出力を開けません\n");
        fclose(exec_fp);
        free(positions.v);
        return 22;
    }

    if (fprintf(pnl_fp, "CIF-NO,INSTR-CODE,SESS-DT,RLZD-AMT,UNRLZD-AMT,FEE-AMT,CALC-TS\n") < 0) {
        fprintf(stderr, "SCPNLF見出し出力異常\n");
        fclose(pnl_fp);
        fclose(exec_fp);
        free(positions.v);
        return 23;
    }

    while (fgets(line, sizeof(line), exec_fp) != NULL) {
        ++row;
        mihft_chomp(line);
        if (row == 1 && strncmp(line, "EXEC-ID,", 8) == 0) {
            continue;
        }
        if (line[0] == '\0') {
            continue;
        }

        if (mihft_parse_exec_line(line, &exe) != 0) {
            fprintf(stderr, "SCEXEC形式異常 行=%lu\n", row);
            rc = 24;
            break;
        }

        pos = mihft_find_pos(&positions, exe.instr_code);
        if (pos == NULL) {
            fprintf(stderr, "SCPOSF建玉未検出 行=%lu\n", row);
            rc = MIHFT_DEC_REJECT_MARGIN;
            continue;
        }

        decision = mihft_apply_exec(pos, &exe, &inc_rlzd);
        if (decision < 0) {
            fprintf(stderr, "損益計算桁あふれ 行=%lu\n", row);
            rc = 25;
            break;
        }
        if (decision != MIHFT_DEC_ACCEPT) {
            rc = decision;
            continue;
        }

        if (inc_rlzd != 0 && mihft_append_pnl(pnl_fp, pos, inc_rlzd, exe.exec_ts) != 0) {
            fprintf(stderr, "SCPNLF明細出力異常 行=%lu\n", row);
            rc = 26;
            break;
        }
    }

    if (ferror(exec_fp)) {
        fprintf(stderr, "SCEXEC読込異常\n");
        rc = 27;
    }

    if (fclose(pnl_fp) != 0 && rc == MIHFT_DEC_ACCEPT) {
        fprintf(stderr, "SCPNLF終了処理異常\n");
        rc = 28;
    }
    if (fclose(exec_fp) != 0 && rc == MIHFT_DEC_ACCEPT) {
        fprintf(stderr, "SCEXEC終了処理異常\n");
        rc = 29;
    }

    if (rc == MIHFT_DEC_ACCEPT && mihft_write_positions(&positions) != 0) {
        fprintf(stderr, "SCPOSF出力異常\n");
        rc = 30;
    }

    free(positions.v);
    return rc;
}
