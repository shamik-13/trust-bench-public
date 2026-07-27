/* 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220906  渡辺 隆 (E-260)  初版作成、約定差分による建玉更新を実装
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_OK 0
#define MIHFT_ERR_IO 2
#define MIHFT_ERR_PARSE 3
#define MIHFT_ERR_RANGE 5

#define MIHFT_EXEC_PATH "SCEXEC.csv"
#define MIHFT_POS_IN_PATH "SCPOSF.csv"
#define MIHFT_POS_OUT_PATH "SCPOSF.out.csv"

#define MIHFT_LINE_MAX 512
#define MIHFT_ID_MAX 64
#define MIHFT_CODE_MAX 32
#define MIHFT_TS_MAX 40
#define MIHFT_POS_MAX 4096

struct exec_rec {
    char exec_id[MIHFT_ID_MAX];
    char order_id[MIHFT_ID_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[MIHFT_TS_MAX];
};

struct pos_rec {
    char cif_no[MIHFT_ID_MAX];
    char instr_code[MIHFT_CODE_MAX];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
};

static void chomp(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dst_sz) {
        return MIHFT_ERR_PARSE;
    }
    memcpy(dst, src, n + 1);
    return MIHFT_OK;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *endp;
    long long v;

    if (*s == '\0') {
        return MIHFT_ERR_PARSE;
    }

    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || *endp != '\0') {
        return MIHFT_ERR_PARSE;
    }

    *out = (int64_t)v;
    return MIHFT_OK;
}

static int next_field(char **cur, char **field)
{
    char *p = *cur;
    char *comma;

    if (p == NULL) {
        return MIHFT_ERR_PARSE;
    }

    comma = strchr(p, ',');
    if (comma != NULL) {
        *comma = '\0';
        *cur = comma + 1;
    } else {
        *cur = NULL;
    }

    *field = p;
    return MIHFT_OK;
}

static int looks_like_header(const char *line)
{
    return strstr(line, "EXEC-ID") != NULL || strstr(line, "CIF-NO") != NULL;
}

static int parse_exec_line(char *line, struct exec_rec *rec)
{
    char *cur = line;
    char *f;
    int rc;

    chomp(line);

    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || copy_field(rec->exec_id, sizeof(rec->exec_id), f) != MIHFT_OK) return MIHFT_ERR_PARSE;
    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || copy_field(rec->order_id, sizeof(rec->order_id), f) != MIHFT_OK) return MIHFT_ERR_PARSE;
    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || copy_field(rec->instr_code, sizeof(rec->instr_code), f) != MIHFT_OK) return MIHFT_ERR_PARSE;
    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || (strcmp(f, "B") != 0 && strcmp(f, "S") != 0)) return MIHFT_ERR_PARSE;
    rec->side_kbn = f[0];
    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || parse_i64(f, &rec->fill_qty) != MIHFT_OK || rec->fill_qty <= 0) return MIHFT_ERR_PARSE;
    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || parse_i64(f, &rec->fill_amt) != MIHFT_OK || rec->fill_amt <= 0) return MIHFT_ERR_PARSE;
    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || copy_field(rec->exec_ts, sizeof(rec->exec_ts), f) != MIHFT_OK) return MIHFT_ERR_PARSE;

    if (cur != NULL || rec->fill_amt > MIHFT_MAX_NOTIONAL) {
        return MIHFT_ERR_RANGE;
    }

    return MIHFT_OK;
}

static int parse_pos_line(char *line, struct pos_rec *rec)
{
    char *cur = line;
    char *f;
    int rc;

    chomp(line);

    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || copy_field(rec->cif_no, sizeof(rec->cif_no), f) != MIHFT_OK) return MIHFT_ERR_PARSE;
    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || copy_field(rec->instr_code, sizeof(rec->instr_code), f) != MIHFT_OK) return MIHFT_ERR_PARSE;
    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || parse_i64(f, &rec->net_qty) != MIHFT_OK) return MIHFT_ERR_PARSE;
    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || parse_i64(f, &rec->avg_amt) != MIHFT_OK || rec->avg_amt < 0) return MIHFT_ERR_PARSE;
    rc = next_field(&cur, &f);
    if (rc != MIHFT_OK || parse_i64(f, &rec->rlzd_amt) != MIHFT_OK) return MIHFT_ERR_PARSE;

    if (cur != NULL) {
        return MIHFT_ERR_PARSE;
    }

    return MIHFT_OK;
}

static int abs_i64(int64_t v, int64_t *out)
{
    if (v == INT64_MIN) {
        return MIHFT_ERR_RANGE;
    }
    *out = v < 0 ? -v : v;
    return MIHFT_OK;
}

static int checked_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return MIHFT_ERR_RANGE;
    }
    *out = a + b;
    return MIHFT_OK;
}

static int checked_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a != 0 && b > INT64_MAX / a) {
        return MIHFT_ERR_RANGE;
    }
    *out = a * b;
    return MIHFT_OK;
}

static int find_pos(struct pos_rec *pos, size_t npos, const char *instr_code)
{
    size_t i;

    for (i = 0; i < npos; i++) {
        if (strcmp(pos[i].instr_code, instr_code) == 0) {
            return (int)i;
        }
    }
    return -1;
}

/*
 * 当処理は NET-QTY の差分更新と数量保存則の検査だけを担う。平均取得単価 (AVG-AMT)
 * の算定は mihft_pos 本体に従い、ここでは SCPOSF の AVG-AMT を所与として引き継ぐ。
 * 反対売買では本体が確定した AVG-AMT を単価基準として実現損益のみを増分し、建玉が
 * ゼロになった銘柄は AVG-AMT を明示的にゼロへ戻す。
 */
static int apply_exec(struct pos_rec *pos, const struct exec_rec *exec)
{
    int64_t old_net = pos->net_qty;
    int64_t old_abs;
    int64_t exec_signed;
    int64_t expected_net;
    int64_t new_abs;
    int64_t close_qty;
    int64_t close_unit;
    int64_t close_amt;
    int64_t basis_amt;
    int64_t delta_rlzd;

    exec_signed = exec->side_kbn == 'B' ? exec->fill_qty : -exec->fill_qty;
    if (checked_add_i64(old_net, exec_signed, &expected_net) != MIHFT_OK) {
        return MIHFT_ERR_RANGE;
    }

    if (abs_i64(old_net, &old_abs) != MIHFT_OK || abs_i64(expected_net, &new_abs) != MIHFT_OK) {
        return MIHFT_ERR_RANGE;
    }

    /* 追加建て: 数量のみ反映し、AVG-AMT は本体算定値をそのまま保持する。 */
    if (old_net == 0 || (old_net > 0 && exec_signed > 0) || (old_net < 0 && exec_signed < 0)) {
        pos->net_qty = expected_net;
        return pos->net_qty == old_net + exec_signed ? MIHFT_OK : MIHFT_ERR_RANGE;
    }

    /* 反対売買: 本体確定の単価基準 (AVG-AMT) を所与に実現損益のみ増分する。 */
    close_qty = exec->fill_qty < old_abs ? exec->fill_qty : old_abs;

    close_unit = exec->fill_amt / exec->fill_qty;
    if (checked_mul_i64(close_unit, close_qty, &close_amt) != MIHFT_OK ||
        checked_mul_i64(pos->avg_amt, close_qty, &basis_amt) != MIHFT_OK) {
        return MIHFT_ERR_RANGE;
    }

    delta_rlzd = old_net > 0 ? close_amt - basis_amt : basis_amt - close_amt;
    if (checked_add_i64(pos->rlzd_amt, delta_rlzd, &pos->rlzd_amt) != MIHFT_OK) {
        return MIHFT_ERR_RANGE;
    }

    pos->net_qty = expected_net;
    if (abs_i64(pos->net_qty, &new_abs) != MIHFT_OK) {
        return MIHFT_ERR_RANGE;
    }
    if (expected_net == 0) {
        pos->avg_amt = 0;
    }

    return pos->net_qty == old_net + exec_signed ? MIHFT_OK : MIHFT_ERR_RANGE;
}

static int load_positions(struct pos_rec *pos, size_t *npos)
{
    FILE *fp = fopen(MIHFT_POS_IN_PATH, "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "SCPOSF入力を開けません\n");
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        int rc;

        if (looks_like_header(line)) {
            continue;
        }
        if (*npos >= MIHFT_POS_MAX) {
            fclose(fp);
            fprintf(stderr, "SCPOSF件数が上限を超過しました\n");
            return MIHFT_ERR_RANGE;
        }

        rc = parse_pos_line(line, &pos[*npos]);
        if (rc != MIHFT_OK) {
            fclose(fp);
            fprintf(stderr, "SCPOSF形式不正\n");
            return rc;
        }
        (*npos)++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCPOSF読込失敗\n");
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    return MIHFT_OK;
}

static int process_execs(struct pos_rec *pos, size_t npos)
{
    FILE *fp = fopen(MIHFT_EXEC_PATH, "r");
    char line[MIHFT_LINE_MAX];

    if (fp == NULL) {
        fprintf(stderr, "SCEXEC入力を開けません\n");
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        struct exec_rec exec;
        int idx;
        int rc;

        if (looks_like_header(line)) {
            continue;
        }

        rc = parse_exec_line(line, &exec);
        if (rc != MIHFT_OK) {
            fclose(fp);
            fprintf(stderr, "SCEXEC形式不正\n");
            return rc;
        }

        idx = find_pos(pos, npos, exec.instr_code);
        if (idx < 0) {
            fclose(fp);
            fprintf(stderr, "対象建玉なし\n");
            return MIHFT_ERR_PARSE;
        }

        rc = apply_exec(&pos[idx], &exec);
        if (rc != MIHFT_OK) {
            fclose(fp);
            fprintf(stderr, "数量保存検査で異常を検出しました\n");
            return rc;
        }
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCEXEC読込失敗\n");
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    return MIHFT_OK;
}

static int write_positions(const struct pos_rec *pos, size_t npos)
{
    FILE *fp = fopen(MIHFT_POS_OUT_PATH, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "SCPOSF出力を開けません\n");
        return MIHFT_ERR_IO;
    }

    fprintf(fp, "CIF-NO,INSTR-CODE,NET-QTY,AVG-AMT,RLZD-AMT\n");
    for (i = 0; i < npos; i++) {
        if (fprintf(fp, "%s,%s,%lld,%lld,%lld\n",
                    pos[i].cif_no,
                    pos[i].instr_code,
                    (long long)pos[i].net_qty,
                    (long long)pos[i].avg_amt,
                    (long long)pos[i].rlzd_amt) < 0) {
            fclose(fp);
            fprintf(stderr, "SCPOSF書込失敗\n");
            return MIHFT_ERR_IO;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "SCPOSF終端書込失敗\n");
        return MIHFT_ERR_IO;
    }

    return MIHFT_OK;
}

int main(void)
{
    struct pos_rec positions[MIHFT_POS_MAX];
    size_t npos = 0;
    int rc;

    rc = load_positions(positions, &npos);
    if (rc != MIHFT_OK) {
        return rc;
    }

    rc = process_execs(positions, npos);
    if (rc != MIHFT_OK) {
        return rc;
    }

    rc = write_positions(positions, npos);
    if (rc != MIHFT_OK) {
        return rc;
    }

    return 0;
}
