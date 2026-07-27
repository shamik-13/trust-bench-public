/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20191022  藤田 和也 (E-271)   初版作成、約定差分によるポジション更新を実装
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_EXEC_FILE "SCEXEC.csv"
#define MIHFT_POS_FILE  "SCPOSF.csv"
#define MIHFT_TMP_FILE  "SCPOSF.tmp"
#define MIHFT_MAX_LINE  512
#define MIHFT_MAX_POS   20000
#define MIHFT_CIF_LEN   32
#define MIHFT_CODE_LEN  32
#define MIHFT_ID_LEN    64

typedef struct {
    char exec_id[MIHFT_ID_LEN];
    char order_id[MIHFT_ID_LEN];
    char instr_code[MIHFT_CODE_LEN];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[MIHFT_ID_LEN];
} exec_rec_t;

typedef struct {
    char cif_no[MIHFT_CIF_LEN];
    char instr_code[MIHFT_CODE_LEN];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
} pos_rec_t;

static void trim_eol(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n;

    if (dstsz == 0) {
        return -1;
    }

    n = strlen(src);
    if (n >= dstsz) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *endp;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || *endp != '\0') {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int split_csv(char *line, char **cols, size_t need)
{
    size_t i = 0;
    char *p = line;

    while (i < need) {
        cols[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    return i == need && strchr(cols[need - 1], ',') == NULL ? 0 : -1;
}

static int parse_exec(char *line, exec_rec_t *rec)
{
    char *cols[7];

    trim_eol(line);
    if (split_csv(line, cols, 7) != 0) {
        return -1;
    }

    if (copy_field(rec->exec_id, sizeof(rec->exec_id), cols[0]) != 0 ||
        copy_field(rec->order_id, sizeof(rec->order_id), cols[1]) != 0 ||
        copy_field(rec->instr_code, sizeof(rec->instr_code), cols[2]) != 0 ||
        copy_field(rec->exec_ts, sizeof(rec->exec_ts), cols[6]) != 0) {
        return -1;
    }

    if ((cols[3][0] != 'B' && cols[3][0] != 'S') || cols[3][1] != '\0') {
        return -1;
    }
    rec->side_kbn = cols[3][0];

    if (parse_i64(cols[4], &rec->fill_qty) != 0 ||
        parse_i64(cols[5], &rec->fill_amt) != 0) {
        return -1;
    }

    if (rec->fill_qty <= 0 || rec->fill_amt < 0) {
        return -1;
    }

    return 0;
}

static int parse_pos(char *line, pos_rec_t *rec)
{
    char *cols[5];

    trim_eol(line);
    if (split_csv(line, cols, 5) != 0) {
        return -1;
    }

    if (copy_field(rec->cif_no, sizeof(rec->cif_no), cols[0]) != 0 ||
        copy_field(rec->instr_code, sizeof(rec->instr_code), cols[1]) != 0 ||
        parse_i64(cols[2], &rec->net_qty) != 0 ||
        parse_i64(cols[3], &rec->avg_amt) != 0 ||
        parse_i64(cols[4], &rec->rlzd_amt) != 0) {
        return -1;
    }

    return 0;
}

static int load_positions(pos_rec_t *pos, size_t *count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];

    fp = fopen(MIHFT_POS_FILE, "r");
    if (fp == NULL) {
        perror("ポジションファイル読込失敗");
        return -1;
    }

    *count = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        if (*count >= MIHFT_MAX_POS) {
            fputs("ポジション件数上限超過\n", stderr);
            fclose(fp);
            return -1;
        }
        if (parse_pos(line, &pos[*count]) != 0) {
            fputs("ポジションレコード形式不正\n", stderr);
            fclose(fp);
            return -1;
        }
        ++*count;
    }

    if (ferror(fp)) {
        perror("ポジションファイル読込失敗");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int save_positions(const pos_rec_t *pos, size_t count)
{
    FILE *fp;
    size_t i;

    fp = fopen(MIHFT_TMP_FILE, "w");
    if (fp == NULL) {
        perror("ポジション一時ファイル作成失敗");
        return -1;
    }

    for (i = 0; i < count; ++i) {
        if (fprintf(fp, "%s,%s,%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
                    pos[i].cif_no,
                    pos[i].instr_code,
                    pos[i].net_qty,
                    pos[i].avg_amt,
                    pos[i].rlzd_amt) < 0) {
            perror("ポジション一時ファイル書込失敗");
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        perror("ポジション一時ファイル確定失敗");
        return -1;
    }

    if (rename(MIHFT_TMP_FILE, MIHFT_POS_FILE) != 0) {
        perror("ポジションファイル置換失敗");
        return -1;
    }

    return 0;
}

static int derive_cif_no(const char *order_id, char *cif_no, size_t cifsz)
{
    const char *sep;
    size_t n;

    sep = strchr(order_id, '-');
    n = sep == NULL ? strlen(order_id) : (size_t)(sep - order_id);

    if (n == 0 || n >= cifsz) {
        return -1;
    }

    memcpy(cif_no, order_id, n);
    cif_no[n] = '\0';
    return 0;
}

static pos_rec_t *find_or_add_pos(pos_rec_t *pos, size_t *count,
                                  const char *cif_no, const char *instr_code)
{
    size_t i;

    for (i = 0; i < *count; ++i) {
        if (strcmp(pos[i].cif_no, cif_no) == 0 &&
            strcmp(pos[i].instr_code, instr_code) == 0) {
            return &pos[i];
        }
    }

    if (*count >= MIHFT_MAX_POS) {
        return NULL;
    }

    memset(&pos[*count], 0, sizeof(pos[*count]));
    if (copy_field(pos[*count].cif_no, sizeof(pos[*count].cif_no), cif_no) != 0 ||
        copy_field(pos[*count].instr_code, sizeof(pos[*count].instr_code), instr_code) != 0) {
        return NULL;
    }

    return &pos[(*count)++];
}

static int checked_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a != 0 && b != 0) {
        if ((a > 0 && b > 0 && a > INT64_MAX / b) ||
            (a > 0 && b < 0 && b < INT64_MIN / a) ||
            (a < 0 && b > 0 && a < INT64_MIN / b) ||
            (a < 0 && b < 0 && a < INT64_MAX / b)) {
            return -1;
        }
    }

    *out = a * b;
    return 0;
}

static int checked_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) ||
        (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }

    *out = a + b;
    return 0;
}

static int abs_i64(int64_t v, int64_t *out)
{
    if (v == INT64_MIN) {
        return -1;
    }

    *out = v < 0 ? -v : v;
    return 0;
}

static int apply_exec(pos_rec_t *pos, const exec_rec_t *exec)
{
    int64_t signed_qty;
    int64_t old_qty;
    int64_t new_qty;
    int64_t old_abs;
    int64_t new_abs;
    int64_t close_qty;
    int64_t open_qty;
    int64_t fill_px;
    int64_t old_basis;
    int64_t fill_basis;
    int64_t new_basis;
    int64_t pnl;
    int64_t persist_notional;

    if (exec->fill_qty > 0 && exec->fill_amt > MIHFT_MAX_NOTIONAL) {
        return 8;
    }

    fill_px = exec->fill_amt / exec->fill_qty;
    signed_qty = exec->side_kbn == 'B' ? exec->fill_qty : -exec->fill_qty;
    old_qty = pos->net_qty;

    if (checked_add_i64(old_qty, signed_qty, &new_qty) != 0) {
        return 8;
    }

    if (abs_i64(old_qty, &old_abs) != 0 || abs_i64(new_qty, &new_abs) != 0) {
        return 8;
    }

    close_qty = 0;
    open_qty = exec->fill_qty;
    if ((old_qty > 0 && signed_qty < 0) || (old_qty < 0 && signed_qty > 0)) {
        close_qty = old_abs < exec->fill_qty ? old_abs : exec->fill_qty;
        open_qty = exec->fill_qty - close_qty;
    }

    if (close_qty > 0) {
        if (old_qty > 0) {
            if (checked_mul_i64(close_qty, fill_px - pos->avg_amt, &pnl) != 0) {
                return 8;
            }
        } else {
            if (checked_mul_i64(close_qty, pos->avg_amt - fill_px, &pnl) != 0) {
                return 8;
            }
        }
        if (checked_add_i64(pos->rlzd_amt, pnl, &pos->rlzd_amt) != 0) {
            return 8;
        }
    }

    if (new_qty == 0) {
        pos->net_qty = 0;
        pos->avg_amt = 0;
        return 0;
    }

    if (open_qty == 0) {
        pos->net_qty = new_qty;
        return 0;
    }

    if ((old_qty > 0 && signed_qty > 0) || (old_qty < 0 && signed_qty < 0)) {
        if (checked_mul_i64(old_abs, pos->avg_amt, &old_basis) != 0 ||
            checked_mul_i64(open_qty, fill_px, &fill_basis) != 0 ||
            checked_add_i64(old_basis, fill_basis, &new_basis) != 0) {
            return 8;
        }
        pos->avg_amt = new_basis / new_abs;
    } else {
        pos->avg_amt = fill_px;
    }

    if (checked_mul_i64(new_abs, pos->avg_amt, &persist_notional) != 0 ||
        persist_notional > MIHFT_MAX_NOTIONAL) {
        return 8;
    }

    pos->net_qty = new_qty;
    return 0;
}

int main(void)
{
    FILE *fp;
    pos_rec_t positions[MIHFT_MAX_POS];
    size_t pos_count;
    char line[MIHFT_MAX_LINE];
    int decision = 0;

    if (load_positions(positions, &pos_count) != 0) {
        return 2;
    }

    fp = fopen(MIHFT_EXEC_FILE, "r");
    if (fp == NULL) {
        perror("約定ファイル読込失敗");
        return 2;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        exec_rec_t exec;
        pos_rec_t *pos;
        char cif_no[MIHFT_CIF_LEN];

        if (parse_exec(line, &exec) != 0) {
            fputs("約定レコード形式不正\n", stderr);
            fclose(fp);
            return 2;
        }

        if (derive_cif_no(exec.order_id, cif_no, sizeof(cif_no)) != 0) {
            fputs("顧客番号導出失敗\n", stderr);
            fclose(fp);
            return 2;
        }

        pos = find_or_add_pos(positions, &pos_count, cif_no, exec.instr_code);
        if (pos == NULL) {
            fputs("ポジション領域不足\n", stderr);
            fclose(fp);
            return 2;
        }

        decision = apply_exec(pos, &exec);
        if (decision != 0) {
            fclose(fp);
            return decision;
        }
    }

    if (ferror(fp)) {
        perror("約定ファイル読込失敗");
        fclose(fp);
        return 2;
    }

    fclose(fp);

    if (save_positions(positions, pos_count) != 0) {
        return 2;
    }

    return decision;
}
