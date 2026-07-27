/* ================================================================
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.0   20220222  今井 彩 (E-230)  日中時価評価バッチ初版
 * ================================================================ */
#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_MAX_POS_ROWS 4096
#define MIHFT_MAX_MKT_ROWS 4096
#define MIHFT_LINE_MAX 512
#define MIHFT_CODE_MAX 32
#define MIHFT_CIF_MAX 32
#define MIHFT_SESS_DT_MAX 9
#define MIHFT_STALE_NS 5000000000LL
#define MIHFT_IO_ERROR 16
#define MIHFT_PARSE_ERROR 20

struct pos_row {
    char cif_no[MIHFT_CIF_MAX];
    char instr_code[MIHFT_CODE_MAX];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
};

struct mkt_row {
    char instr_code[MIHFT_CODE_MAX];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t last_amt;
    int64_t vol_qty;
    int64_t tick_ts;
};

struct mtm_row {
    char cif_no[MIHFT_CIF_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char sess_dt[MIHFT_SESS_DT_MAX];
    int64_t net_qty;
    int64_t mark_amt;
    int64_t mark_notional_amt;
    int64_t unrlzd_amt;
};

static void trim_field(char *s)
{
    char *p = s;
    char *e;

    while (*p != '\0' && isspace((unsigned char)*p)) {
        p++;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1U);
    }

    e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) {
        *--e = '\0';
    }
}

static int copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t len = strlen(src);

    if (dst_sz == 0U || len >= dst_sz) {
        return -1;
    }
    memcpy(dst, src, len + 1U);
    return 0;
}

static int next_field(char **cursor, char *dst, size_t dst_sz)
{
    char *start;
    char *comma;
    size_t len;

    if (*cursor == NULL) {
        return -1;
    }

    start = *cursor;
    comma = strchr(start, ',');
    if (comma != NULL) {
        *comma = '\0';
        *cursor = comma + 1;
    } else {
        *cursor = NULL;
    }

    trim_field(start);
    len = strlen(start);
    if (len == 0U || len >= dst_sz) {
        return -1;
    }

    memcpy(dst, start, len + 1U);
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s) {
        return -1;
    }
    while (*end != '\0') {
        if (!isspace((unsigned char)*end)) {
            return -1;
        }
        end++;
    }

    *out = (int64_t)v;
    return 0;
}

static int next_i64(char **cursor, int64_t *out)
{
    char buf[64];

    if (next_field(cursor, buf, sizeof buf) != 0) {
        return -1;
    }
    return parse_i64(buf, out);
}

static int is_header_line(const char *line, const char *first_name)
{
    char buf[MIHFT_LINE_MAX];
    char *cursor = buf;
    char first[64];

    if (copy_field(buf, sizeof buf, line) != 0) {
        return 0;
    }
    if (next_field(&cursor, first, sizeof first) != 0) {
        return 0;
    }
    return strcmp(first, first_name) == 0;
}

static int load_positions(struct pos_row *rows, size_t max_rows, size_t *count)
{
    FILE *fp = fopen("SCPOSF.csv", "r");
    char line[MIHFT_LINE_MAX];
    size_t n = 0U;

    if (fp == NULL) {
        fprintf(stderr, "SCPOSF.csv オープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *cursor = line;

        trim_field(line);
        if (line[0] == '\0' || line[0] == '#') {
            continue;
        }
        if (is_header_line(line, "CIF-NO")) {
            continue;
        }
        if (n >= max_rows) {
            fprintf(stderr, "SCPOSF.csv 件数超過\n");
            fclose(fp);
            return -1;
        }

        memset(&rows[n], 0, sizeof rows[n]);
        if (next_field(&cursor, rows[n].cif_no, sizeof rows[n].cif_no) != 0 ||
            next_field(&cursor, rows[n].instr_code, sizeof rows[n].instr_code) != 0 ||
            next_i64(&cursor, &rows[n].net_qty) != 0 ||
            next_i64(&cursor, &rows[n].avg_amt) != 0 ||
            next_i64(&cursor, &rows[n].rlzd_amt) != 0) {
            fprintf(stderr, "SCPOSF.csv 解析失敗\n");
            fclose(fp);
            return -1;
        }
        n++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCPOSF.csv 読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int load_markets(struct mkt_row *rows, size_t max_rows, size_t *count)
{
    FILE *fp = fopen("SCMKTD.csv", "r");
    char line[MIHFT_LINE_MAX];
    size_t n = 0U;

    if (fp == NULL) {
        fprintf(stderr, "SCMKTD.csv オープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *cursor = line;

        trim_field(line);
        if (line[0] == '\0' || line[0] == '#') {
            continue;
        }
        if (is_header_line(line, "INSTR-CODE")) {
            continue;
        }
        if (n >= max_rows) {
            fprintf(stderr, "SCMKTD.csv 件数超過\n");
            fclose(fp);
            return -1;
        }

        memset(&rows[n], 0, sizeof rows[n]);
        if (next_field(&cursor, rows[n].instr_code, sizeof rows[n].instr_code) != 0 ||
            next_i64(&cursor, &rows[n].bid_amt) != 0 ||
            next_i64(&cursor, &rows[n].ask_amt) != 0 ||
            next_i64(&cursor, &rows[n].last_amt) != 0 ||
            next_i64(&cursor, &rows[n].vol_qty) != 0 ||
            next_i64(&cursor, &rows[n].tick_ts) != 0) {
            fprintf(stderr, "SCMKTD.csv 解析失敗\n");
            fclose(fp);
            return -1;
        }
        n++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCMKTD.csv 読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static const struct mkt_row *find_market(const struct mkt_row *rows, size_t count, const char *instr_code)
{
    const struct mkt_row *best = NULL;
    size_t i;

    for (i = 0U; i < count; i++) {
        if (strcmp(rows[i].instr_code, instr_code) == 0) {
            if (best == NULL || rows[i].tick_ts > best->tick_ts) {
                best = &rows[i];
            }
        }
    }
    return best;
}

static int load_previous_mark(const char *cif_no, const char *instr_code, struct mtm_row *out)
{
    FILE *fp = fopen("SCM2MF.csv", "r");
    char line[MIHFT_LINE_MAX];
    int found = 0;

    if (fp == NULL) {
        return 0;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        struct mtm_row row;
        char *cursor = line;

        trim_field(line);
        if (line[0] == '\0' || line[0] == '#') {
            continue;
        }
        if (is_header_line(line, "CIF-NO")) {
            continue;
        }

        memset(&row, 0, sizeof row);
        if (next_field(&cursor, row.cif_no, sizeof row.cif_no) != 0 ||
            next_field(&cursor, row.instr_code, sizeof row.instr_code) != 0 ||
            next_field(&cursor, row.sess_dt, sizeof row.sess_dt) != 0 ||
            next_i64(&cursor, &row.net_qty) != 0 ||
            next_i64(&cursor, &row.mark_amt) != 0 ||
            next_i64(&cursor, &row.mark_notional_amt) != 0 ||
            next_i64(&cursor, &row.unrlzd_amt) != 0) {
            fprintf(stderr, "SCM2MF.csv 解析失敗\n");
            fclose(fp);
            return -1;
        }

        if (strcmp(row.cif_no, cif_no) == 0 && strcmp(row.instr_code, instr_code) == 0) {
            *out = row;
            found = 1;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCM2MF.csv 読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return found;
}

static int abs_i64_checked(int64_t v, int64_t *out)
{
    if (v == INT64_MIN) {
        return -1;
    }
    *out = v < 0 ? -v : v;
    return 0;
}

static int mul_i64_checked(int64_t a, int64_t b, int64_t *out)
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

static int sub_i64_checked(int64_t a, int64_t b, int64_t *out)
{
    if ((b < 0 && a > INT64_MAX + b) || (b > 0 && a < INT64_MIN + b)) {
        return -1;
    }
    *out = a - b;
    return 0;
}

static int select_mark_amt(const struct pos_row *pos, const struct mkt_row *mkt, int64_t *mark_amt)
{
    if (pos->net_qty > 0) {
        if (mkt->bid_amt > 0) {
            *mark_amt = mkt->bid_amt;
            return 0;
        }
        if (mkt->last_amt > 0) {
            *mark_amt = mkt->last_amt;
            return 0;
        }
        if (mkt->ask_amt > 0) {
            *mark_amt = mkt->ask_amt;
            return 0;
        }
    } else if (pos->net_qty < 0) {
        if (mkt->ask_amt > 0) {
            *mark_amt = mkt->ask_amt;
            return 0;
        }
        if (mkt->last_amt > 0) {
            *mark_amt = mkt->last_amt;
            return 0;
        }
        if (mkt->bid_amt > 0) {
            *mark_amt = mkt->bid_amt;
            return 0;
        }
    } else {
        if (mkt->last_amt > 0) {
            *mark_amt = mkt->last_amt;
            return 0;
        }
        if (mkt->bid_amt > 0) {
            *mark_amt = mkt->bid_amt;
            return 0;
        }
        if (mkt->ask_amt > 0) {
            *mark_amt = mkt->ask_amt;
            return 0;
        }
    }

    return -1;
}

static int build_sess_dt(char *dst, size_t dst_sz)
{
    time_t now = time(NULL);
    struct tm *lt = localtime(&now);

    if (lt == NULL || dst_sz < MIHFT_SESS_DT_MAX) {
        return -1;
    }
    if (strftime(dst, dst_sz, "%Y%m%d", lt) == 0U) {
        return -1;
    }
    return 0;
}

static int resolve_now_ts(const struct mkt_row *rows, size_t count, int64_t *now_ts)
{
    const char *env_now = getenv("MIHFT_NOW_TS");
    size_t i;

    if (env_now != NULL && env_now[0] != '\0') {
        return parse_i64(env_now, now_ts);
    }

    if (count == 0U) {
        return -1;
    }

    *now_ts = rows[0].tick_ts;
    for (i = 1U; i < count; i++) {
        if (rows[i].tick_ts > *now_ts) {
            *now_ts = rows[i].tick_ts;
        }
    }
    return 0;
}

static int write_mtm_row(FILE *fp, const struct mtm_row *row)
{
    if (fprintf(fp, "%s,%s,%s,%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
                row->cif_no,
                row->instr_code,
                row->sess_dt,
                row->net_qty,
                row->mark_amt,
                row->mark_notional_amt,
                row->unrlzd_amt) < 0) {
        return -1;
    }
    return 0;
}

static int make_mtm_row(const struct pos_row *pos,
                        const struct mkt_row *mkt,
                        const char *sess_dt,
                        struct mtm_row *out)
{
    int64_t mark_amt;
    int64_t abs_qty;
    int64_t diff_amt;

    if (select_mark_amt(pos, mkt, &mark_amt) != 0 ||
        abs_i64_checked(pos->net_qty, &abs_qty) != 0 ||
        mul_i64_checked(abs_qty, mark_amt, &out->mark_notional_amt) != 0 ||
        sub_i64_checked(mark_amt, pos->avg_amt, &diff_amt) != 0 ||
        mul_i64_checked(pos->net_qty, diff_amt, &out->unrlzd_amt) != 0) {
        return -1;
    }

    memset(out, 0, sizeof *out);
    if (copy_field(out->cif_no, sizeof out->cif_no, pos->cif_no) != 0 ||
        copy_field(out->instr_code, sizeof out->instr_code, pos->instr_code) != 0 ||
        copy_field(out->sess_dt, sizeof out->sess_dt, sess_dt) != 0) {
        return -1;
    }

    out->net_qty = pos->net_qty;
    out->mark_amt = mark_amt;
    if (mul_i64_checked(abs_qty, mark_amt, &out->mark_notional_amt) != 0 ||
        mul_i64_checked(pos->net_qty, diff_amt, &out->unrlzd_amt) != 0) {
        return -1;
    }

    return 0;
}

int main(void)
{
    struct pos_row positions[MIHFT_MAX_POS_ROWS];
    struct mkt_row markets[MIHFT_MAX_MKT_ROWS];
    char sess_dt[MIHFT_SESS_DT_MAX];
    int64_t now_ts;
    size_t pos_count = 0U;
    size_t mkt_count = 0U;
    size_t i;
    int rc = MIHFT_ACCEPT;
    FILE *out_fp;

    if (load_positions(positions, MIHFT_MAX_POS_ROWS, &pos_count) != 0) {
        return MIHFT_PARSE_ERROR;
    }
    if (load_markets(markets, MIHFT_MAX_MKT_ROWS, &mkt_count) != 0) {
        return MIHFT_PARSE_ERROR;
    }
    if (resolve_now_ts(markets, mkt_count, &now_ts) != 0 || build_sess_dt(sess_dt, sizeof sess_dt) != 0) {
        fprintf(stderr, "評価基準時刻取得失敗\n");
        return MIHFT_IO_ERROR;
    }

    out_fp = fopen("SCM2MF.out", "w");
    if (out_fp == NULL) {
        fprintf(stderr, "SCM2MF.out オープン失敗\n");
        return MIHFT_IO_ERROR;
    }

    if (fprintf(out_fp, "CIF-NO,INSTR-CODE,SESS-DT,NET-QTY,MARK-AMT,MARK-NOTIONAL-AMT,UNRLZD-AMT\n") < 0) {
        fprintf(stderr, "SCM2MF.out 書込失敗\n");
        fclose(out_fp);
        return MIHFT_IO_ERROR;
    }

    for (i = 0U; i < pos_count; i++) {
        const struct mkt_row *mkt = find_market(markets, mkt_count, positions[i].instr_code);
        struct mtm_row row;
        int previous;

        if (mkt == NULL) {
            fprintf(stderr, "銘柄別気配なし:%s\n", positions[i].instr_code);
            rc = MIHFT_REJ_NOTIONAL;
            continue;
        }

        previous = load_previous_mark(positions[i].cif_no, positions[i].instr_code, &row);
        if (previous < 0) {
            fclose(out_fp);
            return MIHFT_PARSE_ERROR;
        }

        if (now_ts - mkt->tick_ts > MIHFT_STALE_NS) {
            if (previous > 0) {
                if (write_mtm_row(out_fp, &row) != 0) {
                    fprintf(stderr, "SCM2MF.out 書込失敗\n");
                    fclose(out_fp);
                    return MIHFT_IO_ERROR;
                }
            }
            rc = MIHFT_REJ_NOTIONAL;
            continue;
        }

        if (make_mtm_row(&positions[i], mkt, sess_dt, &row) != 0) {
            fprintf(stderr, "時価評価計算失敗:%s\n", positions[i].instr_code);
            fclose(out_fp);
            return MIHFT_PARSE_ERROR;
        }

        if (write_mtm_row(out_fp, &row) != 0) {
            fprintf(stderr, "SCM2MF.out 書込失敗\n");
            fclose(out_fp);
            return MIHFT_IO_ERROR;
        }
    }

    if (fclose(out_fp) != 0) {
        fprintf(stderr, "SCM2MF.out クローズ失敗\n");
        return MIHFT_IO_ERROR;
    }

    return rc;
}
