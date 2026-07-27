/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20250121  西村 亮 (E-204)  初版作成。取引セッション切替の判定前状態を実装。
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

#ifndef MIHFT_DECISION_OK
#define MIHFT_DECISION_OK 0
#endif

#ifndef MIHFT_DECISION_DEFER
#define MIHFT_DECISION_DEFER 4
#endif

#ifndef MIHFT_DECISION_REJECT
#define MIHFT_DECISION_REJECT 8
#endif

#ifndef MIHFT_DECISION_IO_ERROR
#define MIHFT_DECISION_IO_ERROR 16
#endif

#define MAX_CAL 64u
#define MAX_POS 4096u
#define MAX_PNL 4096u
#define MAX_LINE 512u
#define KEY_LEN 96u
#define TS_LEN 32u

typedef struct {
    char sess_dt[16];
    char sess_kbn[8];
    char open_ts[TS_LEN];
    char close_ts[TS_LEN];
    int64_t open_epoch;
    int64_t close_epoch;
} CalRec;

typedef struct {
    char cif_no[32];
    char instr_code[32];
    char sess_dt[16];
    int64_t net_qty;
    int64_t mark_amt;
    int64_t mark_notional_amt;
    int64_t unrlzd_amt;
    unsigned done_bit;
} PosRec;

typedef struct {
    char cif_no[32];
    char instr_code[32];
    char sess_dt[16];
    int64_t rlzd_amt;
    int64_t unrlzd_amt;
    int64_t fee_amt;
    char calc_ts[TS_LEN];
} PnlRec;

typedef struct {
    CalRec cal[MAX_CAL];
    size_t cal_count;
    PosRec pos[MAX_POS];
    size_t pos_count;
    PnlRec pnl[MAX_PNL];
    size_t pnl_count;
} Store;

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t n;

    if (dst_len == 0u || src == NULL) {
        return -1;
    }
    n = strlen(src);
    if (n >= dst_len) {
        return -1;
    }
    memcpy(dst, src, n + 1u);
    return 0;
}

static char *trim(char *s)
{
    char *e;

    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n') {
        ++s;
    }
    e = s + strlen(s);
    while (e > s && (e[-1] == ' ' || e[-1] == '\t' || e[-1] == '\r' || e[-1] == '\n')) {
        --e;
    }
    *e = '\0';
    return s;
}

static int next_field(char **cur, char *out, size_t out_len)
{
    char *p;
    char *start;
    size_t n;

    if (cur == NULL || *cur == NULL || out == NULL || out_len == 0u) {
        return -1;
    }

    start = *cur;
    p = start;
    while (*p != '\0' && *p != ',') {
        ++p;
    }

    n = (size_t)(p - start);
    if (n >= out_len) {
        return -1;
    }
    memcpy(out, start, n);
    out[n] = '\0';
    (void)copy_field(out, out_len, trim(out));

    *cur = (*p == ',') ? p + 1 : p;
    return 0;
}

static int parse_i64(const char *s, int64_t *v)
{
    char *end;
    long long tmp;

    if (s == NULL || *s == '\0' || v == NULL) {
        return -1;
    }
    errno = 0;
    tmp = strtoll(s, &end, 10);
    if (errno != 0 || *trim(end) != '\0') {
        return -1;
    }
    *v = (int64_t)tmp;
    return 0;
}

static int parse_epoch(const char *dt, const char *ts, int64_t *epoch)
{
    struct tm tmv;
    char buf[32];

    if (dt == NULL || ts == NULL || epoch == NULL) {
        return -1;
    }
    if (strlen(dt) != 8u || strlen(ts) < 8u) {
        return -1;
    }

    memset(&tmv, 0, sizeof(tmv));
    if (snprintf(buf, sizeof(buf), "%.8s%.8s", dt, ts) != 16) {
        return -1;
    }

    if (sscanf(buf, "%4d%2d%2d%2d:%2d:%2d",
               &tmv.tm_year, &tmv.tm_mon, &tmv.tm_mday,
               &tmv.tm_hour, &tmv.tm_min, &tmv.tm_sec) != 6) {
        return -1;
    }

    tmv.tm_year -= 1900;
    tmv.tm_mon -= 1;
    tmv.tm_isdst = -1;
    *epoch = (int64_t)mktime(&tmv);
    return (*epoch < 0) ? -1 : 0;
}

static int key_of(char *dst, size_t dst_len, const char *cif, const char *instr)
{
    int n;

    n = snprintf(dst, dst_len, "%s|%s", cif, instr);
    return (n < 0 || (size_t)n >= dst_len) ? -1 : 0;
}

static int same_key(const PosRec *p, const PnlRec *q)
{
    return strcmp(p->cif_no, q->cif_no) == 0 && strcmp(p->instr_code, q->instr_code) == 0;
}

static int read_sccalf(Store *st, const char *path)
{
    FILE *fp;
    char line[MAX_LINE];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "E-SCCALF-OPEN: 暦ファイルを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cur;
        CalRec *r;

        if (st->cal_count >= MAX_CAL) {
            fclose(fp);
            fprintf(stderr, "E-SCCALF-LIMIT: 暦件数が上限を超過しました\n");
            return -1;
        }
        if (strncmp(line, "SESS-DT", 7) == 0) {
            continue;
        }

        r = &st->cal[st->cal_count];
        cur = line;
        if (next_field(&cur, r->sess_dt, sizeof(r->sess_dt)) != 0 ||
            next_field(&cur, r->sess_kbn, sizeof(r->sess_kbn)) != 0 ||
            next_field(&cur, r->open_ts, sizeof(r->open_ts)) != 0 ||
            next_field(&cur, r->close_ts, sizeof(r->close_ts)) != 0 ||
            parse_epoch(r->sess_dt, r->open_ts, &r->open_epoch) != 0 ||
            parse_epoch(r->sess_dt, r->close_ts, &r->close_epoch) != 0 ||
            r->open_epoch >= r->close_epoch) {
            fclose(fp);
            fprintf(stderr, "E-SCCALF-PARSE: 暦行の形式が不正です\n");
            return -1;
        }
        ++st->cal_count;
    }

    if (ferror(fp) != 0) {
        fclose(fp);
        fprintf(stderr, "E-SCCALF-READ: 暦ファイル読込に失敗しました\n");
        return -1;
    }
    fclose(fp);
    return st->cal_count == 0u ? -1 : 0;
}

static int read_scm2mf(Store *st, const char *path)
{
    FILE *fp;
    char line[MAX_LINE];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "E-SCM2MF-OPEN: 建玉ファイルを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cur;
        char f[7][64];
        PosRec *r;
        int64_t abs_qty;

        if (st->pos_count >= MAX_POS) {
            fclose(fp);
            fprintf(stderr, "E-SCM2MF-LIMIT: 建玉件数が上限を超過しました\n");
            return -1;
        }
        if (strncmp(line, "CIF-NO", 6) == 0) {
            continue;
        }

        cur = line;
        if (next_field(&cur, f[0], sizeof(f[0])) != 0 ||
            next_field(&cur, f[1], sizeof(f[1])) != 0 ||
            next_field(&cur, f[2], sizeof(f[2])) != 0 ||
            next_field(&cur, f[3], sizeof(f[3])) != 0 ||
            next_field(&cur, f[4], sizeof(f[4])) != 0 ||
            next_field(&cur, f[5], sizeof(f[5])) != 0 ||
            next_field(&cur, f[6], sizeof(f[6])) != 0) {
            fclose(fp);
            fprintf(stderr, "E-SCM2MF-PARSE: 建玉行の形式が不正です\n");
            return -1;
        }

        r = &st->pos[st->pos_count];
        if (copy_field(r->cif_no, sizeof(r->cif_no), f[0]) != 0 ||
            copy_field(r->instr_code, sizeof(r->instr_code), f[1]) != 0 ||
            copy_field(r->sess_dt, sizeof(r->sess_dt), f[2]) != 0 ||
            parse_i64(f[3], &r->net_qty) != 0 ||
            parse_i64(f[4], &r->mark_amt) != 0 ||
            parse_i64(f[5], &r->mark_notional_amt) != 0 ||
            parse_i64(f[6], &r->unrlzd_amt) != 0) {
            fclose(fp);
            fprintf(stderr, "E-SCM2MF-VALUE: 建玉値が不正です\n");
            return -1;
        }

        abs_qty = r->net_qty < 0 ? -r->net_qty : r->net_qty;
        if (abs_qty != 0 && r->mark_notional_amt / abs_qty != r->mark_amt) {
            r->done_bit = 0u;
        } else {
            r->done_bit = 1u;
        }
        ++st->pos_count;
    }

    if (ferror(fp) != 0) {
        fclose(fp);
        fprintf(stderr, "E-SCM2MF-READ: 建玉ファイル読込に失敗しました\n");
        return -1;
    }
    fclose(fp);
    return 0;
}

static int read_scpnlf(Store *st, const char *path)
{
    FILE *fp;
    char line[MAX_LINE];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "E-SCPNLF-OPEN: 損益ファイルを開けません\n");
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cur;
        char f[7][64];
        PnlRec *r;

        if (st->pnl_count >= MAX_PNL) {
            fclose(fp);
            fprintf(stderr, "E-SCPNLF-LIMIT: 損益件数が上限を超過しました\n");
            return -1;
        }
        if (strncmp(line, "CIF-NO", 6) == 0) {
            continue;
        }

        cur = line;
        if (next_field(&cur, f[0], sizeof(f[0])) != 0 ||
            next_field(&cur, f[1], sizeof(f[1])) != 0 ||
            next_field(&cur, f[2], sizeof(f[2])) != 0 ||
            next_field(&cur, f[3], sizeof(f[3])) != 0 ||
            next_field(&cur, f[4], sizeof(f[4])) != 0 ||
            next_field(&cur, f[5], sizeof(f[5])) != 0 ||
            next_field(&cur, f[6], sizeof(f[6])) != 0) {
            fclose(fp);
            fprintf(stderr, "E-SCPNLF-PARSE: 損益行の形式が不正です\n");
            return -1;
        }

        r = &st->pnl[st->pnl_count];
        if (copy_field(r->cif_no, sizeof(r->cif_no), f[0]) != 0 ||
            copy_field(r->instr_code, sizeof(r->instr_code), f[1]) != 0 ||
            copy_field(r->sess_dt, sizeof(r->sess_dt), f[2]) != 0 ||
            parse_i64(f[3], &r->rlzd_amt) != 0 ||
            parse_i64(f[4], &r->unrlzd_amt) != 0 ||
            parse_i64(f[5], &r->fee_amt) != 0 ||
            copy_field(r->calc_ts, sizeof(r->calc_ts), f[6]) != 0) {
            fclose(fp);
            fprintf(stderr, "E-SCPNLF-VALUE: 損益値が不正です\n");
            return -1;
        }
        ++st->pnl_count;
    }

    if (ferror(fp) != 0) {
        fclose(fp);
        fprintf(stderr, "E-SCPNLF-READ: 損益ファイル読込に失敗しました\n");
        return -1;
    }
    fclose(fp);
    return 0;
}

static const CalRec *find_current_session(const Store *st)
{
    size_t i;
    const CalRec *best = NULL;

    for (i = 0u; i < st->cal_count; ++i) {
        if (best == NULL || st->cal[i].close_epoch > best->close_epoch) {
            best = &st->cal[i];
        }
    }
    return best;
}

static const CalRec *find_next_session(const Store *st, const CalRec *cur)
{
    size_t i;
    const CalRec *next = NULL;

    for (i = 0u; i < st->cal_count; ++i) {
        if (strcmp(st->cal[i].sess_dt, cur->sess_dt) > 0) {
            if (next == NULL || strcmp(st->cal[i].sess_dt, next->sess_dt) < 0) {
                next = &st->cal[i];
            }
        }
    }
    return next;
}

static int has_pnl_for_key(const Store *st, const PosRec *p, const char *sess_dt)
{
    size_t i;

    for (i = 0u; i < st->pnl_count; ++i) {
        if (same_key(p, &st->pnl[i]) && strcmp(st->pnl[i].sess_dt, sess_dt) == 0) {
            return 1;
        }
    }
    return 0;
}

static int pending_old_exec_exists(const Store *st, const CalRec *cur)
{
    size_t i;

    for (i = 0u; i < st->pos_count; ++i) {
        const PosRec *p = &st->pos[i];

        if (strcmp(p->sess_dt, cur->sess_dt) != 0) {
            continue;
        }
        if (p->done_bit == 0u) {
            return 1;
        }
        if (!has_pnl_for_key(st, p, cur->sess_dt)) {
            return 1;
        }
    }
    return 0;
}

static int write_scm2mf(const Store *st, const char *path)
{
    FILE *fp;
    size_t i;

    fp = fopen(path, "w");
    if (fp == NULL) {
        fprintf(stderr, "E-SCM2MF-WOPEN: 建玉出力を開けません\n");
        return -1;
    }

    fprintf(fp, "CIF-NO,INSTR-CODE,SESS-DT,NET-QTY,MARK-AMT,MARK-NOTIONAL-AMT,UNRLZD-AMT\n");
    for (i = 0u; i < st->pos_count; ++i) {
        const PosRec *r = &st->pos[i];

        if (fprintf(fp, "%s,%s,%s,%" PRId64 ",%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
                    r->cif_no, r->instr_code, r->sess_dt, r->net_qty, r->mark_amt,
                    r->mark_notional_amt, r->unrlzd_amt) < 0) {
            fclose(fp);
            fprintf(stderr, "E-SCM2MF-WRITE: 建玉出力に失敗しました\n");
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "E-SCM2MF-CLOSE: 建玉出力の確定に失敗しました\n");
        return -1;
    }
    return 0;
}

static int write_scpnlf(const Store *st, const char *path)
{
    FILE *fp;
    size_t i;

    fp = fopen(path, "w");
    if (fp == NULL) {
        fprintf(stderr, "E-SCPNLF-WOPEN: 損益出力を開けません\n");
        return -1;
    }

    fprintf(fp, "CIF-NO,INSTR-CODE,SESS-DT,RLZD-AMT,UNRLZD-AMT,FEE-AMT,CALC-TS\n");
    for (i = 0u; i < st->pnl_count; ++i) {
        const PnlRec *r = &st->pnl[i];

        if (fprintf(fp, "%s,%s,%s,%" PRId64 ",%" PRId64 ",%" PRId64 ",%s\n",
                    r->cif_no, r->instr_code, r->sess_dt, r->rlzd_amt,
                    r->unrlzd_amt, r->fee_amt, r->calc_ts) < 0) {
            fclose(fp);
            fprintf(stderr, "E-SCPNLF-WRITE: 損益出力に失敗しました\n");
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "E-SCPNLF-CLOSE: 損益出力の確定に失敗しました\n");
        return -1;
    }
    return 0;
}

static int append_rollover(Store *st, const CalRec *cur, const CalRec *next)
{
    size_t i;
    char now_ts[TS_LEN];

    if (snprintf(now_ts, sizeof(now_ts), "%sT%s", cur->sess_dt, cur->close_ts) < 0) {
        return -1;
    }

    for (i = 0u; i < st->pos_count; ++i) {
        PosRec *p = &st->pos[i];

        if (strcmp(p->sess_dt, cur->sess_dt) != 0) {
            continue;
        }
        if (st->pnl_count >= MAX_PNL) {
            fprintf(stderr, "E-ROLLOVER-LIMIT: 損益ロール件数が上限を超過しました\n");
            return -1;
        }

        if (!has_pnl_for_key(st, p, next->sess_dt)) {
            PnlRec *q = &st->pnl[st->pnl_count++];

            if (copy_field(q->cif_no, sizeof(q->cif_no), p->cif_no) != 0 ||
                copy_field(q->instr_code, sizeof(q->instr_code), p->instr_code) != 0 ||
                copy_field(q->sess_dt, sizeof(q->sess_dt), next->sess_dt) != 0 ||
                copy_field(q->calc_ts, sizeof(q->calc_ts), now_ts) != 0) {
                return -1;
            }
            q->rlzd_amt = 0;
            q->unrlzd_amt = p->unrlzd_amt;
            q->fee_amt = 0;
        }

        if (copy_field(p->sess_dt, sizeof(p->sess_dt), next->sess_dt) != 0) {
            return -1;
        }
        p->done_bit = 0u;
    }

    return 0;
}

int main(void)
{
    Store st;
    const CalRec *cur;
    const CalRec *next;

    memset(&st, 0, sizeof(st));

    if (read_sccalf(&st, "SCCALF.csv") != 0 ||
        read_scm2mf(&st, "SCM2MF.csv") != 0 ||
        read_scpnlf(&st, "SCPNLF.csv") != 0) {
        return MIHFT_DECISION_IO_ERROR;
    }

    cur = find_current_session(&st);
    if (cur == NULL) {
        fprintf(stderr, "E-SESSION-NONE: 対象セッションがありません\n");
        return MIHFT_DECISION_REJECT;
    }

    next = find_next_session(&st, cur);
    if (next == NULL) {
        fprintf(stderr, "W-SESSION-NEXT: 翌営業日セッションが未登録です\n");
        return MIHFT_DECISION_DEFER;
    }

    if (pending_old_exec_exists(&st, cur)) {
        fprintf(stderr, "W-ROLLOVER-DEFER: 旧セッションに未完了約定または未集計銘柄があります\n");
        return MIHFT_DECISION_DEFER;
    }

    if (append_rollover(&st, cur, next) != 0) {
        return MIHFT_DECISION_IO_ERROR;
    }

    if (write_scm2mf(&st, "SCM2MF.out.csv") != 0 ||
        write_scpnlf(&st, "SCPNLF.out.csv") != 0) {
        return MIHFT_DECISION_IO_ERROR;
    }

    return MIHFT_DECISION_OK;
}
