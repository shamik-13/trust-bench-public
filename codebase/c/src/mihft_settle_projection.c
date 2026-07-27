/*
 * 変更履歴
 * 版数  年月日    担当    概要
 * 1.00  20210715  中川 美和 (E-283)  初版作成
 * 1.01  20211215  村上 健司 (E-301)  半日立会の翌営業日繰延を追加
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_OK_ACCEPT 0
#define MIHFT_ERR_IO 20
#define MIHFT_ERR_PARSE 24
#define MIHFT_ERR_RANGE 28

#define MIHFT_MAX_LINE 512
#define MIHFT_MAX_EXEC 200000
#define MIHFT_MAX_CAL 4096
#define MIHFT_FIELD_MAX 8
#define MIHFT_ID_MAX 40
#define MIHFT_CODE_MAX 24
#define MIHFT_TS_MAX 20
#define MIHFT_STATUS_UNSETTLED "0"

typedef struct {
    char exec_id[MIHFT_ID_MAX];
    char order_id[MIHFT_ID_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[MIHFT_TS_MAX];
    int exec_dt;
} MihftExecRec;

typedef struct {
    int sess_dt;
    char sess_kbn[8];
    char open_ts[MIHFT_TS_MAX];
    char close_ts[MIHFT_TS_MAX];
    int half_day;
    int active;
} MihftCalRec;

static void mihft_rstrip(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r' || s[n - 1] == ' ' || s[n - 1] == '\t')) {
        s[--n] = '\0';
    }
}

static char *mihft_lskip(char *s)
{
    while (*s == ' ' || *s == '\t') {
        ++s;
    }
    return s;
}

static int mihft_split_csv(char *line, char **field, int max_field)
{
    int n = 0;
    char *p = line;

    while (n < max_field) {
        char *start = p;
        char *out = p;
        int quote = 0;

        if (*p == '"') {
            quote = 1;
            ++start;
            ++p;
        }

        if (quote) {
            out = start;
            while (*p != '\0') {
                if (*p == '"' && p[1] == '"') {
                    *out++ = '"';
                    p += 2;
                } else if (*p == '"') {
                    ++p;
                    break;
                } else {
                    *out++ = *p++;
                }
            }
            *out = '\0';
            if (*p == ',') {
                *p++ = '\0';
            }
        } else {
            while (*p != '\0' && *p != ',') {
                ++p;
            }
            if (*p == ',') {
                *p++ = '\0';
            }
            mihft_rstrip(start);
            start = mihft_lskip(start);
        }

        field[n++] = start;
        if (*p == '\0') {
            break;
        }
    }

    return n;
}

static int mihft_parse_i64(const char *s, int64_t *v)
{
    char *end = NULL;
    long long x;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    x = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return -1;
    }

    *v = (int64_t)x;
    return 0;
}

static int mihft_parse_yyyymmdd(const char *s, int *dt)
{
    int y;
    int m;
    int d;

    if (s == NULL || strlen(s) < 8) {
        return -1;
    }

    if (!isdigit((unsigned char)s[0]) || !isdigit((unsigned char)s[1]) ||
        !isdigit((unsigned char)s[2]) || !isdigit((unsigned char)s[3]) ||
        !isdigit((unsigned char)s[4]) || !isdigit((unsigned char)s[5]) ||
        !isdigit((unsigned char)s[6]) || !isdigit((unsigned char)s[7])) {
        return -1;
    }

    y = (s[0] - '0') * 1000 + (s[1] - '0') * 100 + (s[2] - '0') * 10 + (s[3] - '0');
    m = (s[4] - '0') * 10 + (s[5] - '0');
    d = (s[6] - '0') * 10 + (s[7] - '0');

    if (y < 2000 || y > 2099 || m < 1 || m > 12 || d < 1 || d > 31) {
        return -1;
    }

    *dt = y * 10000 + m * 100 + d;
    return 0;
}

static int mihft_parse_exec_dt(const char *ts, int *dt)
{
    char buf[9];

    if (ts == NULL || strlen(ts) < 8) {
        return -1;
    }

    memcpy(buf, ts, 8);
    buf[8] = '\0';
    return mihft_parse_yyyymmdd(buf, dt);
}

static int mihft_copy_text(char *dst, size_t cap, const char *src)
{
    size_t n;

    if (dst == NULL || cap == 0 || src == NULL) {
        return -1;
    }

    n = strlen(src);
    if (n >= cap) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static int mihft_is_header_exec(char **f, int n)
{
    return n >= 7 && strcmp(f[0], "EXEC-ID") == 0;
}

static int mihft_is_header_cal(char **f, int n)
{
    return n >= 4 && strcmp(f[0], "SESS-DT") == 0;
}

static int mihft_load_exec(const char *path, MihftExecRec *rec, size_t cap, size_t *cnt)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0;
    unsigned long row = 0;

    if (fp == NULL) {
        fprintf(stderr, "入力ファイルを開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[MIHFT_FIELD_MAX];
        int nf;
        MihftExecRec r;
        int64_t qty;
        int64_t amt;

        ++row;
        mihft_rstrip(line);
        if (line[0] == '\0') {
            continue;
        }

        nf = mihft_split_csv(line, f, MIHFT_FIELD_MAX);
        if (mihft_is_header_exec(f, nf)) {
            continue;
        }
        if (nf != 7) {
            fprintf(stderr, "約定入力の項目数が不正です: %lu\n", row);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (n >= cap) {
            fprintf(stderr, "約定入力の上限を超過しました: %lu\n", row);
            fclose(fp);
            return MIHFT_ERR_RANGE;
        }
        if (mihft_parse_i64(f[4], &qty) != 0 || mihft_parse_i64(f[5], &amt) != 0 ||
            qty <= 0 || amt <= 0 || amt > MIHFT_MAX_NOTIONAL) {
            fprintf(stderr, "約定入力の数値が不正です: %lu\n", row);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        memset(&r, 0, sizeof(r));
        if (mihft_copy_text(r.exec_id, sizeof(r.exec_id), f[0]) != 0 ||
            mihft_copy_text(r.order_id, sizeof(r.order_id), f[1]) != 0 ||
            mihft_copy_text(r.instr_code, sizeof(r.instr_code), f[2]) != 0 ||
            mihft_copy_text(r.exec_ts, sizeof(r.exec_ts), f[6]) != 0) {
            fprintf(stderr, "約定入力の文字長が不正です: %lu\n", row);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if ((f[3][0] != 'B' && f[3][0] != 'S') || f[3][1] != '\0') {
            fprintf(stderr, "売買区分が不正です: %lu\n", row);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (mihft_parse_exec_dt(r.exec_ts, &r.exec_dt) != 0) {
            fprintf(stderr, "約定時刻が不正です: %lu\n", row);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        r.side_kbn = f[3][0];
        r.fill_qty = qty;
        r.fill_amt = amt;
        rec[n++] = r;
    }

    if (ferror(fp)) {
        fprintf(stderr, "約定入力の読込に失敗しました\n");
        fclose(fp);
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    *cnt = n;
    return MIHFT_OK_ACCEPT;
}

static int mihft_load_cal(const char *path, MihftCalRec *rec, size_t cap, size_t *cnt)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0;
    unsigned long row = 0;

    if (fp == NULL) {
        fprintf(stderr, "営業日入力を開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[MIHFT_FIELD_MAX];
        int nf;
        MihftCalRec r;

        ++row;
        mihft_rstrip(line);
        if (line[0] == '\0') {
            continue;
        }

        nf = mihft_split_csv(line, f, MIHFT_FIELD_MAX);
        if (mihft_is_header_cal(f, nf)) {
            continue;
        }
        if (nf != 4) {
            fprintf(stderr, "営業日入力の項目数が不正です: %lu\n", row);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (n >= cap) {
            fprintf(stderr, "営業日入力の上限を超過しました: %lu\n", row);
            fclose(fp);
            return MIHFT_ERR_RANGE;
        }

        memset(&r, 0, sizeof(r));
        if (mihft_parse_yyyymmdd(f[0], &r.sess_dt) != 0 ||
            mihft_copy_text(r.sess_kbn, sizeof(r.sess_kbn), f[1]) != 0 ||
            mihft_copy_text(r.open_ts, sizeof(r.open_ts), f[2]) != 0 ||
            mihft_copy_text(r.close_ts, sizeof(r.close_ts), f[3]) != 0) {
            fprintf(stderr, "営業日入力の値が不正です: %lu\n", row);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }

        r.active = strcmp(r.sess_kbn, "C") != 0 && strcmp(r.sess_kbn, "休") != 0;
        r.half_day = strcmp(r.sess_kbn, "H") == 0 || strcmp(r.sess_kbn, "半") == 0;
        rec[n++] = r;
    }

    if (ferror(fp)) {
        fprintf(stderr, "営業日入力の読込に失敗しました\n");
        fclose(fp);
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    *cnt = n;
    return MIHFT_OK_ACCEPT;
}

static int mihft_cmp_cal(const void *a, const void *b)
{
    const MihftCalRec *x = (const MihftCalRec *)a;
    const MihftCalRec *y = (const MihftCalRec *)b;

    return (x->sess_dt > y->sess_dt) - (x->sess_dt < y->sess_dt);
}

static int mihft_find_settle_dt(const MihftCalRec *cal, size_t ncal, int exec_dt, int *settle_dt)
{
    size_t i;
    int seen_exec = 0;
    int passed = 0;

    for (i = 0; i < ncal; ++i) {
        if (cal[i].sess_dt < exec_dt || !cal[i].active || cal[i].half_day) {
            continue;
        }

        if (!seen_exec) {
            seen_exec = 1;
            continue;
        }

        if (++passed >= 2) {
            *settle_dt = cal[i].sess_dt;
            return 0;
        }
    }

    return -1;
}

static int mihft_tier_rate_bp(const char *instr_code)
{
    unsigned long h = 1469598103UL;
    const unsigned char *p = (const unsigned char *)instr_code;

    while (*p != '\0') {
        h ^= (unsigned long)*p++;
        h *= 16777619UL;
    }

    switch ((int)(h % 3UL)) {
    case 0:
        return 1000;
    case 1:
        return 2000;
    default:
        return 4000;
    }
}

static int mihft_calc_net_cash(const MihftExecRec *e, int64_t *net_cash)
{
    int bp = mihft_tier_rate_bp(e->instr_code);
    int64_t fee;
    int64_t cash;

    if (e->fill_amt > INT64_MAX / bp) {
        return -1;
    }

    fee = (e->fill_amt * bp + 9999) / 10000;

    if (e->side_kbn == 'B') {
        if (e->fill_amt > INT64_MAX - fee) {
            return -1;
        }
        cash = -(e->fill_amt + fee);
    } else {
        if (e->fill_amt < fee) {
            return -1;
        }
        cash = e->fill_amt - fee;
    }

    *net_cash = cash;
    return 0;
}

static unsigned long mihft_cif_no(const char *order_id)
{
    unsigned long h = 2166136261UL;
    const unsigned char *p = (const unsigned char *)order_id;

    while (*p != '\0') {
        h ^= (unsigned long)*p++;
        h *= 16777619UL;
    }

    return 10000000UL + (h % 90000000UL);
}

static int mihft_write_settle(const char *path, const MihftExecRec *exec, size_t nexec, const MihftCalRec *cal, size_t ncal)
{
    FILE *fp = fopen(path, "w");
    size_t i;

    if (fp == NULL) {
        fprintf(stderr, "決済予定出力を開けません: %s\n", path);
        return MIHFT_ERR_IO;
    }

    if (fprintf(fp, "SETTLE-ID,CIF-NO,INSTR-CODE,SETTLE-DT,NET-QTY,NET-CASH-AMT,STATUS-KBN\n") < 0) {
        fprintf(stderr, "決済予定ヘッダの出力に失敗しました\n");
        fclose(fp);
        return MIHFT_ERR_IO;
    }

    for (i = 0; i < nexec; ++i) {
        int settle_dt;
        int64_t net_qty;
        int64_t net_cash;

        if (mihft_find_settle_dt(cal, ncal, exec[i].exec_dt, &settle_dt) != 0) {
            fprintf(stderr, "決済予定日を決定できません: %s\n", exec[i].exec_id);
            fclose(fp);
            return MIHFT_ERR_PARSE;
        }
        if (mihft_calc_net_cash(&exec[i], &net_cash) != 0) {
            fprintf(stderr, "概算受渡金額が範囲外です: %s\n", exec[i].exec_id);
            fclose(fp);
            return MIHFT_ERR_RANGE;
        }

        net_qty = exec[i].side_kbn == 'B' ? exec[i].fill_qty : -exec[i].fill_qty;

        if (fprintf(fp, "%s-STL,%08lu,%s,%08d,%lld,%lld,%s\n",
                    exec[i].exec_id,
                    mihft_cif_no(exec[i].order_id),
                    exec[i].instr_code,
                    settle_dt,
                    (long long)net_qty,
                    (long long)net_cash,
                    MIHFT_STATUS_UNSETTLED) < 0) {
            fprintf(stderr, "決済予定明細の出力に失敗しました: %s\n", exec[i].exec_id);
            fclose(fp);
            return MIHFT_ERR_IO;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "決済予定出力の終了に失敗しました\n");
        return MIHFT_ERR_IO;
    }

    return MIHFT_OK_ACCEPT;
}

int main(void)
{
    static MihftExecRec exec[MIHFT_MAX_EXEC];
    static MihftCalRec cal[MIHFT_MAX_CAL];
    size_t nexec = 0;
    size_t ncal = 0;
    int rc;

    rc = mihft_load_exec("SCEXEC.csv", exec, MIHFT_MAX_EXEC, &nexec);
    if (rc != MIHFT_OK_ACCEPT) {
        return rc;
    }

    rc = mihft_load_cal("SCCALF.csv", cal, MIHFT_MAX_CAL, &ncal);
    if (rc != MIHFT_OK_ACCEPT) {
        return rc;
    }

    qsort(cal, ncal, sizeof(cal[0]), mihft_cmp_cal);

    rc = mihft_write_settle("SCSETF.csv", exec, nexec, cal, ncal);
    if (rc != MIHFT_OK_ACCEPT) {
        return rc;
    }

    return MIHFT_OK_ACCEPT;
}
