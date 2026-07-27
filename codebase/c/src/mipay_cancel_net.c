/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240415  精算連携  初版作成
 * 1.01  20240902  精算連携  金額差分繰越判定を追加
 * 1.02  20241210  精算連携  取消相殺出力と繰越登録を統合
 */

#include "mipay_trace.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MIPAY_DECISION_ACCEPT
#define MIPAY_DECISION_ACCEPT 0
#endif

#define MI_MAX_LINE 512
#define MI_MAX_KEY 64
#define MI_MAX_MERCHANT 32
#define MI_MAX_KBN 8
#define MI_MAX_REASON 32
#define MI_MAX_DATE 16
#define MI_MAX_DETAIL 96
#define MI_MAX_CANCEL 96
#define MI_MAX_ROWS 8192
#define MI_STATUS_DONE "1"
#define MI_STATUS_CANCEL "20"
#define MI_STATUS_FIXED "30"
#define MI_KBN_IMMEDIATE "1"
#define MI_KBN_NEXT "2"
#define MI_KBN_OUT "9"

typedef struct {
    char cancel_id[MI_MAX_KEY];
    char cap_id[MI_MAX_KEY];
    char hold_id[MI_MAX_KEY];
    char merchant_code[MI_MAX_MERCHANT];
    long cancel_amt;
    char cancel_status[MI_MAX_KBN];
} MiCancelRow;

typedef struct {
    char detail_id[MI_MAX_KEY];
    char settle_txn_id[MI_MAX_KEY];
    char merchant_code[MI_MAX_MERCHANT];
    long txn_amt;
    char settle_kbn[MI_MAX_KBN];
    char output_status[MI_MAX_KBN];
    int appended;
} MiDetailRow;

typedef struct {
    char settle_txn_id[MI_MAX_KEY];
    char merchant_code[MI_MAX_MERCHANT];
    long txn_amt;
    char settle_kbn[MI_MAX_KBN];
} MiSettleRow;

typedef struct {
    MiCancelRow cancel_rows[MI_MAX_ROWS];
    MiDetailRow detail_rows[MI_MAX_ROWS];
    MiSettleRow settle_rows[MI_MAX_ROWS];
    size_t cancel_count;
    size_t detail_count;
    size_t settle_count;
    size_t offset_count;
    size_t carry_count;
} MiWork;

static int mi_copy_field(char *dst, size_t dst_size, const char *src)
{
    size_t n;

    if (dst_size == 0 || src == NULL) {
        return -1;
    }

    n = strlen(src);
    if (n >= dst_size) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static void mi_chomp(char *line)
{
    size_t n = strlen(line);

    while (n > 0 && (line[n - 1] == '\n' || line[n - 1] == '\r')) {
        line[--n] = '\0';
    }
}

static int mi_split_csv(char *line, char **fields, size_t field_max, size_t *field_count)
{
    char *p = line;
    size_t count = 0;

    while (*p != '\0') {
        char *start;

        if (count >= field_max) {
            return -1;
        }

        start = p;
        while (*p != '\0' && *p != ',') {
            p++;
        }

        if (*p == ',') {
            *p = '\0';
            p++;
        }

        fields[count++] = start;
    }

    if (count < field_max && p > line && p[-1] == '\0') {
        fields[count++] = p;
    }

    *field_count = count;
    return 0;
}

static int mi_parse_amount(const char *s, long *out)
{
    char *end;
    long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtol(s, &end, 10);
    if (errno == ERANGE || end == s || *end != '\0' || v < 0) {
        return -1;
    }

    *out = v;
    return 0;
}

static int mi_read_cancel(const char *path, MiWork *work)
{
    FILE *fp;
    char line[MI_MAX_LINE];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "E101:取消予定ファイルを開けません:%s\n", path);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];
        size_t n = 0;
        MiCancelRow *r;

        mi_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (work->cancel_count >= MI_MAX_ROWS) {
            fprintf(stderr, "E102:取消予定件数が上限を超過\n");
            fclose(fp);
            return -1;
        }
        if (mi_split_csv(line, f, 6, &n) != 0 || n != 6) {
            fprintf(stderr, "E103:取消予定CSV形式不正\n");
            fclose(fp);
            return -1;
        }

        r = &work->cancel_rows[work->cancel_count];
        if (mi_copy_field(r->cancel_id, sizeof(r->cancel_id), f[0]) != 0 ||
            mi_copy_field(r->cap_id, sizeof(r->cap_id), f[1]) != 0 ||
            mi_copy_field(r->hold_id, sizeof(r->hold_id), f[2]) != 0 ||
            mi_copy_field(r->merchant_code, sizeof(r->merchant_code), f[3]) != 0 ||
            mi_parse_amount(f[4], &r->cancel_amt) != 0 ||
            mi_copy_field(r->cancel_status, sizeof(r->cancel_status), f[5]) != 0) {
            fprintf(stderr, "E104:取消予定項目不正\n");
            fclose(fp);
            return -1;
        }

        work->cancel_count++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "E105:取消予定読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int mi_read_detail(const char *path, MiWork *work)
{
    FILE *fp;
    char line[MI_MAX_LINE];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "E201:精算明細ファイルを開けません:%s\n", path);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];
        size_t n = 0;
        MiDetailRow *r;

        mi_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (work->detail_count >= MI_MAX_ROWS) {
            fprintf(stderr, "E202:精算明細件数が上限を超過\n");
            fclose(fp);
            return -1;
        }
        if (mi_split_csv(line, f, 6, &n) != 0 || n != 6) {
            fprintf(stderr, "E203:精算明細CSV形式不正\n");
            fclose(fp);
            return -1;
        }

        r = &work->detail_rows[work->detail_count];
        if (mi_copy_field(r->detail_id, sizeof(r->detail_id), f[0]) != 0 ||
            mi_copy_field(r->settle_txn_id, sizeof(r->settle_txn_id), f[1]) != 0 ||
            mi_copy_field(r->merchant_code, sizeof(r->merchant_code), f[2]) != 0 ||
            mi_parse_amount(f[3], &r->txn_amt) != 0 ||
            mi_copy_field(r->settle_kbn, sizeof(r->settle_kbn), f[4]) != 0 ||
            mi_copy_field(r->output_status, sizeof(r->output_status), f[5]) != 0) {
            fprintf(stderr, "E204:精算明細項目不正\n");
            fclose(fp);
            return -1;
        }

        r->appended = 0;
        work->detail_count++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "E205:精算明細読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int mi_read_settle(const char *path, MiWork *work)
{
    FILE *fp;
    char line[MI_MAX_LINE];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "E301:当日精算ファイルを開けません:%s\n", path);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[4];
        size_t n = 0;
        MiSettleRow *r;

        mi_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (work->settle_count >= MI_MAX_ROWS) {
            fprintf(stderr, "E302:当日精算件数が上限を超過\n");
            fclose(fp);
            return -1;
        }
        if (mi_split_csv(line, f, 4, &n) != 0 || n != 4) {
            fprintf(stderr, "E303:当日精算CSV形式不正\n");
            fclose(fp);
            return -1;
        }

        r = &work->settle_rows[work->settle_count];
        if (mi_copy_field(r->settle_txn_id, sizeof(r->settle_txn_id), f[0]) != 0 ||
            mi_copy_field(r->merchant_code, sizeof(r->merchant_code), f[1]) != 0 ||
            mi_parse_amount(f[2], &r->txn_amt) != 0 ||
            mi_copy_field(r->settle_kbn, sizeof(r->settle_kbn), f[3]) != 0) {
            fprintf(stderr, "E304:当日精算項目不正\n");
            fclose(fp);
            return -1;
        }

        work->settle_count++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "E305:当日精算読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static const MiDetailRow *mi_find_output_detail(const MiWork *work, const MiCancelRow *cancel)
{
    size_t i;

    for (i = 0; i < work->detail_count; i++) {
        const MiDetailRow *d = &work->detail_rows[i];

        if (strcmp(d->settle_txn_id, cancel->cap_id) == 0 &&
            strcmp(d->merchant_code, cancel->merchant_code) == 0 &&
            strcmp(d->output_status, MI_STATUS_DONE) == 0) {
            return d;
        }
    }

    return NULL;
}

static const MiSettleRow *mi_find_today_settle(const MiWork *work, const MiCancelRow *cancel)
{
    size_t i;

    for (i = 0; i < work->settle_count; i++) {
        const MiSettleRow *s = &work->settle_rows[i];

        if (strcmp(s->settle_txn_id, cancel->cap_id) == 0 &&
            strcmp(s->merchant_code, cancel->merchant_code) == 0) {
            return s;
        }
    }

    return NULL;
}

static int mi_next_id(char *dst, size_t dst_size, const char *prefix, size_t seq)
{
    int n = snprintf(dst, dst_size, "%s%08lu", prefix, (unsigned long)seq);

    if (n < 0 || (size_t)n >= dst_size) {
        return -1;
    }

    return 0;
}

static int mi_write_offset(FILE *fp, const MiCancelRow *cancel, const MiDetailRow *detail, size_t seq)
{
    char id[MI_MAX_KEY];
    long signed_amt;

    if (detail->txn_amt > LONG_MAX - cancel->cancel_amt) {
        fprintf(stderr, "E401:相殺金額計算が上限を超過\n");
        return -1;
    }

    signed_amt = -cancel->cancel_amt;
    if (mi_next_id(id, sizeof(id), "D", seq) != 0) {
        fprintf(stderr, "E402:相殺明細番号の生成失敗\n");
        return -1;
    }

    if (fprintf(fp, "%s,%s,%s,%ld,%s,%s\n",
                id,
                detail->settle_txn_id,
                detail->merchant_code,
                signed_amt,
                detail->settle_kbn,
                MI_STATUS_DONE) < 0) {
        fprintf(stderr, "E403:相殺明細出力失敗\n");
        return -1;
    }

    return 0;
}

static int mi_write_carry(FILE *fp, const MiCancelRow *cancel, const char *settle_kbn,
                          long carry_amt, const char *reason, size_t seq)
{
    char id[MI_MAX_KEY];

    if (mi_next_id(id, sizeof(id), "C", seq) != 0) {
        fprintf(stderr, "E501:繰越番号の生成失敗\n");
        return -1;
    }

    if (fprintf(fp, "%s,%s,%s,%ld,%s,%s\n",
                id,
                cancel->merchant_code,
                settle_kbn,
                carry_amt,
                reason,
                "翌営業日") < 0) {
        fprintf(stderr, "E502:繰越取消出力失敗\n");
        return -1;
    }

    return 0;
}

static int mi_process(MiWork *work)
{
    FILE *detail_out;
    FILE *carry_out;
    size_t i;
    int rc = 0;

    detail_out = fopen("PCDTLF.csv", "a");
    if (detail_out == NULL) {
        fprintf(stderr, "E601:精算明細追記を開始できません\n");
        return -1;
    }

    carry_out = fopen("PCCARF.csv", "a");
    if (carry_out == NULL) {
        fprintf(stderr, "E602:繰越取消追記を開始できません\n");
        fclose(detail_out);
        return -1;
    }

    for (i = 0; i < work->cancel_count; i++) {
        const MiCancelRow *c = &work->cancel_rows[i];
        const MiDetailRow *d;
        const MiSettleRow *s;

        if (strcmp(c->cancel_status, MI_STATUS_CANCEL) != 0) {
            continue;
        }

        d = mi_find_output_detail(work, c);
        s = mi_find_today_settle(work, c);

        if (d != NULL && c->cancel_amt == d->txn_amt && strcmp(d->settle_kbn, MI_KBN_IMMEDIATE) == 0) {
            if (mi_write_offset(detail_out, c, d, work->detail_count + work->offset_count + 1) != 0) {
                rc = -1;
                break;
            }
            work->offset_count++;
        } else {
            const char *kbn = d != NULL ? d->settle_kbn : (s != NULL ? s->settle_kbn : MI_KBN_NEXT);
            const char *reason = "未相殺";
            long amt = c->cancel_amt;

            if (d == NULL && s == NULL) {
                reason = "精算未出力";
            } else if (d != NULL && c->cancel_amt != d->txn_amt) {
                reason = "金額差";
                amt = c->cancel_amt > d->txn_amt ? c->cancel_amt - d->txn_amt : d->txn_amt - c->cancel_amt;
            } else if (strcmp(kbn, MI_KBN_OUT) == 0) {
                reason = "対象外";
            } else if (strcmp(kbn, MI_KBN_NEXT) == 0) {
                reason = "翌月繰越";
            } else {
                reason = "精算日差";
            }

            if (mi_write_carry(carry_out, c, kbn, amt, reason, work->carry_count + 1) != 0) {
                rc = -1;
                break;
            }
            work->carry_count++;
        }
    }

    if (fclose(detail_out) != 0) {
        fprintf(stderr, "E603:精算明細追記の終了失敗\n");
        rc = -1;
    }
    if (fclose(carry_out) != 0) {
        fprintf(stderr, "E604:繰越取消追記の終了失敗\n");
        rc = -1;
    }

    return rc;
}

int main(void)
{
    MiWork work;

    memset(&work, 0, sizeof(work));

    if (mi_read_cancel("PTCANF.csv", &work) != 0) {
        return 101;
    }
    if (mi_read_detail("PCDTLF.csv", &work) != 0) {
        return 201;
    }
    if (mi_read_settle("PTSETF.csv", &work) != 0) {
        return 301;
    }
    if (mi_process(&work) != 0) {
        return 401;
    }

    return MIPAY_DECISION_ACCEPT;
}
