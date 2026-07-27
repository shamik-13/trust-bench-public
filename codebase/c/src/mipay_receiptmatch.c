/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    20240409    精算基盤    初版作成
 * 1.01    20241008    精算基盤    銀行結果照合と過不足記録を追加
 * 1.02    20250303    精算基盤    CSV検証と桁あふれ検査を強化
 */
#include "mipay_settle.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MR_SETTLEABLE_STATUS "01"
#define PROC_CHARGE_BP 30L
#define MAX_LINE_LEN 512
#define MAX_RECEIPTS 20000
#define MAX_SETTLES 20000
#define MAX_PAYOUTS 20000
#define ID_LEN 32
#define MER_LEN 24
#define DATE_LEN 16
#define BANK_ACCT_LEN 40
#define RESULT_LEN 8
#define STATUS_LEN 16
#define MATCH_OK "MATCH"
#define MATCH_DIFF "DIFF"
#define MATCH_MISS "MISS"
#define MATCH_ERR "ERROR"
#define BANK_OK "00"

struct receipt_row {
    char receipt_id[ID_LEN];
    char merchant_code[MER_LEN];
    long long receipt_amt;
    char receipt_dt[DATE_LEN];
    char match_status[STATUS_LEN];
    char settle_id[ID_LEN];
};

struct settle_row {
    char settle_id[ID_LEN];
    char merchant_code[MER_LEN];
    long long net_amt;
    long long charge_amt;
    long long payout_amt;
    char settle_dt[DATE_LEN];
};

struct payout_row {
    char payout_id[ID_LEN];
    char merchant_code[MER_LEN];
    char bank_acct_no[BANK_ACCT_LEN];
    long long payout_amt;
    char payout_dt[DATE_LEN];
    char bank_result_cd[RESULT_LEN];
};

static void log_msg(const char *code, const char *detail)
{
    if (detail != NULL && detail[0] != '\0') {
        fprintf(stderr, "%s:%s\n", code, detail);
    } else {
        fprintf(stderr, "%s\n", code);
    }
}

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t len;

    if (dst_len == 0 || src == NULL) {
        return -1;
    }

    while (*src != '\0' && isspace((unsigned char)*src)) {
        src++;
    }

    len = strlen(src);
    while (len > 0 && isspace((unsigned char)src[len - 1])) {
        len--;
    }

    if (len >= dst_len) {
        return -1;
    }

    memcpy(dst, src, len);
    dst[len] = '\0';
    return 0;
}

static int parse_money(const char *src, long long *out)
{
    char *endp;
    long long val;

    if (src == NULL || out == NULL || src[0] == '\0') {
        return -1;
    }

    errno = 0;
    val = strtoll(src, &endp, 10);
    if (errno == ERANGE || endp == src) {
        return -1;
    }

    while (*endp != '\0') {
        if (!isspace((unsigned char)*endp)) {
            return -1;
        }
        endp++;
    }

    if (val < 0) {
        return -1;
    }

    *out = val;
    return 0;
}

static int split_csv_line(char *line, char **fields, size_t want)
{
    size_t n = 0;
    char *p = line;

    if (line == NULL || fields == NULL || want == 0) {
        return -1;
    }

    line[strcspn(line, "\r\n")] = '\0';

    while (n < want) {
        fields[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p = '\0';
        p++;
    }

    return n == want && strchr(fields[want - 1], ',') == NULL ? 0 : -1;
}

static int read_receipts(const char *path, struct receipt_row *rows, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MAX_LINE_LEN];
    size_t n = 0;
    unsigned long lineno = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        log_msg("E-PSRCVF-OPEN", path);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];

        lineno++;
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '#') {
            continue;
        }
        if (n >= cap) {
            log_msg("E-PSRCVF-MAX", "件数上限超過");
            fclose(fp);
            return -1;
        }
        if (split_csv_line(line, f, 6) != 0 ||
            copy_field(rows[n].receipt_id, sizeof(rows[n].receipt_id), f[0]) != 0 ||
            copy_field(rows[n].merchant_code, sizeof(rows[n].merchant_code), f[1]) != 0 ||
            parse_money(f[2], &rows[n].receipt_amt) != 0 ||
            copy_field(rows[n].receipt_dt, sizeof(rows[n].receipt_dt), f[3]) != 0 ||
            copy_field(rows[n].match_status, sizeof(rows[n].match_status), f[4]) != 0 ||
            copy_field(rows[n].settle_id, sizeof(rows[n].settle_id), f[5]) != 0) {
            char buf[64];
            snprintf(buf, sizeof(buf), "%lu", lineno);
            log_msg("E-PSRCVF-PARSE", buf);
            fclose(fp);
            return -1;
        }
        n++;
    }

    if (ferror(fp)) {
        log_msg("E-PSRCVF-READ", path);
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int read_settles(const char *path, struct settle_row *rows, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MAX_LINE_LEN];
    size_t n = 0;
    unsigned long lineno = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        log_msg("E-PSSETF-OPEN", path);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];

        lineno++;
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '#') {
            continue;
        }
        if (n >= cap) {
            log_msg("E-PSSETF-MAX", "件数上限超過");
            fclose(fp);
            return -1;
        }
        if (split_csv_line(line, f, 6) != 0 ||
            copy_field(rows[n].settle_id, sizeof(rows[n].settle_id), f[0]) != 0 ||
            copy_field(rows[n].merchant_code, sizeof(rows[n].merchant_code), f[1]) != 0 ||
            parse_money(f[2], &rows[n].net_amt) != 0 ||
            parse_money(f[3], &rows[n].charge_amt) != 0 ||
            parse_money(f[4], &rows[n].payout_amt) != 0 ||
            copy_field(rows[n].settle_dt, sizeof(rows[n].settle_dt), f[5]) != 0) {
            char buf[64];
            snprintf(buf, sizeof(buf), "%lu", lineno);
            log_msg("E-PSSETF-PARSE", buf);
            fclose(fp);
            return -1;
        }
        n++;
    }

    if (ferror(fp)) {
        log_msg("E-PSSETF-READ", path);
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int read_payouts(const char *path, struct payout_row *rows, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MAX_LINE_LEN];
    size_t n = 0;
    unsigned long lineno = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        log_msg("E-PSPAYF-OPEN", path);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[6];

        lineno++;
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '#') {
            continue;
        }
        if (n >= cap) {
            log_msg("E-PSPAYF-MAX", "件数上限超過");
            fclose(fp);
            return -1;
        }
        if (split_csv_line(line, f, 6) != 0 ||
            copy_field(rows[n].payout_id, sizeof(rows[n].payout_id), f[0]) != 0 ||
            copy_field(rows[n].merchant_code, sizeof(rows[n].merchant_code), f[1]) != 0 ||
            copy_field(rows[n].bank_acct_no, sizeof(rows[n].bank_acct_no), f[2]) != 0 ||
            parse_money(f[3], &rows[n].payout_amt) != 0 ||
            copy_field(rows[n].payout_dt, sizeof(rows[n].payout_dt), f[4]) != 0 ||
            copy_field(rows[n].bank_result_cd, sizeof(rows[n].bank_result_cd), f[5]) != 0) {
            char buf[64];
            snprintf(buf, sizeof(buf), "%lu", lineno);
            log_msg("E-PSPAYF-PARSE", buf);
            fclose(fp);
            return -1;
        }
        n++;
    }

    if (ferror(fp)) {
        log_msg("E-PSPAYF-READ", path);
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static const struct settle_row *find_settle(const struct settle_row *rows, size_t count,
                                            const char *merchant_code, long long receipt_amt)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(rows[i].merchant_code, merchant_code) == 0 &&
            rows[i].payout_amt == receipt_amt) {
            return &rows[i];
        }
    }

    for (i = 0; i < count; i++) {
        if (strcmp(rows[i].merchant_code, merchant_code) == 0) {
            return &rows[i];
        }
    }

    return NULL;
}

static const struct payout_row *find_payout(const struct payout_row *rows, size_t count,
                                            const char *merchant_code, long long payout_amt)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (strcmp(rows[i].merchant_code, merchant_code) == 0 &&
            rows[i].payout_amt == payout_amt &&
            strcmp(rows[i].bank_result_cd, BANK_OK) == 0) {
            return &rows[i];
        }
    }

    return NULL;
}

/* 手数料額は精算ファイル(PSSETF)の登録値を正とする。入金消込側では
   手数料の丸め方を再現せず、登録手数料が妥当な範囲内(0以上、純額以下)に
   あることのみを確認する。 */
static int validate_charge(const struct settle_row *s)
{
    if (s->charge_amt < 0 || s->charge_amt > s->net_amt) {
        return -1;
    }

    return 0;
}

static int write_receipts(const char *path, const struct receipt_row *rows, size_t count)
{
    FILE *fp;
    size_t i;

    fp = fopen(path, "w");
    if (fp == NULL) {
        log_msg("E-PSRCVF-WOPEN", path);
        return -1;
    }

    for (i = 0; i < count; i++) {
        if (fprintf(fp, "%s,%s,%lld,%s,%s,%s\n",
                    rows[i].receipt_id,
                    rows[i].merchant_code,
                    rows[i].receipt_amt,
                    rows[i].receipt_dt,
                    rows[i].match_status,
                    rows[i].settle_id) < 0) {
            log_msg("E-PSRCVF-WRITE", path);
            fclose(fp);
            return -1;
        }
    }

    if (fclose(fp) != 0) {
        log_msg("E-PSRCVF-CLOSE", path);
        return -1;
    }

    return 0;
}

static int write_diff(FILE *fp, const struct receipt_row *r, const struct settle_row *s,
                      long long diff_amt)
{
    if (fprintf(fp, "%s,%s,%s,%lld,%lld,%lld\n",
                r->receipt_id,
                r->merchant_code,
                s->settle_id,
                r->receipt_amt,
                s->payout_amt,
                diff_amt) < 0) {
        return -1;
    }

    return 0;
}

int main(void)
{
    static struct receipt_row receipts[MAX_RECEIPTS];
    static struct settle_row settles[MAX_SETTLES];
    static struct payout_row payouts[MAX_PAYOUTS];
    size_t receipt_count = 0;
    size_t settle_count = 0;
    size_t payout_count = 0;
    size_t i;
    FILE *diff_fp;
    int rc = 0;

    if (read_receipts("PSRCVF.csv", receipts, MAX_RECEIPTS, &receipt_count) != 0 ||
        read_settles("PSSETF.csv", settles, MAX_SETTLES, &settle_count) != 0 ||
        read_payouts("PSPAYF.csv", payouts, MAX_PAYOUTS, &payout_count) != 0) {
        return 12;
    }

    diff_fp = fopen("RCVDIFF.csv", "w");
    if (diff_fp == NULL) {
        log_msg("E-RCVDIFF-OPEN", "RCVDIFF.csv");
        return 16;
    }

    for (i = 0; i < receipt_count; i++) {
        const struct settle_row *s;
        const struct payout_row *p;
        long long diff_amt;

        s = find_settle(settles, settle_count, receipts[i].merchant_code, receipts[i].receipt_amt);
        if (s == NULL) {
            if (copy_field(receipts[i].match_status, sizeof(receipts[i].match_status), MATCH_MISS) != 0 ||
                copy_field(receipts[i].settle_id, sizeof(receipts[i].settle_id), "") != 0) {
                rc = 20;
                break;
            }
            continue;
        }

        if (copy_field(receipts[i].settle_id, sizeof(receipts[i].settle_id), s->settle_id) != 0) {
            rc = 20;
            break;
        }

        p = find_payout(payouts, payout_count, s->merchant_code, s->payout_amt);
        if (p == NULL) {
            if (copy_field(receipts[i].match_status, sizeof(receipts[i].match_status), MATCH_MISS) != 0) {
                rc = 20;
                break;
            }
            continue;
        }

        if (validate_charge(s) != 0 || s->net_amt < s->charge_amt ||
            s->net_amt - s->charge_amt != s->payout_amt) {
            if (copy_field(receipts[i].match_status, sizeof(receipts[i].match_status), MATCH_ERR) != 0) {
                rc = 20;
                break;
            }
            continue;
        }

        diff_amt = receipts[i].receipt_amt - s->payout_amt;
        if (diff_amt == 0 && receipts[i].receipt_amt == p->payout_amt) {
            if (copy_field(receipts[i].match_status, sizeof(receipts[i].match_status), MATCH_OK) != 0) {
                rc = 20;
                break;
            }
        } else {
            if (copy_field(receipts[i].match_status, sizeof(receipts[i].match_status), MATCH_DIFF) != 0 ||
                write_diff(diff_fp, &receipts[i], s, diff_amt) != 0) {
                rc = 20;
                break;
            }
        }
    }

    if (fclose(diff_fp) != 0 && rc == 0) {
        log_msg("E-RCVDIFF-CLOSE", "RCVDIFF.csv");
        rc = 16;
    }

    if (rc != 0) {
        log_msg("E-BATCH-ABEND", "入金消込中断");
        return rc;
    }

    if (write_receipts("PSRCVF.out", receipts, receipt_count) != 0) {
        return 16;
    }

    return 0;
}
