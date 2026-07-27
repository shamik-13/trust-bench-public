/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    20240311    みらいペイ システム部    初版作成
 * 1.01    20240722    みらいペイ システム部    REQ-ID照合とHOLD-ID照合の事前判定を追加
 * 1.02    20241014    みらいペイ システム部    金額差異と締め時刻超過の保留出力を追加
 */

#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define AR_DECISION_APPROVE 'A'
#define AR_DECISION_DECLINE 'D'

#define TXN_STATUS_APPROVED "A"
#define HOLD_RESULT_ACTIVE "00"
#define HOLD_RESULT_CANCEL "20"
#define HOLD_RESULT_CAPTURED "30"
#define MERCHANT_STATUS_ACTIVE "01"
#define BASE_CURRENCY "JPY"

#define PEND_STATUS_UNSETTLED "10"
#define PEND_STATUS_SETTLED "30"

#define MAX_LINE_LEN 1024
#define MAX_FIELD_LEN 64
#define MAX_TXN 20000
#define MAX_HOLD 20000
#define MAX_MERCHANT 4096
#define CAPTURE_CLOSE_HHMMSS 153000
#define AMT_DIFF_TOLERANCE 0

typedef struct {
    char txn_id[MAX_FIELD_LEN];
    char req_id[MAX_FIELD_LEN];
    char wallet_id[MAX_FIELD_LEN];
    char merchant_code[MAX_FIELD_LEN];
    int64_t req_amt;
    char txn_status[MAX_FIELD_LEN];
    int auth_dt;
    int capture_dt;
} TxnRec;

typedef struct {
    char hold_id[MAX_FIELD_LEN];
    char wallet_id[MAX_FIELD_LEN];
    int64_t hold_amt;
    char hold_result[MAX_FIELD_LEN];
    char merchant_code[MAX_FIELD_LEN];
    char currency_cd[MAX_FIELD_LEN];
    int hold_exp_dt;
} HoldRec;

typedef struct {
    char merchant_code[MAX_FIELD_LEN];
    char merchant_status[MAX_FIELD_LEN];
    char mcc[MAX_FIELD_LEN];
    int64_t daily_limit_amt;
    char risk_rank[MAX_FIELD_LEN];
    char settle_cycle_kbn[MAX_FIELD_LEN];
} MerchantRec;

typedef struct {
    TxnRec rows[MAX_TXN];
    size_t count;
} TxnTable;

typedef struct {
    HoldRec rows[MAX_HOLD];
    size_t count;
} HoldTable;

typedef struct {
    MerchantRec rows[MAX_MERCHANT];
    size_t count;
} MerchantTable;

static void trim_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static bool copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);
    if (n == 0 || n >= dstsz) {
        return false;
    }
    memcpy(dst, src, n + 1);
    return true;
}

static int split_csv_simple(char *line, char *field[], int max_field)
{
    int n = 0;
    char *p = line;

    while (n < max_field) {
        field[n++] = p;
        while (*p != '\0' && *p != ',') {
            p++;
        }
        if (*p == '\0') {
            break;
        }
        *p++ = '\0';
    }

    return n;
}

static bool parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s[0] == '\0') {
        return false;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || *end != '\0' || v < 0) {
        return false;
    }

    *out = (int64_t)v;
    return true;
}

static bool parse_yyyymmdd(const char *s, int *out)
{
    char *end = NULL;
    long v;

    if (strlen(s) != 8) {
        return false;
    }

    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || *end != '\0' || v < 19000101L || v > 20991231L) {
        return false;
    }

    *out = (int)v;
    return true;
}

static int today_yyyymmdd(void)
{
    time_t now = time(NULL);
    struct tm lt;
#if defined(_WIN32)
    localtime_s(&lt, &now);
#else
    localtime_r(&now, &lt);
#endif
    return (lt.tm_year + 1900) * 10000 + (lt.tm_mon + 1) * 100 + lt.tm_mday;
}

static int now_hhmmss(void)
{
    time_t now = time(NULL);
    struct tm lt;
#if defined(_WIN32)
    localtime_s(&lt, &now);
#else
    localtime_r(&now, &lt);
#endif
    return lt.tm_hour * 10000 + lt.tm_min * 100 + lt.tm_sec;
}

static bool read_txn_file(const char *path, TxnTable *table)
{
    FILE *fp = fopen(path, "r");
    char line[MAX_LINE_LEN];

    if (fp == NULL) {
        fprintf(stderr, "PYTXNFオープン失敗:%s\n", path);
        return false;
    }

    table->count = 0;
    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[8];
        TxnRec rec;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (table->count >= MAX_TXN) {
            fprintf(stderr, "PYTXNF件数超過\n");
            fclose(fp);
            return false;
        }
        if (split_csv_simple(line, f, 8) != 8) {
            fprintf(stderr, "PYTXNF項目数不正\n");
            fclose(fp);
            return false;
        }

        memset(&rec, 0, sizeof rec);
        if (!copy_field(rec.txn_id, sizeof rec.txn_id, f[0]) ||
            !copy_field(rec.req_id, sizeof rec.req_id, f[1]) ||
            !copy_field(rec.wallet_id, sizeof rec.wallet_id, f[2]) ||
            !copy_field(rec.merchant_code, sizeof rec.merchant_code, f[3]) ||
            !parse_i64(f[4], &rec.req_amt) ||
            !copy_field(rec.txn_status, sizeof rec.txn_status, f[5]) ||
            !parse_yyyymmdd(f[6], &rec.auth_dt) ||
            !parse_yyyymmdd(f[7], &rec.capture_dt)) {
            fprintf(stderr, "PYTXNF形式不正\n");
            fclose(fp);
            return false;
        }

        table->rows[table->count++] = rec;
    }

    if (ferror(fp)) {
        fprintf(stderr, "PYTXNF読込失敗\n");
        fclose(fp);
        return false;
    }

    fclose(fp);
    return true;
}

static bool read_hold_file(const char *path, HoldTable *table)
{
    FILE *fp = fopen(path, "r");
    char line[MAX_LINE_LEN];

    if (fp == NULL) {
        fprintf(stderr, "PYHOLDFオープン失敗:%s\n", path);
        return false;
    }

    table->count = 0;
    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[7];
        HoldRec rec;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (table->count >= MAX_HOLD) {
            fprintf(stderr, "PYHOLDF件数超過\n");
            fclose(fp);
            return false;
        }
        if (split_csv_simple(line, f, 7) != 7) {
            fprintf(stderr, "PYHOLDF項目数不正\n");
            fclose(fp);
            return false;
        }

        memset(&rec, 0, sizeof rec);
        if (!copy_field(rec.hold_id, sizeof rec.hold_id, f[0]) ||
            !copy_field(rec.wallet_id, sizeof rec.wallet_id, f[1]) ||
            !parse_i64(f[2], &rec.hold_amt) ||
            !copy_field(rec.hold_result, sizeof rec.hold_result, f[3]) ||
            !copy_field(rec.merchant_code, sizeof rec.merchant_code, f[4]) ||
            !copy_field(rec.currency_cd, sizeof rec.currency_cd, f[5]) ||
            !parse_yyyymmdd(f[6], &rec.hold_exp_dt)) {
            fprintf(stderr, "PYHOLDF形式不正\n");
            fclose(fp);
            return false;
        }

        table->rows[table->count++] = rec;
    }

    if (ferror(fp)) {
        fprintf(stderr, "PYHOLDF読込失敗\n");
        fclose(fp);
        return false;
    }

    fclose(fp);
    return true;
}

static bool read_merchant_file(const char *path, MerchantTable *table)
{
    FILE *fp = fopen(path, "r");
    char line[MAX_LINE_LEN];

    if (fp == NULL) {
        fprintf(stderr, "PYMERFオープン失敗:%s\n", path);
        return false;
    }

    table->count = 0;
    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[6];
        MerchantRec rec;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (table->count >= MAX_MERCHANT) {
            fprintf(stderr, "PYMERF件数超過\n");
            fclose(fp);
            return false;
        }
        if (split_csv_simple(line, f, 6) != 6) {
            fprintf(stderr, "PYMERF項目数不正\n");
            fclose(fp);
            return false;
        }

        memset(&rec, 0, sizeof rec);
        if (!copy_field(rec.merchant_code, sizeof rec.merchant_code, f[0]) ||
            !copy_field(rec.merchant_status, sizeof rec.merchant_status, f[1]) ||
            !copy_field(rec.mcc, sizeof rec.mcc, f[2]) ||
            !parse_i64(f[3], &rec.daily_limit_amt) ||
            !copy_field(rec.risk_rank, sizeof rec.risk_rank, f[4]) ||
            !copy_field(rec.settle_cycle_kbn, sizeof rec.settle_cycle_kbn, f[5])) {
            fprintf(stderr, "PYMERF形式不正\n");
            fclose(fp);
            return false;
        }

        table->rows[table->count++] = rec;
    }

    if (ferror(fp)) {
        fprintf(stderr, "PYMERF読込失敗\n");
        fclose(fp);
        return false;
    }

    fclose(fp);
    return true;
}

static const HoldRec *find_hold_by_req(const HoldTable *table, const char *req_id)
{
    size_t i;

    for (i = 0; i < table->count; i++) {
        if (strcmp(table->rows[i].hold_id, req_id) == 0) {
            return &table->rows[i];
        }
    }

    return NULL;
}

static const MerchantRec *find_merchant(const MerchantTable *table, const char *merchant_code)
{
    size_t i;

    for (i = 0; i < table->count; i++) {
        if (strcmp(table->rows[i].merchant_code, merchant_code) == 0) {
            return &table->rows[i];
        }
    }

    return NULL;
}

static bool write_pending(FILE *fp, const TxnRec *txn, const char *reason)
{
    int n = fprintf(fp, "PND-%s,%s,%lld,%s,%08d,%s\n",
                    txn->req_id,
                    txn->wallet_id,
                    (long long)txn->req_amt,
                    PEND_STATUS_UNSETTLED,
                    txn->capture_dt,
                    reason);
    return n > 0;
}

static bool write_handoff(FILE *fp, const TxnRec *txn, const HoldRec *hold, const MerchantRec *merchant)
{
    int n = fprintf(fp, "%s,%s,%s,%s,%lld,%08d,%s,%s\n",
                    txn->txn_id,
                    txn->req_id,
                    hold->hold_id,
                    txn->merchant_code,
                    (long long)txn->req_amt,
                    txn->capture_dt,
                    merchant->mcc,
                    merchant->settle_cycle_kbn);
    return n > 0;
}

int main(void)
{
    TxnTable txns;
    HoldTable holds;
    MerchantTable merchants;
    FILE *pend_fp;
    FILE *ok_fp;
    size_t i;
    int today = today_yyyymmdd();
    int hhmmss = now_hhmmss();
    int approved = 0;
    int declined = 0;

    if (!read_txn_file("PYTXNF.csv", &txns) ||
        !read_hold_file("PYHOLDF.csv", &holds) ||
        !read_merchant_file("PYMERF.csv", &merchants)) {
        return 8;
    }

    pend_fp = fopen("PYPENDF.csv", "w");
    if (pend_fp == NULL) {
        fprintf(stderr, "PYPENDFオープン失敗\n");
        return 8;
    }

    ok_fp = fopen("CAPTURE_JAVA_IF.csv", "w");
    if (ok_fp == NULL) {
        fprintf(stderr, "売上確定連携ファイルオープン失敗\n");
        fclose(pend_fp);
        return 8;
    }

    for (i = 0; i < txns.count; i++) {
        const TxnRec *txn = &txns.rows[i];
        const HoldRec *hold = find_hold_by_req(&holds, txn->req_id);
        const MerchantRec *merchant = find_merchant(&merchants, txn->merchant_code);
        int64_t diff;

        if (strcmp(txn->txn_status, TXN_STATUS_APPROVED) != 0) {
            if (!write_pending(pend_fp, txn, "REQ-NG")) {
                fprintf(stderr, "PYPENDF書込失敗\n");
                fclose(ok_fp);
                fclose(pend_fp);
                return 8;
            }
            declined++;
            continue;
        }

        if (hold == NULL) {
            if (!write_pending(pend_fp, txn, "HOLD-NONE")) {
                fprintf(stderr, "PYPENDF書込失敗\n");
                fclose(ok_fp);
                fclose(pend_fp);
                return 8;
            }
            declined++;
            continue;
        }

        if (merchant == NULL || strcmp(merchant->merchant_status, MERCHANT_STATUS_ACTIVE) != 0) {
            if (!write_pending(pend_fp, txn, "MER-NG")) {
                fprintf(stderr, "PYPENDF書込失敗\n");
                fclose(ok_fp);
                fclose(pend_fp);
                return 8;
            }
            declined++;
            continue;
        }

        if (strcmp(hold->wallet_id, txn->wallet_id) != 0 ||
            strcmp(hold->merchant_code, txn->merchant_code) != 0) {
            if (!write_pending(pend_fp, txn, "HOLD-MISMATCH")) {
                fprintf(stderr, "PYPENDF書込失敗\n");
                fclose(ok_fp);
                fclose(pend_fp);
                return 8;
            }
            declined++;
            continue;
        }

        if (strcmp(hold->hold_result, HOLD_RESULT_ACTIVE) != 0) {
            if (!write_pending(pend_fp, txn, "HOLD-STATUS")) {
                fprintf(stderr, "PYPENDF書込失敗\n");
                fclose(ok_fp);
                fclose(pend_fp);
                return 8;
            }
            declined++;
            continue;
        }

        if (strcmp(hold->currency_cd, BASE_CURRENCY) != 0 || hold->hold_exp_dt < today) {
            if (!write_pending(pend_fp, txn, "HOLD-EXPIRE")) {
                fprintf(stderr, "PYPENDF書込失敗\n");
                fclose(ok_fp);
                fclose(pend_fp);
                return 8;
            }
            declined++;
            continue;
        }

        diff = hold->hold_amt >= txn->req_amt ? hold->hold_amt - txn->req_amt : txn->req_amt - hold->hold_amt;
        if (diff > AMT_DIFF_TOLERANCE || txn->req_amt > merchant->daily_limit_amt) {
            if (!write_pending(pend_fp, txn, "AMT-DIFF")) {
                fprintf(stderr, "PYPENDF書込失敗\n");
                fclose(ok_fp);
                fclose(pend_fp);
                return 8;
            }
            declined++;
            continue;
        }

        if (hhmmss > CAPTURE_CLOSE_HHMMSS) {
            if (!write_pending(pend_fp, txn, "CLOSE-OVER")) {
                fprintf(stderr, "PYPENDF書込失敗\n");
                fclose(ok_fp);
                fclose(pend_fp);
                return 8;
            }
            declined++;
            continue;
        }

        if (!write_handoff(ok_fp, txn, hold, merchant)) {
            fprintf(stderr, "売上確定連携ファイル書込失敗\n");
            fclose(ok_fp);
            fclose(pend_fp);
            return 8;
        }
        approved++;
    }

    if (fclose(ok_fp) != 0) {
        fprintf(stderr, "売上確定連携ファイルクローズ失敗\n");
        fclose(pend_fp);
        return 8;
    }
    if (fclose(pend_fp) != 0) {
        fprintf(stderr, "PYPENDFクローズ失敗\n");
        return 8;
    }

    fprintf(stderr, "処理完了 承認=%d 否認=%d\n", approved, declined);
    return declined == 0 ? AR_DECISION_APPROVE : AR_DECISION_DECLINE;
}
