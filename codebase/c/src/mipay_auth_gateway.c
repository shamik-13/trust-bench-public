/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240408  宮田      初版作成、固定長決済電文の検査処理を追加
 * 1.01  20240916  宮田      通貨レートキャッシュ照合と加盟店限度額検査を追加
 * 1.02  20250303  宮田      桁あふれ検出と中間取引レコード出力を強化
 */

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MI_RET_OK 0
#define MI_RET_PARSE_ERR 12
#define MI_RET_IO_ERR 16
#define MI_RET_REJECT 20

#define MI_MAX_LINE 512
#define MI_MAX_QR 256
#define MI_MAX_MER 256
#define MI_QR_ID_LEN 32
#define MI_WALLET_ID_LEN 32
#define MI_MERCHANT_CODE_LEN 16
#define MI_STATUS_LEN 8
#define MI_MCC_LEN 8
#define MI_RISK_LEN 8
#define MI_CYCLE_LEN 8
#define MI_TXN_ID_LEN 40
#define MI_REQ_ID_LEN 40
#define MI_TS_LEN 20
#define MI_CCY_LEN 4

#define MI_REQ_AMT_MAX 999999999999LL
#define MI_DAILY_LIMIT_MAX 999999999999LL

typedef struct {
    char qr_id[MI_QR_ID_LEN];
    char wallet_id[MI_WALLET_ID_LEN];
    char merchant_code[MI_MERCHANT_CODE_LEN];
    long long req_amt;
    char qr_status[MI_STATUS_LEN];
    char expire_ts[MI_TS_LEN];
    char ccy[MI_CCY_LEN];
} MiQrRecord;

typedef struct {
    char merchant_code[MI_MERCHANT_CODE_LEN];
    char merchant_status[MI_STATUS_LEN];
    char mcc[MI_MCC_LEN];
    long long daily_limit_amt;
    char risk_rank[MI_RISK_LEN];
    char settle_cycle_kbn[MI_CYCLE_LEN];
} MiMerchantRecord;

typedef struct {
    char txn_id[MI_TXN_ID_LEN];
    char req_id[MI_REQ_ID_LEN];
    char wallet_id[MI_WALLET_ID_LEN];
    char merchant_code[MI_MERCHANT_CODE_LEN];
    long long req_amt;
    char txn_status[MI_STATUS_LEN];
    char auth_dt[MI_TS_LEN];
    char capture_dt[MI_TS_LEN];
} MiTxnRecord;

typedef struct {
    char ccy[MI_CCY_LEN];
    uint32_t scaled_rate;
    int permitted;
} MiRateCache;

static const MiRateCache g_rate_cache[] = {
    {"JPY", 1000000U, 1},
    {"USD", 157420000U, 1},
    {"EUR", 168250000U, 1},
    {"CNH", 21680000U, 0}
};

static void mi_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int mi_copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t n = strlen(src);
    if (n == 0U || n >= dst_len) {
        return 0;
    }
    memcpy(dst, src, n + 1U);
    return 1;
}

static int mi_parse_amount(const char *s, long long max_value, long long *out)
{
    long long v = 0;
    const unsigned char *p = (const unsigned char *)s;

    if (*p == '\0' || *p == '+' || *p == '-') {
        return 0;
    }
    while (*p != '\0') {
        int d;
        if (*p < (unsigned char)'0' || *p > (unsigned char)'9') {
            return 0;
        }
        d = (int)(*p - (unsigned char)'0');
        if (v > (max_value - d) / 10LL) {
            return 0;
        }
        v = v * 10LL + (long long)d;
        ++p;
    }
    *out = v;
    return v > 0;
}

static int mi_split_csv(char *line, char **fields, size_t expect)
{
    size_t i = 0U;
    char *p = line;

    while (i < expect) {
        fields[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return i == expect && strchr(fields[expect - 1U], ',') == NULL;
}

static int mi_parse_qr(char *line, MiQrRecord *rec)
{
    char *f[7];
    if (!mi_split_csv(line, f, 7U)) {
        return 0;
    }
    return mi_copy_field(rec->qr_id, sizeof rec->qr_id, f[0]) &&
           mi_copy_field(rec->wallet_id, sizeof rec->wallet_id, f[1]) &&
           mi_copy_field(rec->merchant_code, sizeof rec->merchant_code, f[2]) &&
           mi_parse_amount(f[3], MI_REQ_AMT_MAX, &rec->req_amt) &&
           mi_copy_field(rec->qr_status, sizeof rec->qr_status, f[4]) &&
           mi_copy_field(rec->expire_ts, sizeof rec->expire_ts, f[5]) &&
           mi_copy_field(rec->ccy, sizeof rec->ccy, f[6]);
}

static int mi_parse_merchant(char *line, MiMerchantRecord *rec)
{
    char *f[6];
    if (!mi_split_csv(line, f, 6U)) {
        return 0;
    }
    return mi_copy_field(rec->merchant_code, sizeof rec->merchant_code, f[0]) &&
           mi_copy_field(rec->merchant_status, sizeof rec->merchant_status, f[1]) &&
           mi_copy_field(rec->mcc, sizeof rec->mcc, f[2]) &&
           mi_parse_amount(f[3], MI_DAILY_LIMIT_MAX, &rec->daily_limit_amt) &&
           mi_copy_field(rec->risk_rank, sizeof rec->risk_rank, f[4]) &&
           mi_copy_field(rec->settle_cycle_kbn, sizeof rec->settle_cycle_kbn, f[5]);
}

static int mi_load_qr(const char *path, MiQrRecord *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[MI_MAX_LINE];
    size_t n = 0U;

    if (fp == NULL) {
        fprintf(stderr, "E001:ＱＲ入力を開けません:%s\n", path);
        return MI_RET_IO_ERR;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        mi_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (n >= cap || !mi_parse_qr(line, &rows[n])) {
            fclose(fp);
            fprintf(stderr, "E002:ＱＲ入力の形式が不正です\n");
            return MI_RET_PARSE_ERR;
        }
        ++n;
    }
    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "E003:ＱＲ入力の読込に失敗しました\n");
        return MI_RET_IO_ERR;
    }
    fclose(fp);
    *count = n;
    return MI_RET_OK;
}

static int mi_load_merchant(const char *path, MiMerchantRecord *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[MI_MAX_LINE];
    size_t n = 0U;

    if (fp == NULL) {
        fprintf(stderr, "E004:加盟店入力を開けません:%s\n", path);
        return MI_RET_IO_ERR;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        mi_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (n >= cap || !mi_parse_merchant(line, &rows[n])) {
            fclose(fp);
            fprintf(stderr, "E005:加盟店入力の形式が不正です\n");
            return MI_RET_PARSE_ERR;
        }
        ++n;
    }
    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "E006:加盟店入力の読込に失敗しました\n");
        return MI_RET_IO_ERR;
    }
    fclose(fp);
    *count = n;
    return MI_RET_OK;
}

static const MiMerchantRecord *mi_find_merchant(const MiMerchantRecord *rows, size_t count, const char *code)
{
    size_t i;
    for (i = 0U; i < count; ++i) {
        if (strcmp(rows[i].merchant_code, code) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static int mi_ccy_permitted(const char *ccy)
{
    size_t i;
    for (i = 0U; i < sizeof g_rate_cache / sizeof g_rate_cache[0]; ++i) {
        if (strcmp(g_rate_cache[i].ccy, ccy) == 0) {
            return g_rate_cache[i].permitted && g_rate_cache[i].scaled_rate > 0U;
        }
    }
    return 0;
}

static int mi_expired_yyyymmddhhmmss(const char *ts)
{
    char nowbuf[MI_TS_LEN];
    time_t now = time(NULL);
    struct tm *lt = localtime(&now);
    size_t i;

    if (lt == NULL || strlen(ts) != 14U) {
        return 1;
    }
    for (i = 0U; i < 14U; ++i) {
        if (ts[i] < '0' || ts[i] > '9') {
            return 1;
        }
    }
    if (strftime(nowbuf, sizeof nowbuf, "%Y%m%d%H%M%S", lt) == 0U) {
        return 1;
    }
    return strcmp(ts, nowbuf) < 0;
}

static void mi_now_yyyymmddhhmmss(char *dst, size_t len)
{
    time_t now = time(NULL);
    struct tm *lt = localtime(&now);
    if (lt == NULL || strftime(dst, len, "%Y%m%d%H%M%S", lt) == 0U) {
        memcpy(dst, "19700101000000", 15U);
    }
}

static int mi_valid_identifier(const char *s, size_t max_len)
{
    size_t i;
    size_t n = strlen(s);

    if (n == 0U || n >= max_len) {
        return 0;
    }
    for (i = 0U; i < n; ++i) {
        unsigned char c = (unsigned char)s[i];
        if (!((c >= (unsigned char)'0' && c <= (unsigned char)'9') ||
              (c >= (unsigned char)'A' && c <= (unsigned char)'Z') ||
              c == (unsigned char)'-' || c == (unsigned char)'_')) {
            return 0;
        }
    }
    return 1;
}

static void mi_make_txn(const MiQrRecord *qr, MiTxnRecord *txn)
{
    char nowbuf[MI_TS_LEN];

    mi_now_yyyymmddhhmmss(nowbuf, sizeof nowbuf);
    snprintf(txn->txn_id, sizeof txn->txn_id, "TX%s%s", nowbuf, qr->qr_id);
    snprintf(txn->req_id, sizeof txn->req_id, "RQ%s", qr->qr_id);
    snprintf(txn->wallet_id, sizeof txn->wallet_id, "%s", qr->wallet_id);
    snprintf(txn->merchant_code, sizeof txn->merchant_code, "%s", qr->merchant_code);
    txn->req_amt = qr->req_amt;
    snprintf(txn->txn_status, sizeof txn->txn_status, "%s", "AUTH");
    snprintf(txn->auth_dt, sizeof txn->auth_dt, "%s", nowbuf);
    snprintf(txn->capture_dt, sizeof txn->capture_dt, "%s", "00000000000000");
}

static int mi_write_txn(FILE *fp, const MiTxnRecord *txn)
{
    if (fprintf(fp, "%s,%s,%s,%s,%lld,%s,%s,%s\n",
                txn->txn_id,
                txn->req_id,
                txn->wallet_id,
                txn->merchant_code,
                txn->req_amt,
                txn->txn_status,
                txn->auth_dt,
                txn->capture_dt) < 0) {
        fprintf(stderr, "E007:取引出力に失敗しました\n");
        return MI_RET_IO_ERR;
    }
    return MI_RET_OK;
}

int main(void)
{
    MiQrRecord qr_rows[MI_MAX_QR];
    MiMerchantRecord merchant_rows[MI_MAX_MER];
    size_t qr_count = 0U;
    size_t merchant_count = 0U;
    size_t i;
    int rc;
    int reject_seen = 0;
    FILE *out;

    rc = mi_load_qr("PYQRCF.csv", qr_rows, MI_MAX_QR, &qr_count);
    if (rc != MI_RET_OK) {
        return rc;
    }
    rc = mi_load_merchant("PYMERF.csv", merchant_rows, MI_MAX_MER, &merchant_count);
    if (rc != MI_RET_OK) {
        return rc;
    }

    out = fopen("PYTXNF.csv", "w");
    if (out == NULL) {
        fprintf(stderr, "E008:取引出力を開けません:PYTXNF.csv\n");
        return MI_RET_IO_ERR;
    }

    for (i = 0U; i < qr_count; ++i) {
        const MiQrRecord *qr = &qr_rows[i];
        const MiMerchantRecord *mer = mi_find_merchant(merchant_rows, merchant_count, qr->merchant_code);
        MiTxnRecord txn;

        if (!mi_valid_identifier(qr->qr_id, sizeof qr->qr_id) ||
            !mi_valid_identifier(qr->wallet_id, sizeof qr->wallet_id) ||
            !mi_valid_identifier(qr->merchant_code, sizeof qr->merchant_code)) {
            fprintf(stderr, "R101:識別子不正:%s\n", qr->qr_id);
            reject_seen = 1;
            continue;
        }
        if (strcmp(qr->qr_status, "ACTIVE") != 0) {
            fprintf(stderr, "R102:ＱＲ状態不許可:%s\n", qr->qr_id);
            reject_seen = 1;
            continue;
        }
        if (mi_expired_yyyymmddhhmmss(qr->expire_ts)) {
            fprintf(stderr, "R103:ＱＲ期限切れ:%s\n", qr->qr_id);
            reject_seen = 1;
            continue;
        }
        if (mer == NULL) {
            fprintf(stderr, "R104:加盟店未登録:%s\n", qr->merchant_code);
            reject_seen = 1;
            continue;
        }
        if (strcmp(mer->merchant_status, "OPEN") != 0) {
            fprintf(stderr, "R105:加盟店状態不許可:%s\n", qr->merchant_code);
            reject_seen = 1;
            continue;
        }
        if (qr->req_amt > mer->daily_limit_amt) {
            fprintf(stderr, "R106:加盟店日次限度超過:%s\n", qr->merchant_code);
            reject_seen = 1;
            continue;
        }
        if (strcmp(mer->risk_rank, "D") == 0 || strcmp(mer->risk_rank, "E") == 0) {
            fprintf(stderr, "R107:リスクランク拒否:%s\n", qr->merchant_code);
            reject_seen = 1;
            continue;
        }
        if (!mi_ccy_permitted(qr->ccy)) {
            fprintf(stderr, "R108:通貨コード不許可:%s\n", qr->ccy);
            reject_seen = 1;
            continue;
        }

        mi_make_txn(qr, &txn);
        rc = mi_write_txn(out, &txn);
        if (rc != MI_RET_OK) {
            fclose(out);
            return rc;
        }
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "E009:取引出力のクローズに失敗しました\n");
        return MI_RET_IO_ERR;
    }

    return reject_seen ? MI_RET_REJECT : MI_RET_OK;
}
