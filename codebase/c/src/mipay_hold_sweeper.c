/************************************************************
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240712  みらいペイ システム部  ホールド期限走査バッチ初版
 ************************************************************/

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define 入力ホールドファイル "PYHOLDF.csv"
#define 入力取引ファイル     "PYTXNF.csv"
#define 出力通知ファイル     "PYNTFF.csv"

#define 最大行長             1024
#define 最大項目数           16
#define 最大文字列           128
#define 最大取引件数         200000
#define 最大通知本文         256

#define 正常終了             0
#define 入出力異常           12
#define 形式異常             16
#define 容量異常             20

#define ホールド有効         "00"
#define ホールド取消         "20"
#define ホールド確定済       "30"
#define 未確定               "10"
#define 清算済               "30"
#define 承認                 'A'
#define 否認                 'D'

typedef struct {
    char txn_id[最大文字列];
    char req_id[最大文字列];
    char wallet_id[最大文字列];
    char merchant_code[最大文字列];
    int64_t req_amt;
    char txn_status[最大文字列];
    char auth_dt[最大文字列];
    char capture_dt[最大文字列];
} 取引明細;

typedef struct {
    char hold_id[最大文字列];
    char wallet_id[最大文字列];
    int64_t hold_amt;
    char hold_result[最大文字列];
    char merchant_code[最大文字列];
    char currency_cd[最大文字列];
    int hold_exp_dt;
} ホールド明細;

typedef struct {
    取引明細 *data;
    size_t count;
    size_t capacity;
} 取引表;

static void 改行除去(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int 文字列設定(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);
    if (n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int csv分割(char *line, char *cols[], size_t maxcols)
{
    size_t cnt = 0;
    char *p = line;

    while (*p != '\0') {
        if (cnt >= maxcols) {
            return -1;
        }

        if (*p == '"') {
            char *out = p;
            char *start = ++p;
            cols[cnt++] = out;

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
                ++p;
            } else if (*p != '\0') {
                return -1;
            }

            (void)start;
        } else {
            cols[cnt++] = p;
            while (*p != '\0' && *p != ',') {
                ++p;
            }
            if (*p == ',') {
                *p++ = '\0';
            }
        }
    }

    if (line[0] != '\0' && line[strlen(line) - 1] == ',') {
        if (cnt >= maxcols) {
            return -1;
        }
        cols[cnt++] = "";
    }

    return (int)cnt;
}

static int 金額読取(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (*s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || *end != '\0' || v < 0) {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int 日付読取(const char *s, int *out)
{
    char *end = NULL;
    long v;

    if (strlen(s) != 8) {
        return -1;
    }

    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || *end != '\0' || v < 19000101L || v > 29991231L) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int 今日yyyymmdd(void)
{
    time_t now = time(NULL);
    struct tm tmv;
#if defined(_WIN32)
    localtime_s(&tmv, &now);
#else
    localtime_r(&now, &tmv);
#endif
    return (tmv.tm_year + 1900) * 10000 + (tmv.tm_mon + 1) * 100 + tmv.tm_mday;
}

static int 現在時刻文字列(char *buf, size_t bufsz)
{
    time_t now = time(NULL);
    struct tm tmv;
#if defined(_WIN32)
    localtime_s(&tmv, &now);
#else
    localtime_r(&now, &tmv);
#endif
    return strftime(buf, bufsz, "%Y%m%d%H%M%S", &tmv) == 0 ? -1 : 0;
}

static int 取引追加(取引表 *tab, const 取引明細 *rec)
{
    取引明細 *next;
    size_t nextcap;

    if (tab->count == tab->capacity) {
        nextcap = tab->capacity == 0 ? 4096 : tab->capacity * 2;
        if (nextcap > 最大取引件数 || nextcap < tab->capacity) {
            return -1;
        }
        next = (取引明細 *)realloc(tab->data, nextcap * sizeof(*next));
        if (next == NULL) {
            return -1;
        }
        tab->data = next;
        tab->capacity = nextcap;
    }

    tab->data[tab->count++] = *rec;
    return 0;
}

static int 取引行読取(char *line, 取引明細 *rec)
{
    char *cols[最大項目数];
    int n = csv分割(line, cols, 最大項目数);

    if (n != 8) {
        return -1;
    }

    if (文字列設定(rec->txn_id, sizeof(rec->txn_id), cols[0]) != 0 ||
        文字列設定(rec->req_id, sizeof(rec->req_id), cols[1]) != 0 ||
        文字列設定(rec->wallet_id, sizeof(rec->wallet_id), cols[2]) != 0 ||
        文字列設定(rec->merchant_code, sizeof(rec->merchant_code), cols[3]) != 0 ||
        金額読取(cols[4], &rec->req_amt) != 0 ||
        文字列設定(rec->txn_status, sizeof(rec->txn_status), cols[5]) != 0 ||
        文字列設定(rec->auth_dt, sizeof(rec->auth_dt), cols[6]) != 0 ||
        文字列設定(rec->capture_dt, sizeof(rec->capture_dt), cols[7]) != 0) {
        return -1;
    }

    return 0;
}

static int ホールド行読取(char *line, ホールド明細 *rec)
{
    char *cols[最大項目数];
    int n = csv分割(line, cols, 最大項目数);

    if (n != 7) {
        return -1;
    }

    if (文字列設定(rec->hold_id, sizeof(rec->hold_id), cols[0]) != 0 ||
        文字列設定(rec->wallet_id, sizeof(rec->wallet_id), cols[1]) != 0 ||
        金額読取(cols[2], &rec->hold_amt) != 0 ||
        文字列設定(rec->hold_result, sizeof(rec->hold_result), cols[3]) != 0 ||
        文字列設定(rec->merchant_code, sizeof(rec->merchant_code), cols[4]) != 0 ||
        文字列設定(rec->currency_cd, sizeof(rec->currency_cd), cols[5]) != 0 ||
        日付読取(cols[6], &rec->hold_exp_dt) != 0) {
        return -1;
    }

    return 0;
}

static int 確定取引あり(const 取引表 *tab, const ホールド明細 *hold)
{
    size_t i;

    for (i = 0; i < tab->count; ++i) {
        const 取引明細 *txn = &tab->data[i];

        if (strcmp(txn->wallet_id, hold->wallet_id) == 0 &&
            strcmp(txn->merchant_code, hold->merchant_code) == 0 &&
            txn->req_amt == hold->hold_amt &&
            strcmp(txn->txn_status, 清算済) == 0 &&
            txn->capture_dt[0] != '\0') {
            return 1;
        }
    }

    return 0;
}

static int 取引読込(取引表 *tab)
{
    FILE *fp;
    char line[最大行長];
    unsigned long lineno = 0;

    fp = fopen(入力取引ファイル, "r");
    if (fp == NULL) {
        fprintf(stderr, "取引ファイルを開けません:%s\n", 入力取引ファイル);
        return 入出力異常;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        取引明細 rec;
        ++lineno;
        改行除去(line);

        if (lineno == 1 && strncmp(line, "TXN-ID,", 7) == 0) {
            continue;
        }
        if (line[0] == '\0') {
            continue;
        }
        if (取引行読取(line, &rec) != 0) {
            fprintf(stderr, "取引ファイル形式異常:%lu\n", lineno);
            fclose(fp);
            return 形式異常;
        }
        if (取引追加(tab, &rec) != 0) {
            fprintf(stderr, "取引表容量異常:%lu\n", lineno);
            fclose(fp);
            return 容量異常;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "取引ファイル読込異常\n");
        fclose(fp);
        return 入出力異常;
    }

    fclose(fp);
    return 正常終了;
}

static int 通知出力(FILE *out, unsigned long seq, const ホールド明細 *hold)
{
    char ts[32];
    char text[最大通知本文];

    if (現在時刻文字列(ts, sizeof(ts)) != 0) {
        fprintf(stderr, "処理時刻生成異常\n");
        return 形式異常;
    }

    if (snprintf(text, sizeof(text),
                 "期限切れホールド候補 HOLD=%s WALLET=%s 金額=%lld 店舗=%s",
                 hold->hold_id, hold->wallet_id, (long long)hold->hold_amt,
                 hold->merchant_code) >= (int)sizeof(text)) {
        fprintf(stderr, "通知本文長異常:%s\n", hold->hold_id);
        return 形式異常;
    }

    if (fprintf(out, "NT%010lu,%s,HLD_SWEEP,\"%s\",10,%s\n",
                seq, hold->wallet_id, text, ts) < 0) {
        fprintf(stderr, "通知ファイル書込異常\n");
        return 入出力異常;
    }

    return 正常終了;
}

int main(void)
{
    取引表 txns;
    FILE *holdfp;
    FILE *ntffp;
    char line[最大行長];
    unsigned long lineno = 0;
    unsigned long notice_seq = 0;
    int today = 今日yyyymmdd();
    int rc;

    txns.data = NULL;
    txns.count = 0;
    txns.capacity = 0;

    rc = 取引読込(&txns);
    if (rc != 正常終了) {
        free(txns.data);
        return rc;
    }

    holdfp = fopen(入力ホールドファイル, "r");
    if (holdfp == NULL) {
        fprintf(stderr, "ホールドファイルを開けません:%s\n", 入力ホールドファイル);
        free(txns.data);
        return 入出力異常;
    }

    ntffp = fopen(出力通知ファイル, "w");
    if (ntffp == NULL) {
        fprintf(stderr, "通知ファイルを開けません:%s\n", 出力通知ファイル);
        fclose(holdfp);
        free(txns.data);
        return 入出力異常;
    }

    if (fprintf(ntffp, "NOTICE-ID,WALLET-ID,NOTICE-KBN,NOTICE-TEXT,SEND-STATUS,CREATE-TS\n") < 0) {
        fprintf(stderr, "通知ファイルヘッダ書込異常\n");
        fclose(ntffp);
        fclose(holdfp);
        free(txns.data);
        return 入出力異常;
    }

    while (fgets(line, sizeof(line), holdfp) != NULL) {
        ホールド明細 hold;
        ++lineno;
        改行除去(line);

        if (lineno == 1 && strncmp(line, "HOLD-ID,", 8) == 0) {
            continue;
        }
        if (line[0] == '\0') {
            continue;
        }
        if (ホールド行読取(line, &hold) != 0) {
            fprintf(stderr, "ホールドファイル形式異常:%lu\n", lineno);
            fclose(ntffp);
            fclose(holdfp);
            free(txns.data);
            return 形式異常;
        }

        if (strcmp(hold.currency_cd, "JPY") != 0) {
            continue;
        }
        if (strcmp(hold.hold_result, ホールド有効) != 0) {
            continue;
        }
        if (hold.hold_exp_dt > today) {
            continue;
        }
        if (確定取引あり(&txns, &hold)) {
            continue;
        }

        ++notice_seq;
        rc = 通知出力(ntffp, notice_seq, &hold);
        if (rc != 正常終了) {
            fclose(ntffp);
            fclose(holdfp);
            free(txns.data);
            return rc;
        }
    }

    if (ferror(holdfp)) {
        fprintf(stderr, "ホールドファイル読込異常\n");
        fclose(ntffp);
        fclose(holdfp);
        free(txns.data);
        return 入出力異常;
    }

    if (fclose(ntffp) != 0) {
        fprintf(stderr, "通知ファイルクローズ異常\n");
        fclose(holdfp);
        free(txns.data);
        return 入出力異常;
    }

    fclose(holdfp);
    free(txns.data);

    return 正常終了;
}
