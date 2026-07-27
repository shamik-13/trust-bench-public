/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20250603  精算基盤    初版作成
 * 1.01  20250918  精算基盤    明細突合と加盟店状態判定を追加
 * 1.02  20251104  精算基盤    金額読取の桁あふれ検知を追加
 */

#include "mipay_settle.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define 入力精算ファイル "PSSETF.csv"
#define 入力明細ファイル "PSDTLF.csv"
#define 入力加盟店ファイル "PSMERF.csv"
#define 出力帳票ファイル "PSRPTF"
#define 行最大長 1024
#define 項目最大数 16
#define 加盟店最大件数 4096
#define 精算最大件数 16384
#define 明細集計最大件数 16384
#define 帳票経路最大長 128
#define 正常終了コード 0
#define 異常終了コード 12
#define 手数料率ＢＰ 30L

typedef struct {
    char code[32];
    char name[96];
    char status[4];
    char bank[40];
} 加盟店;

typedef struct {
    char settle_id[32];
    char merchant_code[32];
    long net_amt;
    long charge_amt;
    long payout_amt;
    char settle_dt[16];
} 精算;

typedef struct {
    char settle_id[32];
    char merchant_code[32];
    long capture_amt;
    long refund_amt;
    long charge_amt;
    long line_count;
} 明細集計;

static void 改行除去(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int 文字列複写(char *dst, size_t dstsz, const char *src)
{
    size_t n;

    if (dstsz == 0U) {
        return -1;
    }

    n = strlen(src);
    if (n >= dstsz) {
        return -1;
    }

    memcpy(dst, src, n + 1U);
    return 0;
}

static int csv分割(char *line, char *out[], int max_fields)
{
    int count = 0;
    char *p = line;

    while (*p != '\0' && count < max_fields) {
        out[count++] = p;

        while (*p != '\0' && *p != ',') {
            p++;
        }

        if (*p == ',') {
            *p = '\0';
            p++;
        }
    }

    return count;
}

static int 金額読取(const char *s, long *value)
{
    char *end = NULL;
    long v;

    errno = 0;
    if (s == NULL || *s == '\0') {
        return -1;
    }

    v = strtol(s, &end, 10);
    if (errno == ERANGE || end == s || *end != '\0') {
        return -1;
    }

    *value = v;
    return 0;
}

static int 金額加算(long base, long add, long *result)
{
    if ((add > 0L && base > LONG_MAX - add) ||
        (add < 0L && base < LONG_MIN - add)) {
        return -1;
    }

    *result = base + add;
    return 0;
}

static int 加盟店検索(const 加盟店 *rows, size_t count, const char *code)
{
    size_t i;

    for (i = 0U; i < count; i++) {
        if (strcmp(rows[i].code, code) == 0) {
            return (int)i;
        }
    }

    return -1;
}

static int 明細検索(const 明細集計 *rows, size_t count, const char *settle_id, const char *merchant_code)
{
    size_t i;

    for (i = 0U; i < count; i++) {
        if (strcmp(rows[i].settle_id, settle_id) == 0 &&
            strcmp(rows[i].merchant_code, merchant_code) == 0) {
            return (int)i;
        }
    }

    return -1;
}

static int 加盟店読込(加盟店 *rows, size_t *count)
{
    FILE *fp = fopen(入力加盟店ファイル, "r");
    char line[行最大長];
    long lineno = 0L;

    if (fp == NULL) {
        fprintf(stderr, "加盟店ファイルを開けません: %s\n", 入力加盟店ファイル);
        return -1;
    }

    *count = 0U;
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[項目最大数];

        lineno++;
        改行除去(line);
        if (lineno == 1L && strstr(line, "MERCHANT-CODE") != NULL) {
            continue;
        }

        if (csv分割(line, f, 項目最大数) != 4) {
            fprintf(stderr, "加盟店ファイルの項目数が不正です: %ld\n", lineno);
            fclose(fp);
            return -1;
        }

        if (*count >= 加盟店最大件数) {
            fprintf(stderr, "加盟店件数が上限を超えました\n");
            fclose(fp);
            return -1;
        }

        if (文字列複写(rows[*count].code, sizeof(rows[*count].code), f[0]) != 0 ||
            文字列複写(rows[*count].name, sizeof(rows[*count].name), f[1]) != 0 ||
            文字列複写(rows[*count].status, sizeof(rows[*count].status), f[2]) != 0 ||
            文字列複写(rows[*count].bank, sizeof(rows[*count].bank), f[3]) != 0) {
            fprintf(stderr, "加盟店ファイルの項目長が不正です: %ld\n", lineno);
            fclose(fp);
            return -1;
        }

        (*count)++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "加盟店ファイルの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int 精算読込(精算 *rows, size_t *count)
{
    FILE *fp = fopen(入力精算ファイル, "r");
    char line[行最大長];
    long lineno = 0L;

    if (fp == NULL) {
        fprintf(stderr, "精算ファイルを開けません: %s\n", 入力精算ファイル);
        return -1;
    }

    *count = 0U;
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[項目最大数];

        lineno++;
        改行除去(line);
        if (lineno == 1L && strstr(line, "SETTLE-ID") != NULL) {
            continue;
        }

        if (csv分割(line, f, 項目最大数) != 6) {
            fprintf(stderr, "精算ファイルの項目数が不正です: %ld\n", lineno);
            fclose(fp);
            return -1;
        }

        if (*count >= 精算最大件数) {
            fprintf(stderr, "精算件数が上限を超えました\n");
            fclose(fp);
            return -1;
        }

        if (文字列複写(rows[*count].settle_id, sizeof(rows[*count].settle_id), f[0]) != 0 ||
            文字列複写(rows[*count].merchant_code, sizeof(rows[*count].merchant_code), f[1]) != 0 ||
            金額読取(f[2], &rows[*count].net_amt) != 0 ||
            金額読取(f[3], &rows[*count].charge_amt) != 0 ||
            金額読取(f[4], &rows[*count].payout_amt) != 0 ||
            文字列複写(rows[*count].settle_dt, sizeof(rows[*count].settle_dt), f[5]) != 0) {
            fprintf(stderr, "精算ファイルの内容が不正です: %ld\n", lineno);
            fclose(fp);
            return -1;
        }

        (*count)++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "精算ファイルの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int 明細読込(明細集計 *rows, size_t *count)
{
    FILE *fp = fopen(入力明細ファイル, "r");
    char line[行最大長];
    long lineno = 0L;

    if (fp == NULL) {
        fprintf(stderr, "明細ファイルを開けません: %s\n", 入力明細ファイル);
        return -1;
    }

    *count = 0U;
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *f[項目最大数];
        long txn_amt;
        long charge_amt;
        int pos;

        lineno++;
        改行除去(line);
        if (lineno == 1L && strstr(line, "DETAIL-ID") != NULL) {
            continue;
        }

        if (csv分割(line, f, 項目最大数) != 7) {
            fprintf(stderr, "明細ファイルの項目数が不正です: %ld\n", lineno);
            fclose(fp);
            return -1;
        }

        if (金額読取(f[4], &txn_amt) != 0 || 金額読取(f[5], &charge_amt) != 0) {
            fprintf(stderr, "明細ファイルの金額が不正です: %ld\n", lineno);
            fclose(fp);
            return -1;
        }

        if (strcmp(f[6], "C") != 0 && strcmp(f[6], "R") != 0) {
            fprintf(stderr, "明細ファイルの取引区分が不正です: %ld\n", lineno);
            fclose(fp);
            return -1;
        }

        pos = 明細検索(rows, *count, f[1], f[2]);
        if (pos < 0) {
            if (*count >= 明細集計最大件数) {
                fprintf(stderr, "明細集計件数が上限を超えました\n");
                fclose(fp);
                return -1;
            }

            memset(&rows[*count], 0, sizeof(rows[*count]));
            if (文字列複写(rows[*count].settle_id, sizeof(rows[*count].settle_id), f[1]) != 0 ||
                文字列複写(rows[*count].merchant_code, sizeof(rows[*count].merchant_code), f[2]) != 0) {
                fprintf(stderr, "明細ファイルの項目長が不正です: %ld\n", lineno);
                fclose(fp);
                return -1;
            }

            pos = (int)(*count);
            (*count)++;
        }

        if (strcmp(f[6], "C") == 0) {
            if (金額加算(rows[pos].capture_amt, txn_amt, &rows[pos].capture_amt) != 0) {
                fprintf(stderr, "売上明細の集計で桁あふれしました: %ld\n", lineno);
                fclose(fp);
                return -1;
            }
        } else {
            if (金額加算(rows[pos].refund_amt, txn_amt, &rows[pos].refund_amt) != 0) {
                fprintf(stderr, "返金明細の集計で桁あふれしました: %ld\n", lineno);
                fclose(fp);
                return -1;
            }
        }

        if (金額加算(rows[pos].charge_amt, charge_amt, &rows[pos].charge_amt) != 0 ||
            金額加算(rows[pos].line_count, 1L, &rows[pos].line_count) != 0) {
            fprintf(stderr, "明細集計で桁あふれしました: %ld\n", lineno);
            fclose(fp);
            return -1;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "明細ファイルの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

static int 帳票出力(const 精算 *settles, size_t settle_count,
                 const 加盟店 *merchants, size_t merchant_count,
                 const 明細集計 *details, size_t detail_count)
{
    FILE *fp = fopen(出力帳票ファイル, "w");
    size_t i;
    long report_seq = 1L;

    if (fp == NULL) {
        fprintf(stderr, "帳票ファイルを開けません: %s\n", 出力帳票ファイル);
        return -1;
    }

    for (i = 0U; i < settle_count; i++) {
        int merchant_pos = 加盟店検索(merchants, merchant_count, settles[i].merchant_code);
        int detail_pos = 明細検索(details, detail_count, settles[i].settle_id, settles[i].merchant_code);
        long detail_net = 0L;
        long expected_charge = 0L;
        long expected_payout = 0L;
        int warning = 0;
        char report_id[32];
        char output_path[帳票経路最大長];
        const char *status = "00";
        const char *report_kbn = "S";

        if (merchant_pos < 0) {
            warning = 1;
            status = "91";
        } else if (strcmp(merchants[merchant_pos].status, "01") != 0) {
            warning = 1;
            status = "92";
        }

        if (detail_pos < 0) {
            warning = 1;
            status = "93";
        } else {
            if (金額加算(details[detail_pos].capture_amt, -details[detail_pos].refund_amt, &detail_net) != 0) {
                fprintf(stderr, "明細純額の算出で桁あふれしました\n");
                fclose(fp);
                return -1;
            }

            /* 手数料額は精算ファイル(PSSETF)の登録値を正とし、本処理では
               純額・手数料・支払額の整合のみを突合する。手数料の丸め方は
               精算エンジン側で確定するため、ここでは再計算しない。 */
            expected_charge = settles[i].charge_amt;
            if (金額加算(detail_net, -expected_charge, &expected_payout) != 0) {
                fprintf(stderr, "支払予定額の算出で桁あふれしました\n");
                fclose(fp);
                return -1;
            }

            if (detail_net != settles[i].net_amt ||
                details[detail_pos].charge_amt != settles[i].charge_amt ||
                expected_payout != settles[i].payout_amt) {
                warning = 1;
                status = "94";
            }
        }

        if (warning != 0) {
            report_kbn = "W";
        }

        if (snprintf(report_id, sizeof(report_id), "R%010ld", report_seq) >= (int)sizeof(report_id) ||
            snprintf(output_path, sizeof(output_path), "/batch/mipay/%s/%s.rpt",
                     settles[i].settle_dt, settles[i].merchant_code) >= (int)sizeof(output_path)) {
            fprintf(stderr, "帳票識別子の編集に失敗しました\n");
            fclose(fp);
            return -1;
        }

        if (fprintf(fp, "%s,%s,%s,%s,%s,%s,%s\n",
                    report_id,
                    settles[i].merchant_code,
                    report_kbn,
                    settles[i].settle_dt,
                    settles[i].settle_dt,
                    output_path,
                    status) < 0) {
            fprintf(stderr, "帳票ファイルの書込に失敗しました\n");
            fclose(fp);
            return -1;
        }

        report_seq++;
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "帳票ファイルのクローズに失敗しました\n");
        return -1;
    }

    return 0;
}

int main(void)
{
    static 加盟店 merchants[加盟店最大件数];
    static 精算 settles[精算最大件数];
    static 明細集計 details[明細集計最大件数];
    size_t merchant_count = 0U;
    size_t settle_count = 0U;
    size_t detail_count = 0U;

    if (加盟店読込(merchants, &merchant_count) != 0) {
        return 異常終了コード;
    }

    if (精算読込(settles, &settle_count) != 0) {
        return 異常終了コード;
    }

    if (明細読込(details, &detail_count) != 0) {
        return 異常終了コード;
    }

    if (帳票出力(settles, settle_count, merchants, merchant_count, details, detail_count) != 0) {
        return 異常終了コード;
    }

    return 正常終了コード;
}
