/* 
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20191022  中川 美和 (E-283)    約定レコード生成処理の初版作成
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define 入力行最大 1024
#define 注文最大件数 4096
#define 板最大件数 8192
#define 識別子最大 64
#define 種別最大 16
#define 時刻最大 64

typedef struct {
    char order_id[識別子最大];
    char cif_no[識別子最大];
    char instr_code[識別子最大];
    char side_kbn;
    char ord_type;
    char tif_code[種別最大];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} 注文レコード;

typedef struct {
    char instr_code[識別子最大];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int64_t order_cnt;
    char entry_ts[時刻最大];
} 板レコード;

static void 右端改行除去(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static char *前後空白除去(char *s)
{
    unsigned char *p = (unsigned char *)s;
    char *e;

    while (*p != '\0' && isspace(*p)) {
        p++;
    }

    e = (char *)p + strlen((char *)p);
    while (e > (char *)p && isspace((unsigned char)e[-1])) {
        *--e = '\0';
    }

    return (char *)p;
}

static int CSV分割(char *行, char **列, int 列最大)
{
    int 件数 = 0;
    char *p = 行;

    while (件数 < 列最大) {
        列[件数++] = p;
        while (*p != '\0' && *p != ',') {
            p++;
        }
        if (*p == '\0') {
            break;
        }
        *p++ = '\0';
    }

    return 件数;
}

static int 整数読込(const char *s, int64_t *out)
{
    char *end;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (s == end || errno == ERANGE) {
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

static int 文字列格納(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);

    if (n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int 注文読込(const char *path, 注文レコード *注文, size_t *件数)
{
    FILE *fp = fopen(path, "r");
    char 行[入力行最大];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "入力ファイルを開けません: %s\n", path);
        return -1;
    }

    while (fgets(行, sizeof 行, fp) != NULL) {
        char *列[9];
        int64_t v;
        int c;

        右端改行除去(行);
        if (行[0] == '\0') {
            continue;
        }
        if (strncmp(行, "ORDER-ID", 8) == 0) {
            continue;
        }
        if (n >= 注文最大件数) {
            fprintf(stderr, "注文件数が上限を超過しました\n");
            fclose(fp);
            return -1;
        }

        c = CSV分割(行, 列, 9);
        if (c != 9) {
            fprintf(stderr, "注文CSVの列数が不正です\n");
            fclose(fp);
            return -1;
        }

        for (c = 0; c < 9; c++) {
            列[c] = 前後空白除去(列[c]);
        }

        if (文字列格納(注文[n].order_id, sizeof 注文[n].order_id, 列[0]) != 0 ||
            文字列格納(注文[n].cif_no, sizeof 注文[n].cif_no, 列[1]) != 0 ||
            文字列格納(注文[n].instr_code, sizeof 注文[n].instr_code, 列[2]) != 0 ||
            strlen(列[3]) != 1 || strlen(列[4]) != 1 ||
            文字列格納(注文[n].tif_code, sizeof 注文[n].tif_code, 列[5]) != 0) {
            fprintf(stderr, "注文CSVの文字項目が不正です\n");
            fclose(fp);
            return -1;
        }

        注文[n].side_kbn = 列[3][0];
        注文[n].ord_type = 列[4][0];

        if (整数読込(列[6], &注文[n].ord_qty) != 0 ||
            整数読込(列[7], &注文[n].price_amt) != 0 ||
            整数読込(列[8], &v) != 0 ||
            v < INT_MIN || v > INT_MAX) {
            fprintf(stderr, "注文CSVの数値項目が不正です\n");
            fclose(fp);
            return -1;
        }
        注文[n].instr_tier = (int)v;
        n++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "注文CSVの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *件数 = n;
    return 0;
}

static int 板読込(const char *path, 板レコード *板, size_t *件数)
{
    FILE *fp = fopen(path, "r");
    char 行[入力行最大];
    size_t n = 0;

    if (fp == NULL) {
        fprintf(stderr, "入力ファイルを開けません: %s\n", path);
        return -1;
    }

    while (fgets(行, sizeof 行, fp) != NULL) {
        char *列[7];
        int64_t v;
        int c;

        右端改行除去(行);
        if (行[0] == '\0') {
            continue;
        }
        if (strncmp(行, "INSTR-CODE", 10) == 0) {
            continue;
        }
        if (n >= 板最大件数) {
            fprintf(stderr, "板件数が上限を超過しました\n");
            fclose(fp);
            return -1;
        }

        c = CSV分割(行, 列, 7);
        if (c != 7) {
            fprintf(stderr, "板CSVの列数が不正です\n");
            fclose(fp);
            return -1;
        }

        for (c = 0; c < 7; c++) {
            列[c] = 前後空白除去(列[c]);
        }

        if (文字列格納(板[n].instr_code, sizeof 板[n].instr_code, 列[0]) != 0 ||
            strlen(列[1]) != 1 ||
            文字列格納(板[n].entry_ts, sizeof 板[n].entry_ts, 列[6]) != 0) {
            fprintf(stderr, "板CSVの文字項目が不正です\n");
            fclose(fp);
            return -1;
        }

        板[n].side_kbn = 列[1][0];

        if (整数読込(列[2], &v) != 0 || v < INT_MIN || v > INT_MAX ||
            整数読込(列[3], &板[n].price_amt) != 0 ||
            整数読込(列[4], &板[n].book_qty) != 0 ||
            整数読込(列[5], &板[n].order_cnt) != 0) {
            fprintf(stderr, "板CSVの数値項目が不正です\n");
            fclose(fp);
            return -1;
        }
        板[n].level_cnt = (int)v;
        n++;
    }

    if (ferror(fp)) {
        fprintf(stderr, "板CSVの読込に失敗しました\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *件数 = n;
    return 0;
}

static int64_t 呼値取得(int tier)
{
    if (tier == 1) {
        return 100;
    }
    if (tier == 2) {
        return 500;
    }
    if (tier == 3) {
        return 1000;
    }
    return 0;
}

static int 注文検証(const 注文レコード *o)
{
    int64_t tick;
    int64_t 上限 = (int64_t)MIHFT_MAX_NOTIONAL * 100;

    if (o->side_kbn != 'B' && o->side_kbn != 'S') {
        return 12;
    }
    if (o->ord_type != 'L' && o->ord_type != 'M') {
        return 12;
    }
    if (strcmp(o->tif_code, "DAY") != 0 &&
        strcmp(o->tif_code, "IOC") != 0 &&
        strcmp(o->tif_code, "FOK") != 0) {
        return 12;
    }
    if (o->ord_qty <= 0 || o->price_amt < 0) {
        return 8;
    }
    if (o->ord_type == 'L') {
        if (o->price_amt == 0) {
            return 8;
        }
        tick = 呼値取得(o->instr_tier);
        if (tick == 0 || o->price_amt % tick != 0) {
            return 12;
        }
        if (o->ord_qty > INT64_MAX / o->price_amt) {
            return 8;
        }
        if (o->price_amt * o->ord_qty > 上限) {
            return 8;
        }
    }

    return 0;
}

static int 約定可能(const 注文レコード *o, const 板レコード *b)
{
    char 反対側 = (o->side_kbn == 'B') ? 'S' : 'B';

    if (strcmp(o->instr_code, b->instr_code) != 0 || b->side_kbn != 反対側) {
        return 0;
    }
    if (b->book_qty <= 0 || b->price_amt <= 0) {
        return 0;
    }
    if (o->ord_type == 'M') {
        return 1;
    }
    if (o->side_kbn == 'B') {
        return b->price_amt <= o->price_amt;
    }
    return b->price_amt >= o->price_amt;
}

static int 板順位比較(const void *a, const void *b)
{
    const 板レコード *x = (const 板レコード *)a;
    const 板レコード *y = (const 板レコード *)b;

    if (x->side_kbn != y->side_kbn) {
        return (x->side_kbn < y->side_kbn) ? -1 : 1;
    }
    if (x->side_kbn == 'S') {
        if (x->price_amt != y->price_amt) {
            return (x->price_amt < y->price_amt) ? -1 : 1;
        }
    } else {
        if (x->price_amt != y->price_amt) {
            return (x->price_amt > y->price_amt) ? -1 : 1;
        }
    }
    if (x->level_cnt != y->level_cnt) {
        return (x->level_cnt < y->level_cnt) ? -1 : 1;
    }
    return strcmp(x->entry_ts, y->entry_ts);
}

static int FOK充足(const 注文レコード *o, const 板レコード *板, size_t 板件数)
{
    int64_t 残 = o->ord_qty;
    size_t i;

    for (i = 0; i < 板件数 && 残 > 0; i++) {
        if (約定可能(o, &板[i])) {
            残 -= (板[i].book_qty < 残) ? 板[i].book_qty : 残;
        }
    }

    return 残 == 0;
}

int main(void)
{
    注文レコード 注文[注文最大件数];
    板レコード 板[板最大件数];
    size_t 注文件数 = 0;
    size_t 板件数 = 0;
    FILE *out;
    int 最終判定 = 0;
    size_t i;

    if (注文読込("SCORDF.csv", 注文, &注文件数) != 0) {
        return 2;
    }
    if (板読込("SCBOOK.csv", 板, &板件数) != 0) {
        return 2;
    }

    qsort(板, 板件数, sizeof 板[0], 板順位比較);

    out = fopen("SCEXEC.csv", "w");
    if (out == NULL) {
        fprintf(stderr, "出力ファイルを開けません: SCEXEC.csv\n");
        return 3;
    }

    if (fprintf(out, "EXEC-ID,ORDER-ID,INSTR-CODE,SIDE-KBN,FILL-QTY,FILL-AMT,EXEC-TS\n") < 0) {
        fprintf(stderr, "約定CSVの書込に失敗しました\n");
        fclose(out);
        return 3;
    }

    for (i = 0; i < 注文件数; i++) {
        注文レコード *o = &注文[i];
        int 判定 = 注文検証(o);
        int64_t 残数量 = o->ord_qty;
        int 枝番 = 1;
        size_t j;

        if (判定 != 0) {
            最終判定 = 判定;
            continue;
        }
        if (strcmp(o->tif_code, "FOK") == 0 && !FOK充足(o, 板, 板件数)) {
            continue;
        }

        for (j = 0; j < 板件数 && 残数量 > 0; j++) {
            板レコード *b = &板[j];
            int64_t 約定数量;
            int64_t 約定金額;

            if (!約定可能(o, b)) {
                continue;
            }

            約定数量 = (b->book_qty < 残数量) ? b->book_qty : 残数量;
            if (約定数量 <= 0 || b->price_amt > INT64_MAX / 約定数量) {
                continue;
            }

            約定金額 = b->price_amt * 約定数量;
            if (約定金額 == 0) {
                continue;
            }

            if (fprintf(out, "%s-%03d,%s,%s,%c,%lld,%lld,%s\n",
                        o->order_id,
                        枝番,
                        o->order_id,
                        o->instr_code,
                        o->side_kbn,
                        (long long)約定数量,
                        (long long)約定金額,
                        b->entry_ts) < 0) {
                fprintf(stderr, "約定CSVの書込に失敗しました\n");
                fclose(out);
                return 3;
            }

            枝番++;
            残数量 -= 約定数量;
            b->book_qty -= 約定数量;

            if (strcmp(o->tif_code, "IOC") == 0 && 残数量 > 0) {
                continue;
            }
        }
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "約定CSVの終了処理に失敗しました\n");
        return 3;
    }

    return 最終判定;
}
