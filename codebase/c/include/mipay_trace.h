/* ================================================================
 * mipay_trace.h -- みらいペイ 精算追跡 共通定義 (C 精算側)
 * 変更履歴:
 *   1.0  20240412  ペイ精算基盤  新規 (精算明細の取込・ネット)
 * ================================================================ */
#ifndef MIPAY_TRACE_H
#define MIPAY_TRACE_H
#include <stdint.h>

typedef struct {
    char     settle_txn_id[17];
    char     merchant_code[7];
    int64_t  amount;            /* 円 */
    char     settle_kbn[2];     /* 精算区分 (1/2/9) */
} settle_row_t;

/* mipay_nettrace.c -- 加盟店別 精算ネット (精算区分の取扱いは実装にのみ存在) */
int64_t mipay_merchant_settled_net(const settle_row_t *rows, int n, const char *merchant_code);

#endif /* MIPAY_TRACE_H */
