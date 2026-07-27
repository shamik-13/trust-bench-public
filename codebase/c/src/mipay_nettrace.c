/* ================================================================
 * mipay_nettrace.c -- 加盟店精算ネット (精算側; 売上確定リンクを取り込む)
 *   1.0  20240412  ペイ精算基盤  新規 (精算区分による加盟店別ネット)
 * ================================================================ */
#include "mipay_trace.h"
#include <string.h>

/*
 * 加盟店 merchant_code の精算ネットを、精算明細の精算区分(SETTLE-KBN)に応じて合算する。
 */
int64_t mipay_merchant_settled_net(const settle_row_t *rows, int n, const char *merchant_code) {
    int64_t net = 0;
    for (int i = 0; i < n; i++) {
        if (strcmp(rows[i].merchant_code, merchant_code) != 0) continue;
        if (strcmp(rows[i].settle_kbn, "1") != 0) continue;   /* 即時精算のみ計上 */
        net += rows[i].amount;
    }
    return net;
}
