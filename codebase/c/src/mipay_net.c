/* ================================================================
 * mipay_net.c -- 加盟店精算 ネッティング部品
 *   1.0  20240408  ペイ精算基盤  新規 (加盟店別 売上/返金ネット)
 * ================================================================ */
#include "mipay_settle.h"
#include <string.h>

/* 加盟店 merchant_code の日次ネット = Σ(売上 'C') − Σ(返金 'R')。円 (integer minor units)。
 * 丸めや手数料は扱わない (純粋な集計のみ)。 */
int64_t mipay_merchant_net(const txn_t *txns, int n, const char *merchant_code) {
    int64_t net = 0;
    for (int i = 0; i < n; i++) {
        if (strcmp(txns[i].merchant_code, merchant_code) != 0) continue;
        if (txns[i].txn_kbn == 'C') net += txns[i].amount;
        else if (txns[i].txn_kbn == 'R') net -= txns[i].amount;
    }
    return net;
}
