/* ================================================================
 * mipay_settle.h -- みらいペイ 加盟店精算 共通定義 (shared/pinned, high fan-in)
 * 変更履歴:
 *   1.0  20240408  ペイ精算基盤  新規 (加盟店精算ネッティング)
 *   1.1  20250515  ペイ精算基盤  決済処理手数料率 (一律0.30%) を定義
 * ================================================================ */
#ifndef MIPAY_SETTLE_H
#define MIPAY_SETTLE_H
#include <stdint.h>

/* --- 決済処理手数料率 (本ヘッダにのみ定義) ----------
 * 率はここに定義されるが、丸め方法 (集計後 四捨五入 か 明細毎 切捨て か) は
 * 本ヘッダには無く、精算エンジン mipay_settle.c の実装にのみ存在する。       */
#define MIPAY_PROC_RATE_BP   30     /* 決済処理手数料 0.30% (10000bp = 100%) */

typedef struct {
    char     txn_id[13];
    char     merchant_code[7];
    char     txn_kbn;             /* 'C' 売上 / 'R' 返金 */
    int64_t  amount;              /* 円 (integer minor units) */
    int      txn_dt;              /* YYYYMMDD */
} txn_t;

typedef struct {
    char     merchant_code[7];
    char     status[3];           /* MR-MER-STATUS */
} mer_t;

/* mipay_net.c -- ネッティングの部品 */
int64_t mipay_merchant_net(const txn_t *txns, int n, const char *merchant_code);

/* mipay_settle.c -- 精算本体 */
int64_t mipay_proc_charge(int64_t net);          /* 処理手数料 (丸めはここに実装) */
int64_t mipay_merchant_payout(int64_t net);      /* 振込額 = net - charge */

#endif /* MIPAY_SETTLE_H */
