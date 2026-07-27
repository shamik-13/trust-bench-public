/* ================================================================
 * mihft_risk.c -- 発注前リスク判定 (D-SEC-001 実装箇所)  [GOLDEN]
 *   1.0  20240115  証券IT基盤  新規
 *   2.0  20250620  証券IT基盤  D-SEC-001 適用: グループ与信に基づく判定
 *                              + 1件あたり想定元本上限の適用
 * ================================================================ */
#include "mihft_types.h"

int mihft_risk_eval(const order_t *o, const cust_t *c) {
    int64_t notional = mihft_notional(o);
    /* D-SEC-001: 1件あたり想定元本上限を超える注文は問答無用で謝絶 */
    if (notional > MIHFT_MAX_NOTIONAL) return MIHFT_REJ_NOTIONAL;
    int64_t required  = notional * mihft_rate_bp(o->instr_tier) / 10000;
    int64_t available = mihft_group_available(c);  /* D-SEC-001: グループ与信ベース */
    if (required > available) return MIHFT_REJ_MARGIN;
    return MIHFT_ACCEPT;
}
