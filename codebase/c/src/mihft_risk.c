/* ================================================================
 * mihft_risk.c -- 発注前リスク判定 (D-SEC-001 実装箇所)
 *   1.0  20240115  福田 亮太 (E-211)  新規
 * required = notional × rate(tier) / 10000 を利用可能額と比較する。
 * ================================================================ */
#include "mihft_types.h"

int mihft_risk_eval(const order_t *o, const cust_t *c) {
    int64_t notional  = mihft_notional(o);
    int64_t required  = notional * mihft_rate_bp(o->instr_tier) / 10000;
    int64_t available = mihft_acct_available(c);   /* 口座のみビュー (名寄せ前) */
    if (required > available) return MIHFT_REJ_MARGIN;
    return MIHFT_ACCEPT;
}
