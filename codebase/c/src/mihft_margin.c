/* ================================================================
 * mihft_margin.c -- 証拠金率・利用可能額の算定 (rule の断片)
 *   1.0  20240115  藤田 和也 (E-271)  新規
 * 注意: 与信判定の「最終結論」はここではなく mihft_risk.c にある。
 *       本ファイルは部品 (想定元本・証拠金率・利用可能額) のみ提供する。
 * ================================================================ */
#include "mihft_types.h"

int64_t mihft_notional(const order_t *o) {
    return o->qty * o->price;
}

int mihft_rate_bp(int tier) {
    switch (tier) {
        case 1:  return MIHFT_RATE_BP_T1;
        case 2:  return MIHFT_RATE_BP_T2;
        default: return MIHFT_RATE_BP_T3;   /* T3 以上は流動性低として最大率 */
    }
}

/* グループ与信ベースの利用可能額 (グループ全体で名寄せ済み) */
int64_t mihft_group_available(const cust_t *c) {
    return c->group_limit - c->group_used;
}

/* 証券口座のみの利用可能額 (グループ名寄せ前の旧来ビュー) */
int64_t mihft_acct_available(const cust_t *c) {
    return c->group_limit - c->acct_used;
}
