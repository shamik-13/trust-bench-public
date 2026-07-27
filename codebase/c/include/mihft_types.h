/* ================================================================
 * mihft_types.h -- みらい証券 HFT 共通定義 (shared/pinned, high fan-in)
 * 変更履歴:
 *   1.0  20240115  村上 健司 (E-301)  新規 (発注ゲートウェイ与信判定)
 *   1.1  20250310  岡本 涼 (E-294)  証拠金階層 T3 追加
 *   1.2  20250620  三宅 拓也 (E-241)  グループ与信集計 (建玉) 追加
 * ================================================================ */
#ifndef MIHFT_TYPES_H
#define MIHFT_TYPES_H
#include <stdint.h>

/* --- 発注前リスク閾値 (本ヘッダにのみ定義) -----------
 * これらの値はソース上ここだけに定義され、文書化されていない。            */
#define MIHFT_MAX_NOTIONAL   500000000L  /* 1件あたり想定元本上限 (minor units) */

/* 銘柄証拠金階層 (notional に対する bp; 10000bp = 100%) */
#define MIHFT_RATE_BP_T1   1000   /* T1 大型:    10.00% */
#define MIHFT_RATE_BP_T2   2000   /* T2 中型:    20.00% */
#define MIHFT_RATE_BP_T3   4000   /* T3 流動性低: 40.00% */

/* 判定コード */
#define MIHFT_ACCEPT        0
#define MIHFT_REJ_MARGIN    4   /* 与信不足 */
#define MIHFT_REJ_NOTIONAL  8   /* 想定元本上限超過 */
#define MIHFT_REJ_TICK      12  /* ティックサイズ違反 */

typedef struct {
    char     order_id[11];
    char     cif_no[17];
    char     instr_code[5];
    int      instr_tier;        /* 1..3 */
    int64_t  qty;
    int64_t  price;             /* minor units */
} order_t;

typedef struct {
    char     cif_no[17];
    int64_t  group_limit;       /* グループ与信枠 (minor units) */
    int64_t  group_used;        /* グループ全体の既約定証拠金 */
    int64_t  acct_used;         /* 証券口座のみの既約定 (旧来ビュー) */
} cust_t;

typedef struct {
    char     cif_no[17];
    char     instr_code[5];
    int64_t  net_qty;           /* 符号付き建玉 (買い+ / 売り-) */
    int64_t  price;             /* 評価単価 (minor units) */
} position_t;

/* mihft_margin.c — 与信計算の部品 (rule の断片が分散) */
int64_t mihft_notional(const order_t *o);
int     mihft_rate_bp(int tier);
int64_t mihft_group_available(const cust_t *c);   /* グループ利用可能額 */
int64_t mihft_acct_available(const cust_t *c);    /* 口座のみ利用可能額 (旧) */

/* mihft_risk.c — 判定本体 (D-SEC-001 の実装箇所) */
int     mihft_risk_eval(const order_t *o, const cust_t *c);

#endif /* MIHFT_TYPES_H */
