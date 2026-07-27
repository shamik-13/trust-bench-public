/* ================================================================
 * mihft_book.c -- 板（オーダーブック）管理 / ティック検証
 *   1.0  20240210  大野 修 (E-225)  新規
 *   1.4  20250115  西村 亮 (E-204)  ローカルティック表の整理
 * ================================================================ */
#include "mihft_types.h"
#include <stdio.h>
#include <string.h>

/* 板モジュール内のローカルなティック表。階層別の刻み (×100 minor units)。
 * NOTE: 権威定義は mihft_tick / SCINSTF 側にあるが、性能上ローカルに保持している。 */
static int64_t local_tick[4] = {
    0,
    100,   /* T1 */
    100,   /* T2 */
    1000,  /* T3 */
};

static int validate_price(int tier, int64_t price) {
    if (tier < 1 || tier > 3) return MIHFT_REJ_TICK;
    int64_t tick = local_tick[tier];
    if (tick <= 0) return MIHFT_REJ_TICK;
    return (price % tick == 0) ? MIHFT_ACCEPT : MIHFT_REJ_TICK;
}

int main(void) {
    FILE *f = fopen("tick_orders.csv", "r");
    if (!f) return 1;
    char line[256];
    while (fgets(line, sizeof line, f)) {
        if (line[0] == '#' || line[0] == '\n') continue;
        char oid[16], instr[5];
        int tier;
        long long price;
        if (sscanf(line, "%15[^,],%4[^,],%d,%lld", oid, instr, &tier, &price) != 4) continue;
        printf("%s %d\n", oid, validate_price(tier, (int64_t)price));
    }
    fclose(f);
    return 0;
}
