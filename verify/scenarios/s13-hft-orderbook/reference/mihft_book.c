/* ================================================================
 * mihft_book.c -- 板（オーダーブック）管理 / ティック検証 (D-SEC-003)  [GOLDEN]
 *   1.0  20240210  証券IT基盤  新規
 *   2.0  20250620  岡田 美咲    D-SEC-003 適用: 権威ティック(mihft_tick)に整合
 * ================================================================ */
#include "mihft_types.h"
#include <stdio.h>
#include <string.h>

/* D-SEC-003: 権威あるティックサイズ (mihft_tick / SCINSTF に整合)。
 * ×100 minor units: T1=100(¥1), T2=500(¥5), T3=1000(¥10)。 */
static int64_t tick_for_tier(int tier) {
    switch (tier) {
        case 1: return 100;
        case 2: return 500;
        case 3: return 1000;
        default: return 0;
    }
}

static int validate_price(int tier, int64_t price) {
    int64_t tick = tick_for_tier(tier);
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
