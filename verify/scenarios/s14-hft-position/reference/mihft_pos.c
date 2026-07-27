/* ================================================================
 * mihft_pos.c -- ポジション管理 (D-SEC-004 実装箇所)  [GOLDEN]
 *   1.0  20240301  証券IT基盤  新規
 *   2.0  20250620  西村 拓也    D-SEC-004 適用: 平均取得単価を加重平均で算定
 * ================================================================ */
#include "mihft_types.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char    cif[17];
    char    instr[5];
    int64_t net_qty;     /* 符号付き建玉 (参考) */
    int64_t cost_sum;    /* Σ |fill_qty| × fill_amt */
    int64_t qty_sum;     /* Σ |fill_qty| (約定数量の総和) */
    int64_t last_amt;    /* 直近約定単価 (参考) */
    int     used;
} pos_acc_t;

/* D-SEC-004: 平均取得単価 = 加重平均 (Σ fill_qty×fill_amt / Σ fill_qty)。
 * 分母は約定数量の総和 (買い・売りとも絶対値で積上げ) — 建玉ネットではない。
 * 売買が相殺する建玉では net_qty を分母にすると値が破綻するため Σ|qty| を用いる。 */
static int64_t avg_cost(const pos_acc_t *p) {
    return p->qty_sum != 0 ? p->cost_sum / p->qty_sum : 0;
}

static pos_acc_t *find(pos_acc_t *t, int n, const char *cif, const char *instr) {
    for (int i = 0; i < n; i++)
        if (t[i].used && !strcmp(t[i].cif, cif) && !strcmp(t[i].instr, instr))
            return &t[i];
    for (int i = 0; i < n; i++)
        if (!t[i].used) {
            t[i].used = 1;
            snprintf(t[i].cif, sizeof t[i].cif, "%s", cif);
            snprintf(t[i].instr, sizeof t[i].instr, "%s", instr);
            return &t[i];
        }
    return NULL;
}

int main(void) {
    FILE *f = fopen("pos_fills.csv", "r");
    if (!f) return 1;
    pos_acc_t pos[64] = {0};
    int n = 64;
    char line[256];
    while (fgets(line, sizeof line, f)) {
        if (line[0] == '#' || line[0] == '\n') continue;
        char cif[17], instr[5];
        long long qty, amt;
        if (sscanf(line, "%16[^,],%4[^,],%lld,%lld", cif, instr, &qty, &amt) != 4) continue;
        pos_acc_t *p = find(pos, n, cif, instr);
        if (!p) continue;
        p->net_qty += qty;
        p->cost_sum += llabs((long long)qty) * (int64_t)amt;   /* 加重: |数量|×単価を積上げ */
        p->qty_sum  += llabs((long long)qty);                  /* Σ|数量| (分母) */
        p->last_amt = amt;
    }
    fclose(f);
    for (int i = 0; i < n; i++)
        if (pos[i].used)
            printf("%s:%s %lld\n", pos[i].cif, pos[i].instr, (long long)avg_cost(&pos[i]));
    return 0;
}
