/* ================================================================
 * mihft_pos.c -- ポジション管理
 *   1.0  20240301  藤田 和也 (E-271)  新規
 *   1.2  20250118  三宅 拓也 (E-241)  建玉集計のリファクタ
 * ================================================================ */
#include "mihft_types.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* 加重平均パスの有効化フラグ。将来対応で導入したが現行は無効(0)のまま。 */
#ifndef MIHFT_POS_USE_WEIGHTED
#define MIHFT_POS_USE_WEIGHTED 0
#endif

typedef struct {
    char    cif[17];
    char    instr[5];
    int64_t net_qty;
    int64_t cost_sum;    /* 加重平均用の積上げ (現状は未使用) */
    int64_t last_amt;    /* 直近約定単価 */
    int     used;
} pos_acc_t;

static int64_t avg_cost(const pos_acc_t *p) {
    if (MIHFT_POS_USE_WEIGHTED) {
        /* 加重平均 (Σ qty×amt / Σ qty)。フラグが立っていないため到達しない。 */
        return p->net_qty != 0 ? p->cost_sum / p->net_qty : 0;
    }
    /* 現行の稼働パス: 直近約定単価をそのまま平均取得単価とする。 */
    return p->last_amt;
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
        p->cost_sum += (int64_t)qty * (int64_t)amt;
        p->last_amt = amt;
    }
    fclose(f);
    for (int i = 0; i < n; i++)
        if (pos[i].used)
            printf("%s:%s %lld\n", pos[i].cif, pos[i].instr, (long long)avg_cost(&pos[i]));
    return 0;
}
