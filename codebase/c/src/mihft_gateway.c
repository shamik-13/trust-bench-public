/* ================================================================
 * mihft_gateway.c -- みらいHFT 発注ゲートウェイ (main / driver)
 *   1.0  20240115  福田 亮太 (E-211)  新規
 * orders.csv / customers.csv を読み、各注文を mihft_risk_eval で判定し
 * 「<注文ID> <判定コード>」を1行ずつ標準出力へ。判定の中身は持たない。
 * ================================================================ */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mihft_types.h"

#define MAXO 256
#define MAXC 256

static int load_customers(cust_t *cs, int max) {
    FILE *f = fopen("customers.csv", "r");
    if (!f) { fprintf(stderr, "customers.csv オープン失敗\n"); return -1; }
    char line[256]; int n = 0;
    while (fgets(line, sizeof line, f) && n < max) {
        if (line[0] == '#' || line[0] == '\n') continue;
        cust_t *c = &cs[n]; memset(c, 0, sizeof *c);
        char *p = strtok(line, ",");   if (!p) continue; strncpy(c->cif_no, p, sizeof c->cif_no - 1);
        p = strtok(NULL, ",");         if (!p) continue; c->group_limit = atoll(p);
        p = strtok(NULL, ",");         if (!p) continue; c->group_used  = atoll(p);
        p = strtok(NULL, ",\n");       if (!p) continue; c->acct_used   = atoll(p);
        n++;
    }
    fclose(f); return n;
}

static int load_orders(order_t *os, int max) {
    FILE *f = fopen("orders.csv", "r");
    if (!f) { fprintf(stderr, "orders.csv オープン失敗\n"); return -1; }
    char line[256]; int n = 0;
    while (fgets(line, sizeof line, f) && n < max) {
        if (line[0] == '#' || line[0] == '\n') continue;
        order_t *o = &os[n]; memset(o, 0, sizeof *o);
        char *p = strtok(line, ",");   if (!p) continue; strncpy(o->order_id,   p, sizeof o->order_id - 1);
        p = strtok(NULL, ",");         if (!p) continue; strncpy(o->cif_no,     p, sizeof o->cif_no - 1);
        p = strtok(NULL, ",");         if (!p) continue; strncpy(o->instr_code, p, sizeof o->instr_code - 1);
        p = strtok(NULL, ",");         if (!p) continue; o->instr_tier = atoi(p);
        p = strtok(NULL, ",");         if (!p) continue; o->qty   = atoll(p);
        p = strtok(NULL, ",\n");       if (!p) continue; o->price = atoll(p);
        n++;
    }
    fclose(f); return n;
}

static const cust_t *find_cust(const cust_t *cs, int nc, const char *cif) {
    for (int i = 0; i < nc; i++)
        if (strcmp(cs[i].cif_no, cif) == 0) return &cs[i];
    return NULL;
}

int main(void) {
    cust_t cs[MAXC]; order_t os[MAXO];
    int nc = load_customers(cs, MAXC);
    int no = load_orders(os, MAXO);
    if (nc < 0 || no < 0) return 12;
    for (int i = 0; i < no; i++) {
        const cust_t *c = find_cust(cs, nc, os[i].cif_no);
        int d = c ? mihft_risk_eval(&os[i], c) : MIHFT_REJ_MARGIN;  /* 顧客不明は与信不可扱い */
        printf("%s %d\n", os[i].order_id, d);
    }
    return 0;
}
