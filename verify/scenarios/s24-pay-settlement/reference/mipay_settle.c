/* ================================================================
 * mipay_settle.c -- 加盟店精算エンジン (D-2401 実装箇所)  [GOLDEN]
 *   1.0  20240408  ペイ精算基盤  新規
 *   2.0  20250515  西野 諒       D-2401 適用: 決済処理手数料を集計ネットに対し円未満四捨五入
 * ================================================================ */
#include "mipay_settle.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_TXN 1024
#define MAX_MER 256

/* D-2401: 処理手数料 = 集計ネットに料率を乗じ、円未満を四捨五入 (0.5以上は切上げ)。
 * net<=0 の加盟店は手数料 0。 */
int64_t mipay_proc_charge(int64_t net) {
    if (net <= 0) return 0;
    return (net * MIPAY_PROC_RATE_BP + 5000) / 10000;   /* +5000 = 円未満四捨五入 */
}

int64_t mipay_merchant_payout(int64_t net) {
    return net - mipay_proc_charge(net);
}

static int read_txns(const char *path, txn_t *out, int max) {
    FILE *fp = fopen(path, "r"); if (!fp) return 0;
    char line[512]; int n = 0;
    while (fgets(line, sizeof line, fp) && n < max) {
        if (line[0] == '#' || line[0] == '\n') continue;
        char *p = line, *f[5]; int k = 0;
        for (char *t = strtok(p, ",\n"); t && k < 5; t = strtok(NULL, ",\n")) f[k++] = t;
        if (k < 5) continue;
        strncpy(out[n].txn_id, f[0], sizeof out[n].txn_id - 1);
        strncpy(out[n].merchant_code, f[1], sizeof out[n].merchant_code - 1);
        out[n].txn_kbn = f[2][0];
        out[n].amount = atoll(f[3]);
        out[n].txn_dt = atoi(f[4]);
        n++;
    }
    fclose(fp); return n;
}

static int read_mers(const char *path, mer_t *out, int max) {
    FILE *fp = fopen(path, "r"); if (!fp) return 0;
    char line[256]; int n = 0;
    while (fgets(line, sizeof line, fp) && n < max) {
        if (line[0] == '#' || line[0] == '\n') continue;
        char *f[2]; int k = 0;
        for (char *t = strtok(line, ",\n"); t && k < 2; t = strtok(NULL, ",\n")) f[k++] = t;
        if (k < 2) continue;
        strncpy(out[n].merchant_code, f[0], sizeof out[n].merchant_code - 1);
        strncpy(out[n].status, f[1], sizeof out[n].status - 1);
        n++;
    }
    fclose(fp); return n;
}

/* fixture harness: read merchants.csv + transactions.csv, print
 * "<merchant> <net> <charge> <payout>" per settleable (MR-MER-STATUS='01') merchant. */
int main(void) {
    static txn_t txns[MAX_TXN]; static mer_t mers[MAX_MER];
    int nt = read_txns("transactions.csv", txns, MAX_TXN);
    int nm = read_mers("merchants.csv", mers, MAX_MER);
    for (int i = 0; i < nm; i++) {
        if (strcmp(mers[i].status, "01") != 0) continue;     /* 精算対象のみ */
        int64_t net = mipay_merchant_net(txns, nt, mers[i].merchant_code);
        int64_t charge = mipay_proc_charge(net);
        int64_t payout = net - charge;
        printf("%s %lld %lld %lld\n", mers[i].merchant_code,
               (long long)net, (long long)charge, (long long)payout);
    }
    return 0;
}
