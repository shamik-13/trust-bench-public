/* ================================================================
 * mihft_match.c -- マッチングエンジン (D-SEC-002 / D-SEC-005)  [GOLDEN]
 *   1.0  20240212  証券IT基盤  新規
 *   2.0  20250620  福田 亮太    D-SEC-002 適用: 同値はプライス・タイム優先(FIFO)で約定
 * ================================================================ */
#include "mihft_types.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct { char id[16]; int arrival; long long qty; long long filled; } resting_t;

/* D-SEC-002: 同一価格帯では到着順(FIFO)に約定する。到着順で並べて先頭から充当。 */
static int cmp_select(const void *a, const void *b) {
    const resting_t *x = a, *y = b;
    return x->arrival - y->arrival;          /* arrival ascending = FIFO */
}

static int cmp_arrival(const void *a, const void *b) {
    return ((const resting_t *)a)->arrival - ((const resting_t *)b)->arrival;
}

/* D-SEC-005: TIF (有効期間) の約定後処理。
 *  IOC = 一部約定後に未約定残を取消 / FOK = 全量約定できなければ一切約定しない /
 *  DAY = 未約定残を板に残す。 */
static int fok_blocks(const char *tif, long long incoming, long long resting_total) {
    return strcmp(tif, "FOK") == 0 && resting_total < incoming;
}

int main(void) {
    FILE *f = fopen("match_book.csv", "r");
    if (!f) return 1;
    resting_t r[64]; int n = 0; long long incoming = 0; char tif[8] = "DAY";
    char line[256];
    while (fgets(line, sizeof line, f)) {
        if (line[0] == '#' || line[0] == '\n') continue;
        char type, id[16], t[8] = "DAY"; int arr; long long qty;
        if (sscanf(line, "%c,%15[^,],%d,%lld,%7s", &type, id, &arr, &qty, t) < 4) continue;
        if (type == 'R' && n < 64) {
            snprintf(r[n].id, sizeof r[n].id, "%s", id);
            r[n].arrival = arr; r[n].qty = qty; r[n].filled = 0; n++;
        } else if (type == 'I') {
            incoming = qty; snprintf(tif, sizeof tif, "%s", t);
        }
    }
    fclose(f);

    long long total = 0;
    for (int i = 0; i < n; i++) total += r[i].qty;

    if (!fok_blocks(tif, incoming, total)) {
        qsort(r, n, sizeof r[0], cmp_select);    /* FIFO selection order */
        long long rem = incoming;
        for (int i = 0; i < n && rem > 0; i++) {
            long long take = r[i].qty < rem ? r[i].qty : rem;
            r[i].filled = take; rem -= take;
        }
    }
    qsort(r, n, sizeof r[0], cmp_arrival);       /* stable report order */
    for (int i = 0; i < n; i++)
        printf("%s %lld\n", r[i].id, r[i].filled);
    return 0;
}
