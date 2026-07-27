/************************************************************
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20191022  市場基盤部  初版作成
 ************************************************************/

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_ERR_IO  91
#define MIHFT_ERR_FMT 92
#define MIHFT_ERR_MEM 93

#define MIHFT_ACCEPT_CD 0
#define MIHFT_REJECT_NOTIONAL_CD 8

#define MIHFT_SEQ_SLOT_COUNT 131071u
#define MIHFT_LINE_SIZE 1024u
#define MIHFT_FIELD_COUNT 9u
#define MIHFT_GAP_LIMIT 1000LL

typedef struct {
    long long order_id;
    long long cif_no;
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    long long ord_qty;
    long long price_amt;
    int instr_tier;
} OrderRecord;

typedef struct {
    long long key;
    unsigned char used;
} OrderSlot;

static void trim_field(char *s)
{
    char *p = s;
    size_t n;

    while (*p != '\0' && isspace((unsigned char)*p)) {
        ++p;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1u);
    }

    n = strlen(s);
    while (n > 0u && isspace((unsigned char)s[n - 1u])) {
        s[--n] = '\0';
    }
}

static int split_csv_line(char *line, char *fields[], size_t max_fields, size_t *out_count)
{
    size_t count = 0u;
    char *p = line;

    while (count < max_fields) {
        fields[count++] = p;
        while (*p != '\0' && *p != ',' && *p != '\n' && *p != '\r') {
            ++p;
        }
        if (*p == ',') {
            *p++ = '\0';
            continue;
        }
        if (*p == '\n' || *p == '\r') {
            *p = '\0';
        }
        break;
    }

    *out_count = count;
    return count == max_fields ? 0 : -1;
}

static int parse_ll_field(const char *s, long long min_value, long long max_value, long long *out)
{
    char *end = NULL;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0' || v < min_value || v > max_value) {
        return -1;
    }

    *out = v;
    return 0;
}

static int parse_int_field(const char *s, int min_value, int max_value, int *out)
{
    long long v;

    if (parse_ll_field(s, min_value, max_value, &v) != 0) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int parse_order(char *line, OrderRecord *rec)
{
    char *fields[MIHFT_FIELD_COUNT];
    size_t count;
    size_t i;

    if (split_csv_line(line, fields, MIHFT_FIELD_COUNT, &count) != 0) {
        return -1;
    }

    for (i = 0u; i < count; ++i) {
        trim_field(fields[i]);
    }

    if (parse_ll_field(fields[0], 1LL, LLONG_MAX, &rec->order_id) != 0 ||
        parse_ll_field(fields[1], 1LL, LLONG_MAX, &rec->cif_no) != 0 ||
        parse_ll_field(fields[6], 1LL, LLONG_MAX, &rec->ord_qty) != 0 ||
        parse_ll_field(fields[7], 0LL, LLONG_MAX, &rec->price_amt) != 0 ||
        parse_int_field(fields[8], 1, 3, &rec->instr_tier) != 0) {
        return -1;
    }

    if (fields[2][0] == '\0' || strlen(fields[2]) >= sizeof(rec->instr_code)) {
        return -1;
    }
    memcpy(rec->instr_code, fields[2], strlen(fields[2]) + 1u);

    if ((fields[3][0] != 'B' && fields[3][0] != 'S') || fields[3][1] != '\0') {
        return -1;
    }
    rec->side_kbn = fields[3][0];

    if ((fields[4][0] != 'L' && fields[4][0] != 'M') || fields[4][1] != '\0') {
        return -1;
    }
    rec->ord_type = fields[4][0];

    if (strcmp(fields[5], "DAY") != 0 && strcmp(fields[5], "IOC") != 0 && strcmp(fields[5], "FOK") != 0) {
        return -1;
    }
    memcpy(rec->tif_code, fields[5], strlen(fields[5]) + 1u);

    return 0;
}

static int checked_notional(long long qty, long long price, long long *out)
{
    if (price != 0LL && qty > LLONG_MAX / price) {
        return -1;
    }

    *out = qty * price;
    return 0;
}

static unsigned long long hash_order_id(long long order_id)
{
    unsigned long long x = (unsigned long long)order_id;

    x ^= x >> 33;
    x *= 0xff51afd7ed558ccdULL;
    x ^= x >> 33;
    x *= 0xc4ceb9fe1a85ec53ULL;
    x ^= x >> 33;

    return x;
}

static int remember_order(OrderSlot *slots, long long order_id)
{
    size_t pos = (size_t)(hash_order_id(order_id) % MIHFT_SEQ_SLOT_COUNT);
    size_t i;

    for (i = 0u; i < MIHFT_SEQ_SLOT_COUNT; ++i) {
        size_t idx = (pos + i) % MIHFT_SEQ_SLOT_COUNT;

        if (!slots[idx].used) {
            slots[idx].used = 1u;
            slots[idx].key = order_id;
            return 0;
        }
        if (slots[idx].key == order_id) {
            return 1;
        }
    }

    return -1;
}

static void make_timestamp(char *buf, size_t size)
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_WIN32)
    localtime_s(&tmv, &now);
#else
    localtime_r(&now, &tmv);
#endif

    (void)strftime(buf, size, "%Y%m%d%H%M%S", &tmv);
}

static int write_decision(FILE *fp, long long decision_id, const OrderRecord *rec,
                          int decision_cd, const char *reason_cd, long long notional)
{
    char ts[32];

    make_timestamp(ts, sizeof(ts));
    if (fprintf(fp, "%lld,%lld,%lld,%s,%d,%s,%lld,%lld,%s\n",
                decision_id,
                rec->order_id,
                rec->cif_no,
                rec->instr_code,
                decision_cd,
                reason_cd,
                notional,
                0LL,
                ts) < 0) {
        return -1;
    }

    return 0;
}

static int write_reject(FILE *fp, long long reject_id, const OrderRecord *rec,
                        const char *reject_cd, const char *detail_cd)
{
    char ts[32];

    make_timestamp(ts, sizeof(ts));
    if (fprintf(fp, "%lld,%lld,%lld,%s,%s,%s,%s\n",
                reject_id,
                rec->order_id,
                rec->cif_no,
                rec->instr_code,
                reject_cd,
                detail_cd,
                ts) < 0) {
        return -1;
    }

    return 0;
}

int main(void)
{
    const char *in_path = getenv("SCORDF_PATH");
    const char *dec_path = getenv("HFDEC_PATH");
    const char *rjct_path = getenv("HFRJCT_PATH");
    FILE *in_fp;
    FILE *dec_fp;
    FILE *rjct_fp;
    OrderSlot *slots;
    char line[MIHFT_LINE_SIZE];
    long long last_order_id = 0LL;
    long long decision_id = 1LL;
    long long reject_id = 1LL;
    int final_cd = MIHFT_ACCEPT_CD;

    if (in_path == NULL || *in_path == '\0') {
        in_path = "SCORDF.csv";
    }
    if (dec_path == NULL || *dec_path == '\0') {
        dec_path = "HFDEC.dat";
    }
    if (rjct_path == NULL || *rjct_path == '\0') {
        rjct_path = "HFRJCT.dat";
    }

    slots = calloc(MIHFT_SEQ_SLOT_COUNT, sizeof(*slots));
    if (slots == NULL) {
        fputs("メモリ確保失敗\n", stderr);
        return MIHFT_ERR_MEM;
    }

    in_fp = fopen(in_path, "r");
    if (in_fp == NULL) {
        fputs("入力オープン失敗\n", stderr);
        free(slots);
        return MIHFT_ERR_IO;
    }

    dec_fp = fopen(dec_path, "w");
    if (dec_fp == NULL) {
        fputs("判定出力オープン失敗\n", stderr);
        fclose(in_fp);
        free(slots);
        return MIHFT_ERR_IO;
    }

    rjct_fp = fopen(rjct_path, "w");
    if (rjct_fp == NULL) {
        fputs("拒否出力オープン失敗\n", stderr);
        fclose(dec_fp);
        fclose(in_fp);
        free(slots);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), in_fp) != NULL) {
        OrderRecord rec;
        long long notional;
        int seen;

        if (parse_order(line, &rec) != 0) {
            fputs("入力形式不正\n", stderr);
            fclose(rjct_fp);
            fclose(dec_fp);
            fclose(in_fp);
            free(slots);
            return MIHFT_ERR_FMT;
        }

        if (checked_notional(rec.ord_qty, rec.price_amt, &notional) != 0) {
            notional = LLONG_MAX;
        }

        seen = remember_order(slots, rec.order_id);
        if (seen < 0) {
            fputs("注文番号表あふれ\n", stderr);
            fclose(rjct_fp);
            fclose(dec_fp);
            fclose(in_fp);
            free(slots);
            return MIHFT_ERR_MEM;
        }

        if (seen > 0) {
            if (write_reject(rjct_fp, reject_id++, &rec, "SEQ", "DUP") != 0 ||
                write_decision(dec_fp, decision_id++, &rec, MIHFT_REJECT_NOTIONAL_CD, "SEQ-DUP", notional) != 0) {
                fputs("出力失敗\n", stderr);
                fclose(rjct_fp);
                fclose(dec_fp);
                fclose(in_fp);
                free(slots);
                return MIHFT_ERR_IO;
            }
            final_cd = MIHFT_REJECT_NOTIONAL_CD;
            continue;
        }

        if (last_order_id != 0LL &&
            (rec.order_id <= last_order_id || rec.order_id - last_order_id > MIHFT_GAP_LIMIT)) {
            if (write_reject(rjct_fp, reject_id++, &rec, "SEQ", "GAP") != 0 ||
                write_decision(dec_fp, decision_id++, &rec, MIHFT_REJECT_NOTIONAL_CD, "SEQ-GAP", notional) != 0) {
                fputs("出力失敗\n", stderr);
                fclose(rjct_fp);
                fclose(dec_fp);
                fclose(in_fp);
                free(slots);
                return MIHFT_ERR_IO;
            }
            final_cd = MIHFT_REJECT_NOTIONAL_CD;
            last_order_id = rec.order_id;
            continue;
        }

        if (notional > MIHFT_MAX_NOTIONAL) {
            if (write_reject(rjct_fp, reject_id++, &rec, "RISK", "NOTIONAL") != 0 ||
                write_decision(dec_fp, decision_id++, &rec, MIHFT_REJECT_NOTIONAL_CD, "NOTIONAL", notional) != 0) {
                fputs("出力失敗\n", stderr);
                fclose(rjct_fp);
                fclose(dec_fp);
                fclose(in_fp);
                free(slots);
                return MIHFT_ERR_IO;
            }
            final_cd = MIHFT_REJECT_NOTIONAL_CD;
        } else {
            if (write_decision(dec_fp, decision_id++, &rec, MIHFT_ACCEPT_CD, "OK", notional) != 0) {
                fputs("出力失敗\n", stderr);
                fclose(rjct_fp);
                fclose(dec_fp);
                fclose(in_fp);
                free(slots);
                return MIHFT_ERR_IO;
            }
        }

        last_order_id = rec.order_id;
    }

    if (ferror(in_fp) || fflush(dec_fp) != 0 || fflush(rjct_fp) != 0) {
        fputs("入出力失敗\n", stderr);
        fclose(rjct_fp);
        fclose(dec_fp);
        fclose(in_fp);
        free(slots);
        return MIHFT_ERR_IO;
    }

    fclose(rjct_fp);
    fclose(dec_fp);
    fclose(in_fp);
    free(slots);

    return final_cd;
}
