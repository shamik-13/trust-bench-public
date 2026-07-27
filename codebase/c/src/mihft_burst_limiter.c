/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20230418  福田 亮太 (E-211)  初版作成
 */
#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

enum {
    CSV行長 = 512,
    鍵長 = 96,
    注文ID長 = 48,
    顧客番号長 = 32,
    銘柄コード長 = 32,
    区分長 = 8,
    拒否ID長 = 64,
    詳細長 = 32,
    時刻長 = 32,
    価格履歴上限 = 16,
    顧客状態上限 = 4096,
    窓ミリ秒 = 5,
    連射件数閾値 = 6,
    微小変更ティック閾値 = 2
};

typedef struct {
    char bucket_key[鍵長];
    int64_t window_ts;
    int64_t order_cnt;
    int64_t notional_amt;
    int64_t drop_cnt;
} HfrateRecord;

typedef struct {
    char order_id[注文ID長];
    char cif_no[顧客番号長];
    char instr_code[銘柄コード長];
    char side_kbn[区分長];
    char ord_type[区分長];
    char tif_code[区分長];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} ScordfRecord;

typedef struct {
    char cif_no[顧客番号長];
    char instr_code[銘柄コード長];
    char side_kbn[区分長];
    int64_t first_ts;
    int64_t last_ts;
    int count;
    int64_t prices[価格履歴上限];
    size_t price_pos;
    size_t price_len;
} CustomerState;

static int copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t n;

    if (dst_sz == 0) {
        return -1;
    }
    n = strlen(src);
    if (n >= dst_sz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static char *trim_field(char *s)
{
    char *e;

    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n') {
        s++;
    }
    e = s + strlen(s);
    while (e > s && (e[-1] == ' ' || e[-1] == '\t' || e[-1] == '\r' || e[-1] == '\n')) {
        *--e = '\0';
    }
    return s;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *trim_field(end) != '\0') {
        return -1;
    }
    *out = (int64_t)v;
    return 0;
}

static int parse_int(const char *s, int *out)
{
    int64_t v;

    if (parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int split_csv(char *line, char **fields, size_t need)
{
    size_t i = 0;
    char *p = line;

    while (i < need) {
        fields[i++] = trim_field(p);
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return i == need && strchr(fields[need - 1], ',') == NULL ? 0 : -1;
}

static int parse_hfrate_line(char *line, HfrateRecord *r)
{
    char *f[5];

    if (split_csv(line, f, 5) != 0) {
        return -1;
    }
    if (copy_field(r->bucket_key, sizeof(r->bucket_key), f[0]) != 0) {
        return -1;
    }
    if (parse_i64(f[1], &r->window_ts) != 0 ||
        parse_i64(f[2], &r->order_cnt) != 0 ||
        parse_i64(f[3], &r->notional_amt) != 0 ||
        parse_i64(f[4], &r->drop_cnt) != 0) {
        return -1;
    }
    return r->window_ts >= 0 && r->order_cnt >= 0 && r->notional_amt >= 0 && r->drop_cnt >= 0 ? 0 : -1;
}

static int parse_scordf_line(char *line, ScordfRecord *r)
{
    char *f[9];

    if (split_csv(line, f, 9) != 0) {
        return -1;
    }
    if (copy_field(r->order_id, sizeof(r->order_id), f[0]) != 0 ||
        copy_field(r->cif_no, sizeof(r->cif_no), f[1]) != 0 ||
        copy_field(r->instr_code, sizeof(r->instr_code), f[2]) != 0 ||
        copy_field(r->side_kbn, sizeof(r->side_kbn), f[3]) != 0 ||
        copy_field(r->ord_type, sizeof(r->ord_type), f[4]) != 0 ||
        copy_field(r->tif_code, sizeof(r->tif_code), f[5]) != 0) {
        return -1;
    }
    if (parse_i64(f[6], &r->ord_qty) != 0 ||
        parse_i64(f[7], &r->price_amt) != 0 ||
        parse_int(f[8], &r->instr_tier) != 0) {
        return -1;
    }
    return r->ord_qty > 0 && r->price_amt >= 0 ? 0 : -1;
}

static int tier_tick(int tier, int64_t *tick)
{
    if (tier == 1) {
        *tick = 100;
        return 0;
    }
    if (tier == 2) {
        *tick = 500;
        return 0;
    }
    if (tier == 3) {
        *tick = 1000;
        return 0;
    }
    return -1;
}

static int calc_notional(const ScordfRecord *o, int64_t *notional)
{
    if (o->price_amt != 0 && o->ord_qty > INT64_MAX / o->price_amt) {
        return -1;
    }
    *notional = o->ord_qty * o->price_amt;
    return 0;
}

static int valid_code_set(const ScordfRecord *o)
{
    if (strcmp(o->side_kbn, "B") != 0 && strcmp(o->side_kbn, "S") != 0) {
        return 0;
    }
    if (strcmp(o->ord_type, "L") != 0 && strcmp(o->ord_type, "M") != 0) {
        return 0;
    }
    if (strcmp(o->tif_code, "DAY") != 0 && strcmp(o->tif_code, "IOC") != 0 && strcmp(o->tif_code, "FOK") != 0) {
        return 0;
    }
    return 1;
}

static int same_stream(const CustomerState *s, const ScordfRecord *o)
{
    return strcmp(s->cif_no, o->cif_no) == 0 &&
           strcmp(s->instr_code, o->instr_code) == 0 &&
           strcmp(s->side_kbn, o->side_kbn) == 0;
}

static CustomerState *find_state(CustomerState *states, size_t *len, const ScordfRecord *o)
{
    size_t i;

    for (i = 0; i < *len; i++) {
        if (same_stream(&states[i], o)) {
            return &states[i];
        }
    }
    if (*len >= 顧客状態上限) {
        return NULL;
    }
    memset(&states[*len], 0, sizeof(states[*len]));
    if (copy_field(states[*len].cif_no, sizeof(states[*len].cif_no), o->cif_no) != 0 ||
        copy_field(states[*len].instr_code, sizeof(states[*len].instr_code), o->instr_code) != 0 ||
        copy_field(states[*len].side_kbn, sizeof(states[*len].side_kbn), o->side_kbn) != 0) {
        return NULL;
    }
    return &states[(*len)++];
}

static int count_small_price_moves(const CustomerState *s, int64_t price, int64_t tick)
{
    size_t i;
    int n = 0;
    int64_t limit = tick * 微小変更ティック閾値;

    for (i = 0; i < s->price_len; i++) {
        int64_t prev = s->prices[i];
        int64_t diff = prev >= price ? prev - price : price - prev;
        if (diff > 0 && diff <= limit) {
            n++;
        }
    }
    return n;
}

static int detect_burst(CustomerState *s, const ScordfRecord *o, int64_t ts)
{
    int64_t tick;

    if (tier_tick(o->instr_tier, &tick) != 0) {
        return 12;
    }
    if (s->count == 0 || ts - s->first_ts > 窓ミリ秒) {
        s->first_ts = ts;
        s->count = 0;
        s->price_len = 0;
        s->price_pos = 0;
    }
    s->last_ts = ts;
    s->count++;

    if (strcmp(o->ord_type, "L") == 0 &&
        s->count >= 連射件数閾値 &&
        count_small_price_moves(s, o->price_amt, tick) >= 連射件数閾値 - 2) {
        return 8;
    }

    s->prices[s->price_pos] = o->price_amt;
    s->price_pos = (s->price_pos + 1) % 価格履歴上限;
    if (s->price_len < 価格履歴上限) {
        s->price_len++;
    }
    return 0;
}

static void make_bucket_key(char *dst, size_t dst_sz, const ScordfRecord *o)
{
    snprintf(dst, dst_sz, "%s|%s|%s", o->cif_no, o->instr_code, o->side_kbn);
}

static int load_hfrate(HfrateRecord *rates, size_t cap, size_t *len)
{
    FILE *fp;
    char line[CSV行長];

    *len = 0;
    fp = fopen("HFRATE.csv", "r");
    if (fp == NULL) {
        return errno == ENOENT ? 0 : -1;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        if (*len >= cap || parse_hfrate_line(line, &rates[*len]) != 0) {
            fclose(fp);
            return -1;
        }
        (*len)++;
    }
    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }
    fclose(fp);
    return 0;
}

static HfrateRecord *find_rate(HfrateRecord *rates, size_t *len, size_t cap, const char *key, int64_t ts)
{
    size_t i;

    for (i = 0; i < *len; i++) {
        if (strcmp(rates[i].bucket_key, key) == 0) {
            return &rates[i];
        }
    }
    if (*len >= cap) {
        return NULL;
    }
    memset(&rates[*len], 0, sizeof(rates[*len]));
    if (copy_field(rates[*len].bucket_key, sizeof(rates[*len].bucket_key), key) != 0) {
        return NULL;
    }
    rates[*len].window_ts = ts;
    return &rates[(*len)++];
}

static int write_hfrate(const HfrateRecord *rates, size_t len)
{
    FILE *fp;
    size_t i;

    fp = fopen("HFRATE.out", "w");
    if (fp == NULL) {
        return -1;
    }
    for (i = 0; i < len; i++) {
        if (fprintf(fp, "%s,%lld,%lld,%lld,%lld\n",
                    rates[i].bucket_key,
                    (long long)rates[i].window_ts,
                    (long long)rates[i].order_cnt,
                    (long long)rates[i].notional_amt,
                    (long long)rates[i].drop_cnt) < 0) {
            fclose(fp);
            return -1;
        }
    }
    return fclose(fp) == 0 ? 0 : -1;
}

static int write_reject(FILE *fp, int64_t seq, const ScordfRecord *o, int reject_cd, const char *detail, int64_t ts)
{
    char reject_id[拒否ID長];

    snprintf(reject_id, sizeof(reject_id), "RJ%012lld", (long long)seq);
    return fprintf(fp, "%s,%s,%s,%s,%d,%s,%lld\n",
                   reject_id,
                   o->order_id,
                   o->cif_no,
                   o->instr_code,
                   reject_cd,
                   detail,
                   (long long)ts) < 0 ? -1 : 0;
}

static int process_orders(HfrateRecord *rates, size_t *rate_len, size_t rate_cap)
{
    FILE *in;
    FILE *rej;
    char line[CSV行長];
    CustomerState states[顧客状態上限];
    size_t state_len = 0;
    int64_t reject_seq = 0;
    int normal_code = 0;

    memset(states, 0, sizeof(states));

    in = fopen("SCORDF.csv", "r");
    if (in == NULL) {
        fprintf(stderr, "SCORDF読込失敗\n");
        return 20;
    }
    rej = fopen("HFRJCT.csv", "w");
    if (rej == NULL) {
        fclose(in);
        fprintf(stderr, "HFRJCT作成失敗\n");
        return 21;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        ScordfRecord o;
        CustomerState *s;
        HfrateRecord *r;
        char key[鍵長];
        int64_t notional;
        int decision;
        int64_t ts;

        if (parse_scordf_line(line, &o) != 0 || !valid_code_set(&o) || calc_notional(&o, &notional) != 0) {
            fclose(in);
            fclose(rej);
            fprintf(stderr, "注文レコード解析失敗\n");
            return 22;
        }

        ts = (int64_t)time(NULL) * 1000;
        make_bucket_key(key, sizeof(key), &o);
        r = find_rate(rates, rate_len, rate_cap, key, ts);
        s = find_state(states, &state_len, &o);
        if (r == NULL || s == NULL) {
            fclose(in);
            fclose(rej);
            fprintf(stderr, "バケット領域不足\n");
            return 23;
        }

        if (ts - r->window_ts > 窓ミリ秒) {
            r->window_ts = ts;
            r->order_cnt = 0;
            r->notional_amt = 0;
            r->drop_cnt = 0;
        }

        decision = 0;
        if (notional > MIHFT_MAX_NOTIONAL) {
            decision = 8;
        } else {
            decision = detect_burst(s, &o, ts);
        }

        if (decision == 0) {
            if (r->order_cnt == INT64_MAX || r->notional_amt > INT64_MAX - notional) {
                fclose(in);
                fclose(rej);
                fprintf(stderr, "集計値桁あふれ\n");
                return 24;
            }
            r->order_cnt++;
            r->notional_amt += notional;
        } else {
            if (r->drop_cnt == INT64_MAX) {
                fclose(in);
                fclose(rej);
                fprintf(stderr, "拒否件数桁あふれ\n");
                return 25;
            }
            r->drop_cnt++;
            reject_seq++;
            if (write_reject(rej, reject_seq, &o, decision, decision == 12 ? "TICK" : "BURST", ts) != 0) {
                fclose(in);
                fclose(rej);
                fprintf(stderr, "拒否レコード出力失敗\n");
                return 26;
            }
            normal_code = decision;
        }
    }

    if (ferror(in)) {
        fclose(in);
        fclose(rej);
        fprintf(stderr, "SCORDF読込中断\n");
        return 27;
    }
    if (fclose(in) != 0 || fclose(rej) != 0) {
        fprintf(stderr, "ファイル終端処理失敗\n");
        return 28;
    }
    return normal_code;
}

int main(void)
{
    HfrateRecord rates[顧客状態上限];
    size_t rate_len;
    int rc;

    if (load_hfrate(rates, 顧客状態上限, &rate_len) != 0) {
        fprintf(stderr, "HFRATE読込失敗\n");
        return 30;
    }

    rc = process_orders(rates, &rate_len, 顧客状態上限);
    if (rc >= 20) {
        return rc;
    }

    if (write_hfrate(rates, rate_len) != 0) {
        fprintf(stderr, "HFRATE出力失敗\n");
        return 31;
    }

    return rc;
}
