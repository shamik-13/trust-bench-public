/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210715  村上 健司 (E-301)      初版作成、板単位停止および銘柄単位緊急停止の事前判定を実装
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_RC_IOERR 20
#define MIHFT_RC_PARSEERR 21
#define MIHFT_MAX_LINE 1024
#define MIHFT_MAX_INST 4096
#define MIHFT_MAX_KILL 4096
#define MIHFT_MAX_TOKEN 16
#define MIHFT_REJECT_HALT 90
#define MIHFT_DETAIL_BOARD 11
#define MIHFT_DETAIL_INSTR 12

struct halt_instr_row {
    char instr_code[32];
    char instr_name[96];
    int tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[8];
};

struct halt_kill_row {
    char scope_key[64];
    char kill_flg;
    char reason_cd[16];
    char updated_ts[32];
    char updated_by[32];
};

struct halt_order_row {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[8];
    int64_t ord_qty;
    int64_t ord_price;
};

static void trim_field(char *s)
{
    size_t len;
    char *p = s;

    while (*p != '\0' && isspace((unsigned char)*p)) {
        ++p;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1U);
    }

    len = strlen(s);
    while (len > 0U && isspace((unsigned char)s[len - 1U])) {
        s[--len] = '\0';
    }

    if (len >= 2U && s[0] == '"' && s[len - 1U] == '"') {
        memmove(s, s + 1, len - 2U);
        s[len - 2U] = '\0';
    }
}

static int split_csv(char *line, char fields[][128], size_t cap)
{
    size_t n = 0U;
    char *p = line;

    while (*p != '\0' && *p != '\n' && *p != '\r') {
        size_t w = 0U;
        int quote = 0;

        if (n >= cap) {
            return -1;
        }

        while (*p != '\0') {
            if (*p == '"') {
                if (quote && p[1] == '"') {
                    if (w + 1U >= sizeof(fields[0])) {
                        return -1;
                    }
                    fields[n][w++] = '"';
                    p += 2;
                    continue;
                }
                quote = !quote;
                if (w + 1U >= sizeof(fields[0])) {
                    return -1;
                }
                fields[n][w++] = *p++;
                continue;
            }
            if (!quote && (*p == ',' || *p == '\n' || *p == '\r')) {
                break;
            }
            if (w + 1U >= sizeof(fields[0])) {
                return -1;
            }
            fields[n][w++] = *p++;
        }

        fields[n][w] = '\0';
        trim_field(fields[n]);
        ++n;

        if (*p == ',') {
            ++p;
            if (*p == '\0' || *p == '\n' || *p == '\r') {
                if (n >= cap) {
                    return -1;
                }
                fields[n][0] = '\0';
                ++n;
                break;
            }
        }
    }

    return (int)n;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
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

static int read_instr(const char *path, struct halt_instr_row *rows, size_t *count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];
    size_t n = 0U;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "入力を開けません:%s\n", path);
        return MIHFT_RC_IOERR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char f[8][128];
        int nf;

        if (line[0] == '\n' || line[0] == '\r') {
            continue;
        }

        nf = split_csv(line, f, 8U);
        if (nf != 6) {
            fprintf(stderr, "銘柄マスタ形式不正\n");
            fclose(fp);
            return MIHFT_RC_PARSEERR;
        }
        if (strcmp(f[0], "INSTR-CODE") == 0) {
            continue;
        }
        if (n >= MIHFT_MAX_INST) {
            fprintf(stderr, "銘柄マスタ件数超過\n");
            fclose(fp);
            return MIHFT_RC_PARSEERR;
        }

        if (parse_int(f[2], &rows[n].tier) != 0 ||
            parse_i64(f[3], &rows[n].tick_amt) != 0 ||
            parse_i64(f[4], &rows[n].lot_qty) != 0 ||
            rows[n].tier < 1 || rows[n].tier > 3 ||
            rows[n].tick_amt <= 0 || rows[n].lot_qty <= 0) {
            fprintf(stderr, "銘柄マスタ数値不正\n");
            fclose(fp);
            return MIHFT_RC_PARSEERR;
        }

        snprintf(rows[n].instr_code, sizeof(rows[n].instr_code), "%s", f[0]);
        snprintf(rows[n].instr_name, sizeof(rows[n].instr_name), "%s", f[1]);
        snprintf(rows[n].board_code, sizeof(rows[n].board_code), "%s", f[5]);

        if (strcmp(rows[n].board_code, "T1") != 0 &&
            strcmp(rows[n].board_code, "ST") != 0 &&
            strcmp(rows[n].board_code, "ETF") != 0) {
            fprintf(stderr, "板コード不正\n");
            fclose(fp);
            return MIHFT_RC_PARSEERR;
        }

        ++n;
    }

    if (ferror(fp)) {
        fprintf(stderr, "銘柄マスタ読込失敗\n");
        fclose(fp);
        return MIHFT_RC_IOERR;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static int read_kill(const char *path, struct halt_kill_row *rows, size_t *count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];
    size_t n = 0U;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "停止台帳を開けません:%s\n", path);
        return MIHFT_RC_IOERR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char f[8][128];
        int nf;

        if (line[0] == '\n' || line[0] == '\r') {
            continue;
        }

        nf = split_csv(line, f, 8U);
        if (nf != 5) {
            fprintf(stderr, "停止台帳形式不正\n");
            fclose(fp);
            return MIHFT_RC_PARSEERR;
        }
        if (strcmp(f[0], "SCOPE-KEY") == 0) {
            continue;
        }
        if (n >= MIHFT_MAX_KILL) {
            fprintf(stderr, "停止台帳件数超過\n");
            fclose(fp);
            return MIHFT_RC_PARSEERR;
        }
        if (strlen(f[1]) != 1U || (f[1][0] != '0' && f[1][0] != '1')) {
            fprintf(stderr, "停止フラグ不正\n");
            fclose(fp);
            return MIHFT_RC_PARSEERR;
        }

        snprintf(rows[n].scope_key, sizeof(rows[n].scope_key), "%s", f[0]);
        rows[n].kill_flg = f[1][0];
        snprintf(rows[n].reason_cd, sizeof(rows[n].reason_cd), "%s", f[2]);
        snprintf(rows[n].updated_ts, sizeof(rows[n].updated_ts), "%s", f[3]);
        snprintf(rows[n].updated_by, sizeof(rows[n].updated_by), "%s", f[4]);
        ++n;
    }

    if (ferror(fp)) {
        fprintf(stderr, "停止台帳読込失敗\n");
        fclose(fp);
        return MIHFT_RC_IOERR;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static const struct halt_instr_row *find_instr(const struct halt_instr_row *rows, size_t count, const char *code)
{
    size_t i;

    for (i = 0U; i < count; ++i) {
        if (strcmp(rows[i].instr_code, code) == 0) {
            return &rows[i];
        }
    }
    return NULL;
}

static int is_halted(const struct halt_kill_row *rows, size_t count, const char *key)
{
    size_t i;

    for (i = 0U; i < count; ++i) {
        if (rows[i].kill_flg == '1' && strcmp(rows[i].scope_key, key) == 0) {
            return 1;
        }
    }
    return 0;
}

static int parse_order(char *line, struct halt_order_row *order)
{
    char f[10][128];
    int nf;

    nf = split_csv(line, f, 10U);
    if (nf != 8) {
        return -1;
    }
    if (strcmp(f[0], "ORDER-ID") == 0) {
        return 1;
    }
    if (strlen(f[3]) != 1U || (f[3][0] != 'B' && f[3][0] != 'S')) {
        return -1;
    }
    if (strlen(f[4]) != 1U || (f[4][0] != 'L' && f[4][0] != 'M')) {
        return -1;
    }
    if (strcmp(f[5], "DAY") != 0 && strcmp(f[5], "IOC") != 0 && strcmp(f[5], "FOK") != 0) {
        return -1;
    }
    if (parse_i64(f[6], &order->ord_qty) != 0 ||
        parse_i64(f[7], &order->ord_price) != 0 ||
        order->ord_qty <= 0 || order->ord_price < 0) {
        return -1;
    }

    snprintf(order->order_id, sizeof(order->order_id), "%s", f[0]);
    snprintf(order->cif_no, sizeof(order->cif_no), "%s", f[1]);
    snprintf(order->instr_code, sizeof(order->instr_code), "%s", f[2]);
    order->side_kbn = f[3][0];
    order->ord_type = f[4][0];
    snprintf(order->tif_code, sizeof(order->tif_code), "%s", f[5]);

    return 0;
}

static int make_reject_id(char *buf, size_t bufsz, unsigned long seq)
{
    int n = snprintf(buf, bufsz, "RJ%012lu", seq);
    return (n > 0 && (size_t)n < bufsz) ? 0 : -1;
}

static void now_yyyymmddhhmmss(char *buf, size_t bufsz)
{
    time_t t = time(NULL);
    struct tm tmv;

#if defined(_POSIX_VERSION)
    localtime_r(&t, &tmv);
#else
    {
        struct tm *p = localtime(&t);
        if (p != NULL) {
            tmv = *p;
        } else {
            memset(&tmv, 0, sizeof(tmv));
        }
    }
#endif

    snprintf(buf, bufsz, "%04d%02d%02d%02d%02d%02d",
             tmv.tm_year + 1900, tmv.tm_mon + 1, tmv.tm_mday,
             tmv.tm_hour, tmv.tm_min, tmv.tm_sec);
}

static int write_reject(FILE *out, unsigned long seq, const struct halt_order_row *order, int detail_cd)
{
    char reject_id[32];
    char ts[32];

    if (make_reject_id(reject_id, sizeof(reject_id), seq) != 0) {
        fprintf(stderr, "拒否番号生成失敗\n");
        return MIHFT_RC_PARSEERR;
    }

    now_yyyymmddhhmmss(ts, sizeof(ts));

    if (fprintf(out, "%s,%s,%s,%s,%d,%d,%s\n",
                reject_id,
                order->order_id,
                order->cif_no,
                order->instr_code,
                MIHFT_REJECT_HALT,
                detail_cd,
                ts) < 0) {
        fprintf(stderr, "拒否出力失敗\n");
        return MIHFT_RC_IOERR;
    }

    return 0;
}

static int check_notional_ready(const struct halt_order_row *order, const struct halt_instr_row *instr)
{
    int64_t units;
    int64_t notional;

    if (order->ord_qty > INT64_MAX / instr->lot_qty) {
        return 8;
    }
    units = order->ord_qty * instr->lot_qty;

    if (order->ord_type == 'M') {
        return 0;
    }

    if (order->ord_price > INT64_MAX / units) {
        return 8;
    }
    notional = order->ord_price * units;

    if (notional > MIHFT_MAX_NOTIONAL) {
        return 8;
    }
    if ((order->ord_price % instr->tick_amt) != 0) {
        return 12;
    }

    return 0;
}

int main(void)
{
    struct halt_instr_row instrs[MIHFT_MAX_INST];
    struct halt_kill_row kills[MIHFT_MAX_KILL];
    size_t instr_count = 0U;
    size_t kill_count = 0U;
    FILE *orders;
    FILE *rejects;
    char line[MIHFT_MAX_LINE];
    unsigned long reject_seq = 1UL;
    int final_decision = 0;
    int rc;

    rc = read_instr("SCINSTF.csv", instrs, &instr_count);
    if (rc != 0) {
        return rc;
    }

    rc = read_kill("HFKILL.csv", kills, &kill_count);
    if (rc != 0) {
        return rc;
    }

    orders = fopen("HFORDR.csv", "r");
    if (orders == NULL) {
        fprintf(stderr, "注文入力を開けません:HFORDR.csv\n");
        return MIHFT_RC_IOERR;
    }

    rejects = fopen("HFRJCT.csv", "w");
    if (rejects == NULL) {
        fprintf(stderr, "拒否出力を開けません:HFRJCT.csv\n");
        fclose(orders);
        return MIHFT_RC_IOERR;
    }

    if (fprintf(rejects, "REJECT-ID,ORDER-ID,CIF-NO,INSTR-CODE,REJECT-CD,DETAIL-CD,REJECT-TS\n") < 0) {
        fprintf(stderr, "拒否見出し出力失敗\n");
        fclose(rejects);
        fclose(orders);
        return MIHFT_RC_IOERR;
    }

    while (fgets(line, sizeof(line), orders) != NULL) {
        struct halt_order_row order;
        const struct halt_instr_row *instr;
        int parsed;
        int decision;

        if (line[0] == '\n' || line[0] == '\r') {
            continue;
        }

        parsed = parse_order(line, &order);
        if (parsed > 0) {
            continue;
        }
        if (parsed < 0) {
            fprintf(stderr, "注文入力形式不正\n");
            fclose(rejects);
            fclose(orders);
            return MIHFT_RC_PARSEERR;
        }

        instr = find_instr(instrs, instr_count, order.instr_code);
        if (instr == NULL) {
            fprintf(stderr, "銘柄未登録\n");
            fclose(rejects);
            fclose(orders);
            return MIHFT_RC_PARSEERR;
        }

        if (is_halted(kills, kill_count, instr->board_code)) {
            rc = write_reject(rejects, reject_seq++, &order, MIHFT_DETAIL_BOARD);
            if (rc != 0) {
                fclose(rejects);
                fclose(orders);
                return rc;
            }
            final_decision = 8;
            continue;
        }

        if (is_halted(kills, kill_count, order.instr_code)) {
            rc = write_reject(rejects, reject_seq++, &order, MIHFT_DETAIL_INSTR);
            if (rc != 0) {
                fclose(rejects);
                fclose(orders);
                return rc;
            }
            final_decision = 8;
            continue;
        }

        decision = check_notional_ready(&order, instr);
        if (decision != 0 && final_decision == 0) {
            final_decision = decision;
        }
    }

    if (ferror(orders)) {
        fprintf(stderr, "注文入力読込失敗\n");
        fclose(rejects);
        fclose(orders);
        return MIHFT_RC_IOERR;
    }

    if (fclose(rejects) != 0) {
        fprintf(stderr, "拒否出力終結失敗\n");
        fclose(orders);
        return MIHFT_RC_IOERR;
    }
    if (fclose(orders) != 0) {
        fprintf(stderr, "注文入力終結失敗\n");
        return MIHFT_RC_IOERR;
    }

    return final_decision;
}
