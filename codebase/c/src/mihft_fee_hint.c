#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_DECISION_ACCEPT_CD 0
#define MIHFT_DECISION_REJECT_TICK_CD 1
#define MIHFT_DECISION_REJECT_NOTIONAL_CD 2
#define MIHFT_DECISION_REJECT_MARGIN_CD 3

#define MIHFT_SCFEEF_PATH  "SCFEEF.csv"
#define MIHFT_SCINSTF_PATH "SCINSTF.csv"
#define MIHFT_SCORDF_PATH  "SCORDF.csv"
#define MIHFT_HFDEC_PATH   "HFDEC.csv"

#define MIHFT_MAX_LINE 1024
#define MIHFT_MAX_FEE_ROWS 64
#define MIHFT_MAX_INST_ROWS 4096
#define MIHFT_SCALE_X100 100LL
#define MIHFT_BP_DENOM 10000LL

struct fee_row {
    char board_code[8];
    int64_t fee_rate_bp;
    int64_t min_fee_amt;
};

struct inst_row {
    char instr_code[32];
    char instr_name[128];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[8];
};

struct ord_row {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn[2];
    char ord_type[2];
    char tif_code[4];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
};

static void trim_crlf(char *s)
{
    size_t n = strlen(s);

    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n;

    if (dstsz == 0U) {
        return -1;
    }

    n = strlen(src);
    if (n >= dstsz) {
        return -1;
    }

    memcpy(dst, src, n + 1U);
    return 0;
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

static int split_csv(char *line, char **cols, size_t need)
{
    size_t i = 0U;
    char *p = line;

    while (i < need) {
        cols[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    return i == need && strchr(cols[need - 1U], ',') == NULL ? 0 : -1;
}

static int checked_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a < 0 || b < 0) {
        return -1;
    }
    if (a != 0 && b > INT64_MAX / a) {
        return -1;
    }

    *out = a * b;
    return 0;
}

static int checked_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }

    *out = a + b;
    return 0;
}

static int div_round_up_i64(int64_t num, int64_t den, int64_t *out)
{
    int64_t q;
    int64_t r;

    if (den <= 0 || num < 0) {
        return -1;
    }

    q = num / den;
    r = num % den;
    if (r != 0 && q == INT64_MAX) {
        return -1;
    }

    *out = q + (r != 0 ? 1 : 0);
    return 0;
}

static int tier_margin_bp(int tier, int64_t *rate_bp)
{
    if (tier == 1) {
        *rate_bp = 1000;
        return 0;
    }
    if (tier == 2) {
        *rate_bp = 2000;
        return 0;
    }
    if (tier == 3) {
        *rate_bp = 4000;
        return 0;
    }

    return -1;
}

static int valid_side(const char *v)
{
    return strcmp(v, "B") == 0 || strcmp(v, "S") == 0;
}

static int valid_ord_type(const char *v)
{
    return strcmp(v, "L") == 0 || strcmp(v, "M") == 0;
}

static int valid_tif(const char *v)
{
    return strcmp(v, "DAY") == 0 || strcmp(v, "IOC") == 0 || strcmp(v, "FOK") == 0;
}

static int valid_board(const char *v)
{
    return strcmp(v, "T1") == 0 || strcmp(v, "ST") == 0 || strcmp(v, "ETF") == 0;
}

static int load_scfeef(struct fee_row *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen(MIHFT_SCFEEF_PATH, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0U;

    if (fp == NULL) {
        fprintf(stderr, "SCFEEFオープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *cols[3];
        int64_t fee_rate;
        int64_t min_fee;

        trim_crlf(line);
        if (line[0] == '\0' || strncmp(line, "BOARD-CODE,", 11U) == 0) {
            continue;
        }

        if (n >= cap || split_csv(line, cols, 3U) != 0) {
            fprintf(stderr, "SCFEEF形式不正\n");
            fclose(fp);
            return -1;
        }

        if (!valid_board(cols[0]) || parse_i64(cols[1], &fee_rate) != 0 ||
            parse_i64(cols[2], &min_fee) != 0 || fee_rate < 0 || min_fee < 0) {
            fprintf(stderr, "SCFEEF値不正\n");
            fclose(fp);
            return -1;
        }

        if (copy_field(rows[n].board_code, sizeof rows[n].board_code, cols[0]) != 0) {
            fprintf(stderr, "SCFEEF桁数超過\n");
            fclose(fp);
            return -1;
        }

        rows[n].fee_rate_bp = fee_rate;
        rows[n].min_fee_amt = min_fee;
        n++;
    }

    if (ferror(fp) || fclose(fp) != 0) {
        fprintf(stderr, "SCFEEF読込失敗\n");
        return -1;
    }

    *count = n;
    return n == 0U ? -1 : 0;
}

static int load_scinstf(struct inst_row *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen(MIHFT_SCINSTF_PATH, "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0U;

    if (fp == NULL) {
        fprintf(stderr, "SCINSTFオープン失敗\n");
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *cols[6];
        int tier;
        int64_t tick;
        int64_t lot;
        int64_t margin_bp;

        trim_crlf(line);
        if (line[0] == '\0' || strncmp(line, "INSTR-CODE,", 11U) == 0) {
            continue;
        }

        if (n >= cap || split_csv(line, cols, 6U) != 0) {
            fprintf(stderr, "SCINSTF形式不正\n");
            fclose(fp);
            return -1;
        }

        if (parse_int(cols[2], &tier) != 0 || parse_i64(cols[3], &tick) != 0 ||
            parse_i64(cols[4], &lot) != 0 || tier_margin_bp(tier, &margin_bp) != 0 ||
            tick <= 0 || lot <= 0 || !valid_board(cols[5])) {
            fprintf(stderr, "SCINSTF値不正\n");
            fclose(fp);
            return -1;
        }

        if (copy_field(rows[n].instr_code, sizeof rows[n].instr_code, cols[0]) != 0 ||
            copy_field(rows[n].instr_name, sizeof rows[n].instr_name, cols[1]) != 0 ||
            copy_field(rows[n].board_code, sizeof rows[n].board_code, cols[5]) != 0) {
            fprintf(stderr, "SCINSTF桁数超過\n");
            fclose(fp);
            return -1;
        }

        rows[n].instr_tier = tier;
        rows[n].tick_amt = tick;
        rows[n].lot_qty = lot;
        n++;
    }

    if (ferror(fp) || fclose(fp) != 0) {
        fprintf(stderr, "SCINSTF読込失敗\n");
        return -1;
    }

    *count = n;
    return n == 0U ? -1 : 0;
}

static const struct fee_row *find_fee(const struct fee_row *rows, size_t count, const char *board)
{
    size_t i;

    for (i = 0U; i < count; i++) {
        if (strcmp(rows[i].board_code, board) == 0) {
            return &rows[i];
        }
    }

    return NULL;
}

static const struct inst_row *find_inst(const struct inst_row *rows, size_t count, const char *instr)
{
    size_t i;

    for (i = 0U; i < count; i++) {
        if (strcmp(rows[i].instr_code, instr) == 0) {
            return &rows[i];
        }
    }

    return NULL;
}

static int parse_order(char *line, struct ord_row *ord)
{
    char *cols[9];

    if (split_csv(line, cols, 9U) != 0) {
        return -1;
    }

    if (copy_field(ord->order_id, sizeof ord->order_id, cols[0]) != 0 ||
        copy_field(ord->cif_no, sizeof ord->cif_no, cols[1]) != 0 ||
        copy_field(ord->instr_code, sizeof ord->instr_code, cols[2]) != 0 ||
        copy_field(ord->side_kbn, sizeof ord->side_kbn, cols[3]) != 0 ||
        copy_field(ord->ord_type, sizeof ord->ord_type, cols[4]) != 0 ||
        copy_field(ord->tif_code, sizeof ord->tif_code, cols[5]) != 0) {
        return -1;
    }

    if (!valid_side(ord->side_kbn) || !valid_ord_type(ord->ord_type) ||
        !valid_tif(ord->tif_code)) {
        return -1;
    }

    if (parse_i64(cols[6], &ord->ord_qty) != 0 || parse_i64(cols[7], &ord->price_amt) != 0 ||
        parse_int(cols[8], &ord->instr_tier) != 0) {
        return -1;
    }

    return ord->ord_qty > 0 && ord->price_amt > 0 ? 0 : -1;
}

static int now_ts(char *buf, size_t bufsz)
{
    time_t t = time(NULL);
    struct tm *tmv;

    if (t == (time_t)-1) {
        return -1;
    }

    tmv = localtime(&t);
    if (tmv == NULL) {
        return -1;
    }

    return strftime(buf, bufsz, "%Y%m%d%H%M%S", tmv) == 0U ? -1 : 0;
}

static int write_decision(FILE *out, int64_t seq, const struct ord_row *ord, int decision_cd,
                          const char *reason_cd, int64_t notional_x100, int64_t fee_hint_x100)
{
    char ts[16];

    if (now_ts(ts, sizeof ts) != 0) {
        fprintf(stderr, "時刻取得失敗\n");
        return -1;
    }

    if (fprintf(out, "D%012" PRId64 ",%s,%s,%s,%d,%s,%" PRId64 ",%" PRId64 ",%s\n",
                seq, ord->order_id, ord->cif_no, ord->instr_code, decision_cd,
                reason_cd, notional_x100, fee_hint_x100, ts) < 0) {
        fprintf(stderr, "HFDEC書込失敗\n");
        return -1;
    }

    return 0;
}

static int judge_order(const struct ord_row *ord, const struct inst_row *inst,
                       const struct fee_row *fee, int64_t *notional_x100,
                       int64_t *fee_hint_x100, const char **reason_cd)
{
    int64_t notional;
    int64_t scaled_notional;
    int64_t fee_num;
    int64_t calc_fee;
    int64_t min_fee_x100;
    int64_t margin_bp;

    if (inst == NULL || fee == NULL || ord->instr_tier != inst->instr_tier) {
        *notional_x100 = 0;
        *fee_hint_x100 = 0;
        *reason_cd = "MST";
        return MIHFT_DECISION_REJECT_TICK_CD;
    }

    if (checked_mul_i64(ord->ord_qty, ord->price_amt, &notional) != 0 ||
        checked_mul_i64(notional, MIHFT_SCALE_X100, &scaled_notional) != 0) {
        *notional_x100 = INT64_MAX;
        *fee_hint_x100 = 0;
        *reason_cd = "OVF";
        return MIHFT_DECISION_REJECT_NOTIONAL_CD;
    }

    *notional_x100 = scaled_notional;

    if (notional > MIHFT_MAX_NOTIONAL) {
        *fee_hint_x100 = 0;
        *reason_cd = "NOTIONAL";
        return MIHFT_DECISION_REJECT_NOTIONAL_CD;
    }

    if (ord->price_amt % inst->tick_amt != 0 || ord->ord_qty % inst->lot_qty != 0) {
        *fee_hint_x100 = 0;
        *reason_cd = "TICK";
        return MIHFT_DECISION_REJECT_TICK_CD;
    }

    if (tier_margin_bp(inst->instr_tier, &margin_bp) != 0) {
        *fee_hint_x100 = 0;
        *reason_cd = "TIER";
        return MIHFT_DECISION_REJECT_TICK_CD;
    }

    if (checked_mul_i64(scaled_notional, fee->fee_rate_bp, &fee_num) != 0 ||
        div_round_up_i64(fee_num, MIHFT_BP_DENOM, &calc_fee) != 0 ||
        checked_mul_i64(fee->min_fee_amt, MIHFT_SCALE_X100, &min_fee_x100) != 0) {
        *fee_hint_x100 = 0;
        *reason_cd = "FEE";
        return MIHFT_DECISION_REJECT_NOTIONAL_CD;
    }

    *fee_hint_x100 = calc_fee < min_fee_x100 ? min_fee_x100 : calc_fee;

    if (checked_mul_i64(scaled_notional, margin_bp, &fee_num) != 0 ||
        div_round_up_i64(fee_num, MIHFT_BP_DENOM, &calc_fee) != 0 ||
        checked_add_i64(calc_fee, *fee_hint_x100, &calc_fee) != 0 ||
        calc_fee > scaled_notional) {
        *reason_cd = "MARGIN";
        return MIHFT_DECISION_REJECT_MARGIN_CD;
    }

    *reason_cd = "OK";
    return MIHFT_DECISION_ACCEPT_CD;
}

int main(void)
{
    struct fee_row fees[MIHFT_MAX_FEE_ROWS];
    struct inst_row insts[MIHFT_MAX_INST_ROWS];
    size_t fee_count = 0U;
    size_t inst_count = 0U;
    FILE *in;
    FILE *out;
    char line[MIHFT_MAX_LINE];
    int final_decision = MIHFT_DECISION_ACCEPT_CD;
    int64_t seq = 1;

    if (load_scfeef(fees, MIHFT_MAX_FEE_ROWS, &fee_count) != 0 ||
        load_scinstf(insts, MIHFT_MAX_INST_ROWS, &inst_count) != 0) {
        return 20;
    }

    in = fopen(MIHFT_SCORDF_PATH, "r");
    if (in == NULL) {
        fprintf(stderr, "SCORDFオープン失敗\n");
        return 21;
    }

    out = fopen(MIHFT_HFDEC_PATH, "w");
    if (out == NULL) {
        fprintf(stderr, "HFDECオープン失敗\n");
        fclose(in);
        return 22;
    }

    if (fprintf(out, "DECISION-ID,ORDER-ID,CIF-NO,INSTR-CODE,DECISION-CD,REASON-CD,NOTIONAL-AMT,LIMIT-USED-AMT,DECISION-TS\n") < 0) {
        fprintf(stderr, "HFDEC見出し書込失敗\n");
        fclose(out);
        fclose(in);
        return 23;
    }

    while (fgets(line, sizeof line, in) != NULL) {
        struct ord_row ord;
        const struct inst_row *inst;
        const struct fee_row *fee;
        const char *reason_cd;
        int64_t notional_x100;
        int64_t fee_hint_x100;
        int decision_cd;

        trim_crlf(line);
        if (line[0] == '\0' || strncmp(line, "ORDER-ID,", 9U) == 0) {
            continue;
        }

        if (parse_order(line, &ord) != 0) {
            fprintf(stderr, "SCORDF形式不正\n");
            fclose(out);
            fclose(in);
            return 24;
        }

        inst = find_inst(insts, inst_count, ord.instr_code);
        fee = inst == NULL ? NULL : find_fee(fees, fee_count, inst->board_code);
        decision_cd = judge_order(&ord, inst, fee, &notional_x100, &fee_hint_x100, &reason_cd);

        if (write_decision(out, seq, &ord, decision_cd, reason_cd, notional_x100,
                           fee_hint_x100) != 0) {
            fclose(out);
            fclose(in);
            return 25;
        }

        if (decision_cd > final_decision) {
            final_decision = decision_cd;
        }
        seq++;
    }

    if (ferror(in)) {
        fprintf(stderr, "SCORDF読込失敗\n");
        fclose(out);
        fclose(in);
        return 26;
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "HFDECクローズ失敗\n");
        fclose(in);
        return 27;
    }

    if (fclose(in) != 0) {
        fprintf(stderr, "SCORDFクローズ失敗\n");
        return 28;
    }

    return final_decision;
}
