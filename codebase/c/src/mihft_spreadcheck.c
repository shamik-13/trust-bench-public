/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20190416  市場基盤部  スプレッド異常検知の初版作成
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_INSTR_CODE_LEN 32
#define MIHFT_INSTR_NAME_LEN 96
#define MIHFT_BOARD_CODE_LEN 8
#define MIHFT_TS_LEN 32
#define MIHFT_LINE_LEN 512
#define MIHFT_DECISION_TS "20250115090000000"
#define MIHFT_REASON_SPREAD "SPREAD-TICK"
#define MIHFT_REASON_PARSE "PARSE"
#define MIHFT_ACTION_MONITOR 12
#define MIHFT_ACTION_ACCEPT 0
#define MIHFT_HARD_ERROR 16

typedef struct {
    char instr_code[MIHFT_INSTR_CODE_LEN];
    int64_t bid_amt;
    int64_t ask_amt;
    int64_t mid_amt;
    int64_t spread_amt;
    char quote_ts[MIHFT_TS_LEN];
} mihft_quote_rec;

typedef struct {
    char instr_code[MIHFT_INSTR_CODE_LEN];
    char instr_name[MIHFT_INSTR_NAME_LEN];
    int tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[MIHFT_BOARD_CODE_LEN];
} mihft_inst_rec;

typedef struct {
    mihft_inst_rec rows[256];
    size_t count;
} mihft_inst_table;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);
    if (dstsz == 0U || n >= dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1U);
    return 0;
}

static int mihft_next_field(char **cursor, char *dst, size_t dstsz)
{
    char *start;
    char *comma;

    if (*cursor == NULL) {
        return -1;
    }

    start = *cursor;
    comma = strchr(start, ',');
    if (comma != NULL) {
        *comma = '\0';
        *cursor = comma + 1;
    } else {
        *cursor = NULL;
    }

    return mihft_copy_field(dst, dstsz, start);
}

static int mihft_parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s[0] == '\0') {
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

static int mihft_parse_int(const char *s, int *out)
{
    int64_t v;

    if (mihft_parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int mihft_tier_tick(int tier, int64_t *tick_amt)
{
    if (tier == 1) {
        *tick_amt = 100;
        return 0;
    }
    if (tier == 2) {
        *tick_amt = 500;
        return 0;
    }
    if (tier == 3) {
        *tick_amt = 1000;
        return 0;
    }
    return -1;
}

static int mihft_spread_limit(int tier, int64_t tick_amt, int64_t *limit)
{
    int64_t mult;

    if (tick_amt <= 0) {
        return -1;
    }

    if (tier == 1) {
        mult = 3;
    } else if (tier == 2) {
        mult = 4;
    } else if (tier == 3) {
        mult = 6;
    } else {
        return -1;
    }

    if (tick_amt > INT64_MAX / mult) {
        return -1;
    }

    *limit = tick_amt * mult;
    return 0;
}

static int mihft_parse_inst(char *line, mihft_inst_rec *rec)
{
    char *cur = line;
    char buf[64];
    int64_t canonical_tick;

    if (mihft_next_field(&cur, rec->instr_code, sizeof rec->instr_code) != 0 ||
        mihft_next_field(&cur, rec->instr_name, sizeof rec->instr_name) != 0 ||
        mihft_next_field(&cur, buf, sizeof buf) != 0 ||
        mihft_parse_int(buf, &rec->tier) != 0 ||
        mihft_next_field(&cur, buf, sizeof buf) != 0 ||
        mihft_parse_i64(buf, &rec->tick_amt) != 0 ||
        mihft_next_field(&cur, buf, sizeof buf) != 0 ||
        mihft_parse_i64(buf, &rec->lot_qty) != 0 ||
        mihft_next_field(&cur, rec->board_code, sizeof rec->board_code) != 0 ||
        cur != NULL) {
        return -1;
    }

    if (mihft_tier_tick(rec->tier, &canonical_tick) != 0 ||
        rec->tick_amt != canonical_tick ||
        rec->lot_qty <= 0) {
        return -1;
    }

    return 0;
}

static int mihft_parse_quote(char *line, mihft_quote_rec *rec)
{
    char *cur = line;
    char buf[64];

    if (mihft_next_field(&cur, rec->instr_code, sizeof rec->instr_code) != 0 ||
        mihft_next_field(&cur, buf, sizeof buf) != 0 ||
        mihft_parse_i64(buf, &rec->bid_amt) != 0 ||
        mihft_next_field(&cur, buf, sizeof buf) != 0 ||
        mihft_parse_i64(buf, &rec->ask_amt) != 0 ||
        mihft_next_field(&cur, buf, sizeof buf) != 0 ||
        mihft_parse_i64(buf, &rec->mid_amt) != 0 ||
        mihft_next_field(&cur, buf, sizeof buf) != 0 ||
        mihft_parse_i64(buf, &rec->spread_amt) != 0 ||
        mihft_next_field(&cur, rec->quote_ts, sizeof rec->quote_ts) != 0 ||
        cur != NULL) {
        return -1;
    }

    if (rec->bid_amt <= 0 ||
        rec->ask_amt <= 0 ||
        rec->ask_amt < rec->bid_amt ||
        rec->mid_amt <= 0 ||
        rec->spread_amt < 0 ||
        rec->ask_amt - rec->bid_amt != rec->spread_amt) {
        return -1;
    }

    return 0;
}

static const mihft_inst_rec *mihft_find_inst(const mihft_inst_table *table, const char *instr_code)
{
    size_t i;

    for (i = 0U; i < table->count; i++) {
        if (strcmp(table->rows[i].instr_code, instr_code) == 0) {
            return &table->rows[i];
        }
    }

    return NULL;
}

static int mihft_load_inst(mihft_inst_table *table)
{
    FILE *fp;
    char line[MIHFT_LINE_LEN];

    table->count = 0U;
    fp = fopen("SCINSTF.csv", "r");
    if (fp == NULL) {
        fprintf(stderr, "SCINSTFをオープンできません\n");
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }
        if (table->count >= sizeof table->rows / sizeof table->rows[0]) {
            fclose(fp);
            fprintf(stderr, "SCINSTFの件数が上限を超過しました\n");
            return -1;
        }
        if (mihft_parse_inst(line, &table->rows[table->count]) != 0) {
            fclose(fp);
            fprintf(stderr, "SCINSTFの形式が不正です\n");
            return -1;
        }
        table->count++;
    }

    if (ferror(fp) != 0) {
        fclose(fp);
        fprintf(stderr, "SCINSTFの読込に失敗しました\n");
        return -1;
    }

    fclose(fp);
    return 0;
}

static int mihft_write_decision(FILE *out, uint64_t seq, const mihft_quote_rec *q, int action, const char *reason)
{
    if (fprintf(out,
                "D%012llu,O%012llu,%s,%d,%s,%s\n",
                (unsigned long long)seq,
                (unsigned long long)seq,
                q->instr_code,
                action,
                reason,
                MIHFT_DECISION_TS) < 0) {
        return -1;
    }

    return 0;
}

int main(void)
{
    mihft_inst_table insts;
    FILE *qfp;
    FILE *lfp;
    char line[MIHFT_LINE_LEN];
    uint64_t seq = 1U;
    int final_code = MIHFT_ACTION_ACCEPT;

    if (mihft_load_inst(&insts) != 0) {
        return MIHFT_HARD_ERROR;
    }

    qfp = fopen("HFQUOTF.csv", "r");
    if (qfp == NULL) {
        fprintf(stderr, "HFQUOTFをオープンできません\n");
        return MIHFT_HARD_ERROR;
    }

    lfp = fopen("HFDECLOG.csv", "w");
    if (lfp == NULL) {
        fclose(qfp);
        fprintf(stderr, "HFDECLOGをオープンできません\n");
        return MIHFT_HARD_ERROR;
    }

    while (fgets(line, sizeof line, qfp) != NULL) {
        mihft_quote_rec q;
        const mihft_inst_rec *inst;
        int64_t limit;

        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }

        if (mihft_parse_quote(line, &q) != 0) {
            fprintf(stderr, "HFQUOTFの形式が不正です\n");
            fclose(lfp);
            fclose(qfp);
            return MIHFT_HARD_ERROR;
        }

        inst = mihft_find_inst(&insts, q.instr_code);
        if (inst == NULL) {
            fprintf(stderr, "銘柄マスタが未登録です\n");
            fclose(lfp);
            fclose(qfp);
            return MIHFT_HARD_ERROR;
        }

        if (mihft_spread_limit(inst->tier, inst->tick_amt, &limit) != 0) {
            fprintf(stderr, "スプレッド閾値を算出できません\n");
            fclose(lfp);
            fclose(qfp);
            return MIHFT_HARD_ERROR;
        }

        if (q.spread_amt > limit) {
            if (mihft_write_decision(lfp, seq, &q, MIHFT_ACTION_MONITOR, MIHFT_REASON_SPREAD) != 0) {
                fprintf(stderr, "HFDECLOGの書込に失敗しました\n");
                fclose(lfp);
                fclose(qfp);
                return MIHFT_HARD_ERROR;
            }
            final_code = MIHFT_ACTION_MONITOR;
        } else {
            if (mihft_write_decision(lfp, seq, &q, MIHFT_ACTION_ACCEPT, "OK") != 0) {
                fprintf(stderr, "HFDECLOGの書込に失敗しました\n");
                fclose(lfp);
                fclose(qfp);
                return MIHFT_HARD_ERROR;
            }
        }

        if (seq == UINT64_MAX) {
            fprintf(stderr, "決定番号が上限を超過しました\n");
            fclose(lfp);
            fclose(qfp);
            return MIHFT_HARD_ERROR;
        }
        seq++;
    }

    if (ferror(qfp) != 0) {
        fprintf(stderr, "HFQUOTFの読込に失敗しました\n");
        fclose(lfp);
        fclose(qfp);
        return MIHFT_HARD_ERROR;
    }

    if (fclose(lfp) != 0) {
        fclose(qfp);
        fprintf(stderr, "HFDECLOGのクローズに失敗しました\n");
        return MIHFT_HARD_ERROR;
    }

    fclose(qfp);
    return final_code;
}
