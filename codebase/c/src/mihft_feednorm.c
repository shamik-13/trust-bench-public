/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220906  今井 彩 (E-230)  フィード正規化処理の初版作成
 */
#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_FEED_ACCEPT        0
#define MIHFT_FEED_ERR_IO       16
#define MIHFT_FEED_ERR_PARSE    20
#define MIHFT_FEED_ERR_NOMEM    24
#define MIHFT_FEED_REJECT_TICK  32
#define MIHFT_FEED_REJECT_VALUE 36

#ifndef MIHFT_MAX_NOTIONAL
#error "MIHFT_MAX_NOTIONAL は mihft_types.h で定義すること"
#endif

typedef struct {
    char instr_code[32];
    long long bid_amt;
    long long ask_amt;
    long long last_amt;
    long long vol_qty;
    long long tick_ts;
} feed_row_t;

typedef struct {
    char instr_code[32];
    char instr_name[96];
    int instr_tier;
    long long tick_amt;
    long long lot_qty;
    char board_code[8];
} inst_row_t;

typedef struct {
    inst_row_t *rows;
    size_t len;
    size_t cap;
} inst_table_t;

static void chomp(char *s)
{
    size_t n = strlen(s);

    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

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

static int next_field(char **cur, char **out)
{
    char *p;
    char *q;

    if (*cur == NULL) {
        return -1;
    }

    p = *cur;
    q = strchr(p, ',');
    if (q != NULL) {
        *q = '\0';
        *cur = q + 1;
    } else {
        *cur = NULL;
    }

    *out = p;
    return 0;
}

static int parse_i64(const char *s, long long *out)
{
    char *end;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || *end != '\0') {
        return -1;
    }

    *out = v;
    return 0;
}

static int parse_i32(const char *s, int *out)
{
    long long v;

    if (parse_i64(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static long long tier_tick_amt(int tier)
{
    if (tier == 1) {
        return 100;
    }
    if (tier == 2) {
        return 500;
    }
    if (tier == 3) {
        return 1000;
    }

    return 0;
}

static int tier_valid(int tier)
{
    return tier == 1 || tier == 2 || tier == 3;
}

static int board_valid(const char *board)
{
    return strcmp(board, "T1") == 0 ||
           strcmp(board, "ST") == 0 ||
           strcmp(board, "ETF") == 0;
}

static long long normalize_amt(long long amt, long long tick)
{
    long long rem;

    if (tick <= 0 || amt <= 0) {
        return amt;
    }

    rem = amt % tick;
    return amt - rem;
}

static int parse_inst_line(char *line, inst_row_t *row)
{
    char *cur = line;
    char *f;

    chomp(line);

    if (next_field(&cur, &f) != 0 ||
        copy_field(row->instr_code, sizeof(row->instr_code), f) != 0) {
        return -1;
    }
    if (next_field(&cur, &f) != 0 ||
        copy_field(row->instr_name, sizeof(row->instr_name), f) != 0) {
        return -1;
    }
    if (next_field(&cur, &f) != 0 ||
        parse_i32(f, &row->instr_tier) != 0) {
        return -1;
    }
    if (next_field(&cur, &f) != 0 ||
        parse_i64(f, &row->tick_amt) != 0) {
        return -1;
    }
    if (next_field(&cur, &f) != 0 ||
        parse_i64(f, &row->lot_qty) != 0) {
        return -1;
    }
    if (next_field(&cur, &f) != 0 ||
        copy_field(row->board_code, sizeof(row->board_code), f) != 0) {
        return -1;
    }

    if (cur != NULL ||
        !tier_valid(row->instr_tier) ||
        !board_valid(row->board_code) ||
        row->tick_amt != tier_tick_amt(row->instr_tier) ||
        row->lot_qty <= 0) {
        return -1;
    }

    return 0;
}

static int parse_feed_line(char *line, feed_row_t *row)
{
    char *cur = line;
    char *f;

    chomp(line);

    if (next_field(&cur, &f) != 0 ||
        copy_field(row->instr_code, sizeof(row->instr_code), f) != 0) {
        return -1;
    }
    if (next_field(&cur, &f) != 0 || parse_i64(f, &row->bid_amt) != 0) {
        return -1;
    }
    if (next_field(&cur, &f) != 0 || parse_i64(f, &row->ask_amt) != 0) {
        return -1;
    }
    if (next_field(&cur, &f) != 0 || parse_i64(f, &row->last_amt) != 0) {
        return -1;
    }
    if (next_field(&cur, &f) != 0 || parse_i64(f, &row->vol_qty) != 0) {
        return -1;
    }
    if (next_field(&cur, &f) != 0 || parse_i64(f, &row->tick_ts) != 0) {
        return -1;
    }

    if (cur != NULL ||
        row->bid_amt < 0 ||
        row->ask_amt < 0 ||
        row->last_amt < 0 ||
        row->vol_qty < 0 ||
        row->tick_ts <= 0) {
        return -1;
    }

    return 0;
}

static int inst_push(inst_table_t *tab, const inst_row_t *row)
{
    inst_row_t *new_rows;
    size_t new_cap;

    if (tab->len == tab->cap) {
        new_cap = tab->cap == 0 ? 64U : tab->cap * 2U;
        if (new_cap < tab->cap ||
            new_cap > SIZE_MAX / sizeof(tab->rows[0])) {
            return -1;
        }

        new_rows = (inst_row_t *)realloc(tab->rows,
                                         new_cap * sizeof(tab->rows[0]));
        if (new_rows == NULL) {
            return -1;
        }

        tab->rows = new_rows;
        tab->cap = new_cap;
    }

    tab->rows[tab->len++] = *row;
    return 0;
}

static const inst_row_t *find_inst(const inst_table_t *tab, const char *instr_code)
{
    size_t i;

    for (i = 0; i < tab->len; i++) {
        if (strcmp(tab->rows[i].instr_code, instr_code) == 0) {
            return &tab->rows[i];
        }
    }

    return NULL;
}

static int load_inst_table(const char *path, inst_table_t *tab)
{
    FILE *fp;
    char line[512];

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "銘柄ファイルを開けません: %s\n", path);
        return MIHFT_FEED_ERR_IO;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "銘柄ファイルが空です\n");
        return MIHFT_FEED_ERR_PARSE;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        inst_row_t row;

        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fclose(fp);
            fprintf(stderr, "銘柄行が長すぎます\n");
            return MIHFT_FEED_ERR_PARSE;
        }

        if (parse_inst_line(line, &row) != 0) {
            fclose(fp);
            fprintf(stderr, "銘柄行の形式が不正です\n");
            return MIHFT_FEED_ERR_PARSE;
        }

        if (inst_push(tab, &row) != 0) {
            fclose(fp);
            fprintf(stderr, "銘柄表の領域を確保できません\n");
            return MIHFT_FEED_ERR_NOMEM;
        }
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "銘柄ファイルの読込に失敗しました\n");
        return MIHFT_FEED_ERR_IO;
    }

    fclose(fp);
    return MIHFT_FEED_ACCEPT;
}

static int normalize_feed_row(feed_row_t *row, const inst_row_t *inst)
{
    long long tick;
    long long notional;

    tick = inst->tick_amt;
    if (tick <= 0) {
        return MIHFT_FEED_REJECT_TICK;
    }

    row->bid_amt = normalize_amt(row->bid_amt, tick);
    row->ask_amt = normalize_amt(row->ask_amt, tick);
    row->last_amt = normalize_amt(row->last_amt, tick);

    if (row->bid_amt > 0 && row->ask_amt > 0 && row->bid_amt > row->ask_amt) {
        return MIHFT_FEED_REJECT_TICK;
    }

    if (row->vol_qty == 0 && row->last_amt > 0) {
        row->last_amt = 0;
    }

    if (row->vol_qty > 0 && row->last_amt > LLONG_MAX / row->vol_qty) {
        return MIHFT_FEED_REJECT_VALUE;
    }

    notional = row->last_amt * row->vol_qty;
    if (notional > MIHFT_MAX_NOTIONAL) {
        return MIHFT_FEED_REJECT_VALUE;
    }

    return MIHFT_FEED_ACCEPT;
}

int main(void)
{
    inst_table_t inst_tab = {0};
    FILE *in;
    FILE *out;
    char line[512];
    int rc;

    rc = load_inst_table("SCINSTF.csv", &inst_tab);
    if (rc != MIHFT_FEED_ACCEPT) {
        free(inst_tab.rows);
        return rc;
    }

    in = fopen("SCMKTD.csv", "r");
    if (in == NULL) {
        fprintf(stderr, "市場データファイルを開けません\n");
        free(inst_tab.rows);
        return MIHFT_FEED_ERR_IO;
    }

    out = fopen("SCMKTD.out.csv", "w");
    if (out == NULL) {
        fprintf(stderr, "出力ファイルを開けません\n");
        fclose(in);
        free(inst_tab.rows);
        return MIHFT_FEED_ERR_IO;
    }

    if (fgets(line, sizeof(line), in) == NULL) {
        fprintf(stderr, "市場データファイルが空です\n");
        fclose(out);
        fclose(in);
        free(inst_tab.rows);
        return MIHFT_FEED_ERR_PARSE;
    }

    fputs("INSTR-CODE,BID-AMT,ASK-AMT,LAST-AMT,VOL-QTY,TICK-TS\n", out);

    while (fgets(line, sizeof(line), in) != NULL) {
        feed_row_t row;
        const inst_row_t *inst;

        if (strchr(line, '\n') == NULL && !feof(in)) {
            fprintf(stderr, "市場データ行が長すぎます\n");
            fclose(out);
            fclose(in);
            free(inst_tab.rows);
            return MIHFT_FEED_ERR_PARSE;
        }

        if (parse_feed_line(line, &row) != 0) {
            fprintf(stderr, "市場データ行の形式が不正です\n");
            fclose(out);
            fclose(in);
            free(inst_tab.rows);
            return MIHFT_FEED_ERR_PARSE;
        }

        inst = find_inst(&inst_tab, row.instr_code);
        if (inst == NULL) {
            fprintf(stderr, "銘柄マスタ未登録です\n");
            fclose(out);
            fclose(in);
            free(inst_tab.rows);
            return MIHFT_FEED_REJECT_TICK;
        }

        rc = normalize_feed_row(&row, inst);
        if (rc != MIHFT_FEED_ACCEPT) {
            fclose(out);
            fclose(in);
            free(inst_tab.rows);
            return rc;
        }

        if (fprintf(out, "%s,%lld,%lld,%lld,%lld,%lld\n",
                    row.instr_code,
                    row.bid_amt,
                    row.ask_amt,
                    row.last_amt,
                    row.vol_qty,
                    row.tick_ts) < 0) {
            fprintf(stderr, "市場データ出力に失敗しました\n");
            fclose(out);
            fclose(in);
            free(inst_tab.rows);
            return MIHFT_FEED_ERR_IO;
        }
    }

    if (ferror(in) || fflush(out) != 0 || fclose(out) != 0) {
        fprintf(stderr, "市場データ入出力に失敗しました\n");
        fclose(in);
        free(inst_tab.rows);
        return MIHFT_FEED_ERR_IO;
    }

    fclose(in);
    free(inst_tab.rows);
    return MIHFT_FEED_ACCEPT;
}
