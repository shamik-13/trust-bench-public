/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20200902  市場基盤部  ロット建玉分解の初版作成
 * 1.01  20210202  市場基盤部  CSV検査、FIFO消込、余剰建玉作成を追加
 */
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mihft_types.h"

#define MIHFT_DECISION_ACCEPT_CODE 0
#define MIHFT_DECISION_REJECT_NOTIONAL_CODE 8
#define MIHFT_PARSE_ERROR_CODE 20
#define MIHFT_IO_ERROR_CODE 21
#define MIHFT_DATA_ERROR_CODE 22

#define MIHFT_MAX_LINE 512
#define MIHFT_MAX_EXEC 4096
#define MIHFT_MAX_LOT 8192
#define MIHFT_ID_LEN 32
#define MIHFT_CODE_LEN 24
#define MIHFT_TS_LEN 32
#define MIHFT_CIF_LEN 24

typedef struct {
    char exec_id[MIHFT_ID_LEN];
    char order_id[MIHFT_ID_LEN];
    char instr_code[MIHFT_CODE_LEN];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[MIHFT_TS_LEN];
} ExecRow;

typedef struct {
    char lot_id[MIHFT_ID_LEN];
    char cif_no[MIHFT_CIF_LEN];
    char instr_code[MIHFT_CODE_LEN];
    int64_t open_qty;
    int64_t open_amt;
    char acq_ts[MIHFT_TS_LEN];
    char src_exec_id[MIHFT_ID_LEN];
    unsigned long seq;
} LotRow;

static int copy_field(char *dst, size_t dst_size, const char *src)
{
    size_t len;

    if (dst_size == 0) {
        return 0;
    }

    len = strlen(src);
    if (len >= dst_size) {
        return 0;
    }

    memcpy(dst, src, len + 1);
    return 1;
}

static char *trim_eol(char *s)
{
    size_t len = strlen(s);

    while (len > 0 && (s[len - 1] == '\n' || s[len - 1] == '\r')) {
        s[--len] = '\0';
    }

    return s;
}

static int split_csv(char *line, char **cols, size_t max_cols, size_t *count)
{
    size_t n = 0;
    char *p = line;

    while (n < max_cols) {
        cols[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            *count = n;
            return 1;
        }
        *p++ = '\0';
    }

    *count = n;
    return strchr(p, ',') == NULL;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s[0] == '\0') {
        return 0;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0') {
        return 0;
    }

    *out = (int64_t)v;
    return 1;
}

static int parse_exec_row(char *line, ExecRow *row)
{
    char *cols[7];
    size_t n = 0;

    trim_eol(line);
    if (!split_csv(line, cols, 7, &n) || n != 7) {
        return 0;
    }

    if (!copy_field(row->exec_id, sizeof(row->exec_id), cols[0]) ||
        !copy_field(row->order_id, sizeof(row->order_id), cols[1]) ||
        !copy_field(row->instr_code, sizeof(row->instr_code), cols[2]) ||
        !copy_field(row->exec_ts, sizeof(row->exec_ts), cols[6])) {
        return 0;
    }

    if ((cols[3][0] != 'B' && cols[3][0] != 'S') || cols[3][1] != '\0') {
        return 0;
    }
    row->side_kbn = cols[3][0];

    if (!parse_i64(cols[4], &row->fill_qty) ||
        !parse_i64(cols[5], &row->fill_amt) ||
        row->fill_qty <= 0 ||
        row->fill_amt <= 0) {
        return 0;
    }

    return 1;
}

static int parse_lot_row(char *line, LotRow *row, unsigned long seq)
{
    char *cols[7];
    size_t n = 0;

    trim_eol(line);
    if (!split_csv(line, cols, 7, &n) || n != 7) {
        return 0;
    }

    if (!copy_field(row->lot_id, sizeof(row->lot_id), cols[0]) ||
        !copy_field(row->cif_no, sizeof(row->cif_no), cols[1]) ||
        !copy_field(row->instr_code, sizeof(row->instr_code), cols[2]) ||
        !copy_field(row->acq_ts, sizeof(row->acq_ts), cols[5]) ||
        !copy_field(row->src_exec_id, sizeof(row->src_exec_id), cols[6])) {
        return 0;
    }

    if (!parse_i64(cols[3], &row->open_qty) ||
        !parse_i64(cols[4], &row->open_amt) ||
        row->open_qty < 0 ||
        row->open_amt < 0) {
        return 0;
    }

    row->seq = seq;
    return 1;
}

static int read_exec_file(const char *path, ExecRow *rows, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];
    size_t n = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "約定ファイルを開けません: %s\n", path);
        return MIHFT_IO_ERROR_CODE;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "約定ファイルが空です: %s\n", path);
        return MIHFT_PARSE_ERROR_CODE;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        if (n >= cap) {
            fclose(fp);
            fprintf(stderr, "約定件数が上限を超えました\n");
            return MIHFT_DATA_ERROR_CODE;
        }
        if (!parse_exec_row(line, &rows[n])) {
            fclose(fp);
            fprintf(stderr, "約定CSVの形式が不正です\n");
            return MIHFT_PARSE_ERROR_CODE;
        }
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "約定ファイルの読込に失敗しました: %s\n", path);
        return MIHFT_IO_ERROR_CODE;
    }

    fclose(fp);
    *count = n;
    return MIHFT_DECISION_ACCEPT_CODE;
}

static int read_lot_file(const char *path, LotRow *rows, size_t cap, size_t *count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];
    size_t n = 0;
    unsigned long seq = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "ロットファイルを開けません: %s\n", path);
        return MIHFT_IO_ERROR_CODE;
    }

    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "ロットファイルが空です: %s\n", path);
        return MIHFT_PARSE_ERROR_CODE;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        if (n >= cap) {
            fclose(fp);
            fprintf(stderr, "ロット件数が上限を超えました\n");
            return MIHFT_DATA_ERROR_CODE;
        }
        if (!parse_lot_row(line, &rows[n], seq++)) {
            fclose(fp);
            fprintf(stderr, "ロットCSVの形式が不正です\n");
            return MIHFT_PARSE_ERROR_CODE;
        }
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "ロットファイルの読込に失敗しました: %s\n", path);
        return MIHFT_IO_ERROR_CODE;
    }

    fclose(fp);
    *count = n;
    return MIHFT_DECISION_ACCEPT_CODE;
}

static int compare_lot_fifo(const void *a, const void *b)
{
    const LotRow *la = (const LotRow *)a;
    const LotRow *lb = (const LotRow *)b;
    int ts_cmp = strcmp(la->acq_ts, lb->acq_ts);

    if (ts_cmp != 0) {
        return ts_cmp;
    }
    if (la->seq < lb->seq) {
        return -1;
    }
    if (la->seq > lb->seq) {
        return 1;
    }
    return strcmp(la->lot_id, lb->lot_id);
}

static int derive_cif_from_order(const char *order_id, char *cif_no, size_t cif_size)
{
    const char *sep = strchr(order_id, '-');
    size_t len;

    if (sep == NULL) {
        return copy_field(cif_no, cif_size, "CIF000000");
    }

    len = (size_t)(sep - order_id);
    if (len == 0 || len >= cif_size) {
        return 0;
    }

    memcpy(cif_no, order_id, len);
    cif_no[len] = '\0';
    return 1;
}

static int make_lot_id(char *dst, size_t dst_size, const ExecRow *exec, unsigned int suffix)
{
    int written = snprintf(dst, dst_size, "LOT-%s-%03u", exec->exec_id, suffix);

    return written > 0 && (size_t)written < dst_size;
}

static int append_open_lot(LotRow *lots, size_t cap, size_t *lot_count, const ExecRow *exec, int64_t qty, int64_t amt)
{
    LotRow *lot;

    if (*lot_count >= cap) {
        fprintf(stderr, "新規ロット件数が上限を超えました\n");
        return MIHFT_DATA_ERROR_CODE;
    }

    lot = &lots[*lot_count];
    memset(lot, 0, sizeof(*lot));

    if (!make_lot_id(lot->lot_id, sizeof(lot->lot_id), exec, 1) ||
        !derive_cif_from_order(exec->order_id, lot->cif_no, sizeof(lot->cif_no)) ||
        !copy_field(lot->instr_code, sizeof(lot->instr_code), exec->instr_code) ||
        !copy_field(lot->acq_ts, sizeof(lot->acq_ts), exec->exec_ts) ||
        !copy_field(lot->src_exec_id, sizeof(lot->src_exec_id), exec->exec_id)) {
        fprintf(stderr, "新規ロット項目が上限長を超えました\n");
        return MIHFT_DATA_ERROR_CODE;
    }

    lot->open_qty = qty;
    lot->open_amt = amt;
    lot->seq = (unsigned long)*lot_count;
    (*lot_count)++;

    return MIHFT_DECISION_ACCEPT_CODE;
}

static int reduce_fifo_lots(LotRow *lots, size_t lot_count, const ExecRow *exec, int64_t *remaining_qty, int64_t *remaining_amt)
{
    size_t i;

    qsort(lots, lot_count, sizeof(lots[0]), compare_lot_fifo);

    for (i = 0; i < lot_count && *remaining_qty > 0; i++) {
        int64_t close_qty;
        int64_t close_amt;

        if (strcmp(lots[i].instr_code, exec->instr_code) != 0 || lots[i].open_qty == 0) {
            continue;
        }

        close_qty = lots[i].open_qty < *remaining_qty ? lots[i].open_qty : *remaining_qty;
        if (lots[i].open_qty == close_qty) {
            close_amt = lots[i].open_amt;
        } else {
            close_amt = (lots[i].open_amt / lots[i].open_qty) * close_qty;
        }

        lots[i].open_qty -= close_qty;
        lots[i].open_amt -= close_amt;
        *remaining_qty -= close_qty;
        *remaining_amt -= close_amt;

        if (lots[i].open_amt < 0) {
            fprintf(stderr, "ロット金額が負値になりました\n");
            return MIHFT_DATA_ERROR_CODE;
        }
    }

    return MIHFT_DECISION_ACCEPT_CODE;
}

static int apply_execution(LotRow *lots, size_t cap, size_t *lot_count, const ExecRow *exec, int *decision)
{
    int64_t remaining_qty = exec->fill_qty;
    int64_t remaining_amt = exec->fill_amt;
    int rc;

    if (exec->fill_amt > (int64_t)MIHFT_MAX_NOTIONAL) {
        *decision = MIHFT_DECISION_REJECT_NOTIONAL_CODE;
        return MIHFT_DECISION_ACCEPT_CODE;
    }

    if (exec->side_kbn == 'S') {
        rc = reduce_fifo_lots(lots, *lot_count, exec, &remaining_qty, &remaining_amt);
        if (rc != MIHFT_DECISION_ACCEPT_CODE) {
            return rc;
        }
        if (remaining_qty == 0) {
            return MIHFT_DECISION_ACCEPT_CODE;
        }
    }

    return append_open_lot(lots, cap, lot_count, exec, remaining_qty, remaining_amt);
}

static int write_lot_file(const char *path, LotRow *lots, size_t lot_count)
{
    FILE *fp;
    size_t i;

    fp = fopen(path, "w");
    if (fp == NULL) {
        fprintf(stderr, "ロット出力ファイルを開けません: %s\n", path);
        return MIHFT_IO_ERROR_CODE;
    }

    fprintf(fp, "LOT-ID,CIF-NO,INSTR-CODE,OPEN-QTY,OPEN-AMT,ACQ-TS,SRC-EXEC-ID\n");
    for (i = 0; i < lot_count; i++) {
        if (lots[i].open_qty == 0) {
            continue;
        }
        if (fprintf(fp, "%s,%s,%s,%" PRId64 ",%" PRId64 ",%s,%s\n",
                    lots[i].lot_id,
                    lots[i].cif_no,
                    lots[i].instr_code,
                    lots[i].open_qty,
                    lots[i].open_amt,
                    lots[i].acq_ts,
                    lots[i].src_exec_id) < 0) {
            fclose(fp);
            fprintf(stderr, "ロット出力に失敗しました\n");
            return MIHFT_IO_ERROR_CODE;
        }
    }

    if (fclose(fp) != 0) {
        fprintf(stderr, "ロット出力ファイルの終了処理に失敗しました: %s\n", path);
        return MIHFT_IO_ERROR_CODE;
    }

    return MIHFT_DECISION_ACCEPT_CODE;
}

int main(void)
{
    ExecRow execs[MIHFT_MAX_EXEC];
    LotRow lots[MIHFT_MAX_LOT];
    size_t exec_count = 0;
    size_t lot_count = 0;
    size_t i;
    int decision = MIHFT_DECISION_ACCEPT_CODE;
    int rc;

    rc = read_exec_file("SCEXEC.csv", execs, MIHFT_MAX_EXEC, &exec_count);
    if (rc != MIHFT_DECISION_ACCEPT_CODE) {
        return rc;
    }

    rc = read_lot_file("SCLOT.csv", lots, MIHFT_MAX_LOT, &lot_count);
    if (rc != MIHFT_DECISION_ACCEPT_CODE) {
        return rc;
    }

    for (i = 0; i < exec_count; i++) {
        rc = apply_execution(lots, MIHFT_MAX_LOT, &lot_count, &execs[i], &decision);
        if (rc != MIHFT_DECISION_ACCEPT_CODE) {
            return rc;
        }
    }

    rc = write_lot_file("SCLOT.out.csv", lots, lot_count);
    if (rc != MIHFT_DECISION_ACCEPT_CODE) {
        return rc;
    }

    return decision;
}
