/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20190416  市場基盤部  売買単位検証ベンチ入力の初版作成
 */
#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_RC_IOERR  64
#define MIHFT_RC_PARSE  65
#define MIHFT_RC_NOTFND 66

enum {
    MIHFT_DECISION_ACCEPT_LOCAL = 0,
    MIHFT_DECISION_REJECT_TICK_LOCAL = 12
};

struct mihft_instr_ref_local {
    char instr_code[32];
    char instr_name[96];
    int instr_tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[8];
};

static void mihft_trim(char *s)
{
    char *p;
    size_t n;

    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n') {
        memmove(s, s + 1, strlen(s));
    }

    n = strlen(s);
    while (n > 0) {
        p = s + n - 1;
        if (*p != ' ' && *p != '\t' && *p != '\r' && *p != '\n') {
            break;
        }
        *p = '\0';
        --n;
    }
}

static int mihft_copy_field(char *dst, size_t dst_sz, const char *src)
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

static int mihft_parse_i64(const char *s, int64_t *out)
{
    char *endp;
    long long v;

    if (*s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || *endp != '\0') {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int mihft_parse_i32(const char *s, int *out)
{
    int64_t v;

    if (mihft_parse_i64(s, &v) != 0) {
        return -1;
    }
    if (v < INT32_MIN || v > INT32_MAX) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int mihft_next_csv_field(char **cur, char *dst, size_t dst_sz)
{
    char *p;
    size_t n;
    int quoted;

    if (*cur == NULL || dst_sz == 0) {
        return -1;
    }

    p = *cur;
    n = 0;
    quoted = 0;

    if (*p == '"') {
        quoted = 1;
        ++p;
    }

    while (*p != '\0') {
        if (quoted) {
            if (*p == '"') {
                if (p[1] == '"') {
                    if (n + 1 >= dst_sz) {
                        return -1;
                    }
                    dst[n++] = '"';
                    p += 2;
                    continue;
                }
                ++p;
                if (*p == ',') {
                    ++p;
                }
                break;
            }
        } else if (*p == ',') {
            ++p;
            break;
        } else if (*p == '\r' || *p == '\n') {
            break;
        }

        if (n + 1 >= dst_sz) {
            return -1;
        }
        dst[n++] = *p++;
    }

    dst[n] = '\0';
    *cur = p;
    mihft_trim(dst);
    return 0;
}

static int mihft_parse_scinstf_line(const char *line, struct mihft_instr_ref_local *ref)
{
    char work[512];
    char f0[64];
    char f1[128];
    char f2[32];
    char f3[32];
    char f4[32];
    char f5[32];
    char *cur;

    if (strlen(line) >= sizeof(work)) {
        return -1;
    }

    memcpy(work, line, strlen(line) + 1);
    cur = work;

    if (mihft_next_csv_field(&cur, f0, sizeof(f0)) != 0 ||
        mihft_next_csv_field(&cur, f1, sizeof(f1)) != 0 ||
        mihft_next_csv_field(&cur, f2, sizeof(f2)) != 0 ||
        mihft_next_csv_field(&cur, f3, sizeof(f3)) != 0 ||
        mihft_next_csv_field(&cur, f4, sizeof(f4)) != 0 ||
        mihft_next_csv_field(&cur, f5, sizeof(f5)) != 0) {
        return -1;
    }

    if (mihft_copy_field(ref->instr_code, sizeof(ref->instr_code), f0) != 0 ||
        mihft_copy_field(ref->instr_name, sizeof(ref->instr_name), f1) != 0 ||
        mihft_copy_field(ref->board_code, sizeof(ref->board_code), f5) != 0) {
        return -1;
    }

    if (mihft_parse_i32(f2, &ref->instr_tier) != 0 ||
        mihft_parse_i64(f3, &ref->tick_amt) != 0 ||
        mihft_parse_i64(f4, &ref->lot_qty) != 0) {
        return -1;
    }

    if (ref->instr_code[0] == '\0' ||
        ref->instr_tier < 1 || ref->instr_tier > 3 ||
        ref->tick_amt <= 0 ||
        ref->lot_qty <= 0 ||
        ref->board_code[0] == '\0') {
        return -1;
    }

    return 0;
}

static int mihft_board_orderable(int tier, const char *board)
{
    if (strcmp(board, "T1") == 0) {
        return tier == 1 || tier == 2;
    }
    if (strcmp(board, "ST") == 0) {
        return tier == 2 || tier == 3;
    }
    if (strcmp(board, "ETF") == 0) {
        return tier == 1 || tier == 2;
    }
    return 0;
}

static int mihft_lot_guard_decide(const struct mihft_instr_ref_local *ref, int64_t ord_qty)
{
    if (ord_qty <= 0) {
        return MIHFT_DECISION_REJECT_TICK_LOCAL;
    }

    if (ref->lot_qty <= 0 || ord_qty % ref->lot_qty != 0) {
        return MIHFT_DECISION_REJECT_TICK_LOCAL;
    }

    if (!mihft_board_orderable(ref->instr_tier, ref->board_code)) {
        return MIHFT_DECISION_REJECT_TICK_LOCAL;
    }

    return MIHFT_DECISION_ACCEPT_LOCAL;
}

static int mihft_find_instr(FILE *fp, const char *instr_code, struct mihft_instr_ref_local *ref)
{
    char line[512];
    unsigned long line_no;

    line_no = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        struct mihft_instr_ref_local cur;

        ++line_no;
        if (line[0] == '\0' || line[0] == '\n' || line[0] == '\r') {
            continue;
        }

        if (line_no == 1 && strncmp(line, "INSTR-CODE,", 11) == 0) {
            continue;
        }

        if (mihft_parse_scinstf_line(line, &cur) != 0) {
            fprintf(stderr, "SCINSTF解析異常 行=%lu\n", line_no);
            return MIHFT_RC_PARSE;
        }

        if (strcmp(cur.instr_code, instr_code) == 0) {
            *ref = cur;
            return 0;
        }
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCINSTF読込異常\n");
        return MIHFT_RC_IOERR;
    }

    return MIHFT_RC_NOTFND;
}

int main(int argc, char **argv)
{
    FILE *fp;
    struct mihft_instr_ref_local ref;
    int64_t ord_qty;
    int rc;

    if (argc != 4) {
        fprintf(stderr, "引数異常 使用法: mihft_lot_guard SCINSTF INSTR-CODE ORD-QTY\n");
        return MIHFT_RC_PARSE;
    }

    if (mihft_parse_i64(argv[3], &ord_qty) != 0) {
        fprintf(stderr, "ORD-QTY解析異常\n");
        return MIHFT_RC_PARSE;
    }

    fp = fopen(argv[1], "r");
    if (fp == NULL) {
        fprintf(stderr, "SCINSTFオープン異常\n");
        return MIHFT_RC_IOERR;
    }

    rc = mihft_find_instr(fp, argv[2], &ref);
    if (fclose(fp) != 0 && rc == 0) {
        fprintf(stderr, "SCINSTFクローズ異常\n");
        return MIHFT_RC_IOERR;
    }

    if (rc != 0) {
        if (rc == MIHFT_RC_NOTFND) {
            fprintf(stderr, "銘柄未検出\n");
        }
        return rc;
    }

    return mihft_lot_guard_decide(&ref, ord_qty);
}
