/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20250121  大野 修 (E-225)  概算手数料計算の初版作成
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_FEE_ROWS 128
#define MAX_INST_ROWS 4096
#define MAX_LINE_LEN 1024
#define MAX_FIELD_LEN 128
#define OUT_SESS_DT "20250115"
#define OUT_CALC_TS "20250115090000000"

enum {
    RC_IO_ERROR = 1,
    RC_PARSE_ERROR = 2,
    RC_LIMIT_ERROR = 3
};

typedef struct {
    char board_code[8];
    int64_t fee_rate_scaled;
    int64_t min_fee_x100;
} FeeRow;

typedef struct {
    char instr_code[32];
    char instr_name[96];
    int tier;
    int64_t tick_amt;
    int64_t lot_qty;
    char board_code[8];
} InstRow;

typedef struct {
    char exec_id[32];
    char order_id[32];
    char instr_code[32];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[32];
} ExecRow;

static FeeRow g_fee_rows[MAX_FEE_ROWS];
static size_t g_fee_count;
static InstRow g_inst_rows[MAX_INST_ROWS];
static size_t g_inst_count;

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

static int split_csv_line(char *line, char fields[][MAX_FIELD_LEN], size_t need)
{
    size_t col = 0U;
    char *p = line;

    while (col < need) {
        size_t n = 0U;
        int quoted = 0;

        if (*p == '"') {
            quoted = 1;
            ++p;
        }

        while (*p != '\0') {
            if (quoted != 0) {
                if (*p == '"' && p[1] == '"') {
                    if (n + 1U >= MAX_FIELD_LEN) {
                        return -1;
                    }
                    fields[col][n++] = '"';
                    p += 2;
                    continue;
                }
                if (*p == '"') {
                    ++p;
                    break;
                }
            } else if (*p == ',' || *p == '\n' || *p == '\r') {
                break;
            }

            if (n + 1U >= MAX_FIELD_LEN) {
                return -1;
            }
            fields[col][n++] = *p++;
        }

        fields[col][n] = '\0';
        trim_field(fields[col]);

        while (*p != '\0' && *p != ',') {
            if (*p != '\n' && *p != '\r' && !isspace((unsigned char)*p)) {
                return -1;
            }
            ++p;
        }
        if (*p == ',') {
            ++p;
        } else if (col + 1U < need) {
            return -1;
        }

        ++col;
    }

    return 0;
}

static int is_header_line(const char *line)
{
    while (*line != '\0' && isspace((unsigned char)*line)) {
        ++line;
    }
    return !isdigit((unsigned char)*line) && strchr(line, '-') != NULL;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s) {
        return -1;
    }
    while (*end != '\0') {
        if (!isspace((unsigned char)*end)) {
            return -1;
        }
        ++end;
    }
    *out = (int64_t)v;
    return 0;
}

static int parse_decimal_scaled(const char *s, int64_t scale, int64_t *out)
{
    int sign = 1;
    int64_t whole = 0;
    int64_t frac = 0;
    int64_t div = 1;
    const unsigned char *p = (const unsigned char *)s;

    while (isspace(*p)) {
        ++p;
    }
    if (*p == '-') {
        sign = -1;
        ++p;
    } else if (*p == '+') {
        ++p;
    }
    if (!isdigit(*p) && *p != '.') {
        return -1;
    }

    while (isdigit(*p)) {
        if (whole > (INT64_MAX - (*p - '0')) / 10) {
            return -1;
        }
        whole = whole * 10 + (*p - '0');
        ++p;
    }

    if (*p == '.') {
        ++p;
        while (isdigit(*p) && div < scale) {
            frac = frac * 10 + (*p - '0');
            div *= 10;
            ++p;
        }
        while (isdigit(*p)) {
            ++p;
        }
    }

    while (isspace(*p)) {
        ++p;
    }
    if (*p != '\0') {
        return -1;
    }

    while (div < scale) {
        frac *= 10;
        div *= 10;
    }

    if (whole > (INT64_MAX - frac) / scale) {
        return -1;
    }
    *out = (whole * scale + frac) * sign;
    return 0;
}

static FILE *open_input(const char *base)
{
    char name[MAX_FIELD_LEN];
    FILE *fp;

    (void)snprintf(name, sizeof(name), "%s.csv", base);
    fp = fopen(name, "r");
    if (fp != NULL) {
        return fp;
    }

    fp = fopen(base, "r");
    if (fp != NULL) {
        return fp;
    }

    for (size_t i = 0U; base[i] != '\0' && i + 5U < sizeof(name); ++i) {
        name[i] = (char)tolower((unsigned char)base[i]);
        name[i + 1U] = '\0';
    }
    (void)strncat(name, ".csv", sizeof(name) - strlen(name) - 1U);
    return fopen(name, "r");
}

static const FeeRow *find_fee(const char *board_code)
{
    for (size_t i = 0U; i < g_fee_count; ++i) {
        if (strcmp(g_fee_rows[i].board_code, board_code) == 0) {
            return &g_fee_rows[i];
        }
    }
    return NULL;
}

static const InstRow *find_inst(const char *instr_code)
{
    for (size_t i = 0U; i < g_inst_count; ++i) {
        if (strcmp(g_inst_rows[i].instr_code, instr_code) == 0) {
            return &g_inst_rows[i];
        }
    }
    return NULL;
}

static int load_fee_file(void)
{
    FILE *fp = open_input("SCFEEF");
    char line[MAX_LINE_LEN];

    if (fp == NULL) {
        fprintf(stderr, "SCFEEFを開けません\n");
        return RC_IO_ERROR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char f[3][MAX_FIELD_LEN];

        if (line[0] == '\n' || line[0] == '\r' || is_header_line(line)) {
            continue;
        }
        if (g_fee_count >= MAX_FEE_ROWS || split_csv_line(line, f, 3U) != 0) {
            fclose(fp);
            fprintf(stderr, "SCFEEFの形式が不正です\n");
            return RC_PARSE_ERROR;
        }

        (void)snprintf(g_fee_rows[g_fee_count].board_code,
                       sizeof(g_fee_rows[g_fee_count].board_code), "%s", f[0]);
        if (parse_decimal_scaled(f[1], 1000000, &g_fee_rows[g_fee_count].fee_rate_scaled) != 0 ||
            parse_decimal_scaled(f[2], 100, &g_fee_rows[g_fee_count].min_fee_x100) != 0 ||
            g_fee_rows[g_fee_count].fee_rate_scaled < 0 ||
            g_fee_rows[g_fee_count].min_fee_x100 < 0) {
            fclose(fp);
            fprintf(stderr, "SCFEEFの数値が不正です\n");
            return RC_PARSE_ERROR;
        }
        ++g_fee_count;
    }

    if (ferror(fp) != 0) {
        fclose(fp);
        fprintf(stderr, "SCFEEFの読込に失敗しました\n");
        return RC_IO_ERROR;
    }
    fclose(fp);
    return 0;
}

static int load_inst_file(void)
{
    FILE *fp = open_input("SCINSTF");
    char line[MAX_LINE_LEN];

    if (fp == NULL) {
        fprintf(stderr, "SCINSTFを開けません\n");
        return RC_IO_ERROR;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char f[6][MAX_FIELD_LEN];

        if (line[0] == '\n' || line[0] == '\r' || is_header_line(line)) {
            continue;
        }
        if (g_inst_count >= MAX_INST_ROWS || split_csv_line(line, f, 6U) != 0) {
            fclose(fp);
            fprintf(stderr, "SCINSTFの形式が不正です\n");
            return RC_PARSE_ERROR;
        }

        (void)snprintf(g_inst_rows[g_inst_count].instr_code,
                       sizeof(g_inst_rows[g_inst_count].instr_code), "%s", f[0]);
        (void)snprintf(g_inst_rows[g_inst_count].instr_name,
                       sizeof(g_inst_rows[g_inst_count].instr_name), "%s", f[1]);
        (void)snprintf(g_inst_rows[g_inst_count].board_code,
                       sizeof(g_inst_rows[g_inst_count].board_code), "%s", f[5]);

        {
            int64_t tier64;
            if (parse_i64(f[2], &tier64) != 0 ||
                parse_i64(f[3], &g_inst_rows[g_inst_count].tick_amt) != 0 ||
                parse_i64(f[4], &g_inst_rows[g_inst_count].lot_qty) != 0 ||
                tier64 < 1 || tier64 > 3 ||
                g_inst_rows[g_inst_count].tick_amt <= 0 ||
                g_inst_rows[g_inst_count].lot_qty <= 0) {
                fclose(fp);
                fprintf(stderr, "SCINSTFの数値が不正です\n");
                return RC_PARSE_ERROR;
            }
            g_inst_rows[g_inst_count].tier = (int)tier64;
        }

        ++g_inst_count;
    }

    if (ferror(fp) != 0) {
        fclose(fp);
        fprintf(stderr, "SCINSTFの読込に失敗しました\n");
        return RC_IO_ERROR;
    }
    fclose(fp);
    return 0;
}

static int parse_exec_line(char *line, ExecRow *row)
{
    char f[7][MAX_FIELD_LEN];

    if (split_csv_line(line, f, 7U) != 0) {
        return -1;
    }

    (void)snprintf(row->exec_id, sizeof(row->exec_id), "%s", f[0]);
    (void)snprintf(row->order_id, sizeof(row->order_id), "%s", f[1]);
    (void)snprintf(row->instr_code, sizeof(row->instr_code), "%s", f[2]);
    row->side_kbn = f[3][0];
    (void)snprintf(row->exec_ts, sizeof(row->exec_ts), "%s", f[6]);

    if ((row->side_kbn != 'B' && row->side_kbn != 'S') ||
        parse_i64(f[4], &row->fill_qty) != 0 ||
        parse_i64(f[5], &row->fill_amt) != 0 ||
        row->fill_qty <= 0 ||
        row->fill_amt <= 0) {
        return -1;
    }

    return 0;
}

static int64_t fee_x100_for_exec(int64_t fill_amt, const FeeRow *fee)
{
    int64_t computed;

#if defined(__SIZEOF_INT128__)
    __int128 v = (__int128)fill_amt * (__int128)fee->fee_rate_scaled;
    v = (v + 5000) / 10000;
    if (v > INT64_MAX) {
        return -1;
    }
    computed = (int64_t)v;
#else
    if (fill_amt > INT64_MAX / fee->fee_rate_scaled) {
        return -1;
    }
    computed = (fill_amt * fee->fee_rate_scaled + 5000) / 10000;
#endif

    if (computed < fee->min_fee_x100) {
        computed = fee->min_fee_x100;
    }
    return computed;
}

static int process_exec_file(void)
{
    FILE *in = open_input("SCEXEC");
    FILE *out;
    char line[MAX_LINE_LEN];

    if (in == NULL) {
        fprintf(stderr, "SCEXECを開けません\n");
        return RC_IO_ERROR;
    }

    out = fopen("SCPNLF.csv", "w");
    if (out == NULL) {
        fclose(in);
        fprintf(stderr, "SCPNLFを作成できません\n");
        return RC_IO_ERROR;
    }

    if (fprintf(out, "CIF-NO,INSTR-CODE,SESS-DT,RLZD-AMT,UNRLZD-AMT,FEE-AMT,CALC-TS\n") < 0) {
        fclose(out);
        fclose(in);
        fprintf(stderr, "SCPNLFの出力に失敗しました\n");
        return RC_IO_ERROR;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        ExecRow exec_row;
        const InstRow *inst;
        const FeeRow *fee;
        int64_t fee_x100;

        if (line[0] == '\n' || line[0] == '\r' || is_header_line(line)) {
            continue;
        }
        if (parse_exec_line(line, &exec_row) != 0) {
            fclose(out);
            fclose(in);
            fprintf(stderr, "SCEXECの形式が不正です\n");
            return RC_PARSE_ERROR;
        }

        if (exec_row.fill_amt > MIHFT_MAX_NOTIONAL) {
            fprintf(stderr, "EXEC-ID=%s 想定元本超過のため手数料計算を除外しました\n", exec_row.exec_id);
            continue;
        }

        inst = find_inst(exec_row.instr_code);
        if (inst == NULL) {
            fprintf(stderr, "EXEC-ID=%s 銘柄未登録のため手数料計算を除外しました\n", exec_row.exec_id);
            continue;
        }

        fee = find_fee(inst->board_code);
        if (fee == NULL) {
            fprintf(stderr, "EXEC-ID=%s BOARD-CODE=%s 手数料未設定です\n",
                    exec_row.exec_id, inst->board_code);
            continue;
        }

        fee_x100 = fee_x100_for_exec(exec_row.fill_amt, fee);
        if (fee_x100 < 0) {
            fclose(out);
            fclose(in);
            fprintf(stderr, "手数料計算で桁あふれを検知しました\n");
            return RC_LIMIT_ERROR;
        }

        if (fprintf(out, "%s,%s,%s,0,0,%" PRId64 ",%s\n",
                    exec_row.order_id,
                    exec_row.instr_code,
                    OUT_SESS_DT,
                    fee_x100,
                    OUT_CALC_TS) < 0) {
            fclose(out);
            fclose(in);
            fprintf(stderr, "SCPNLFの出力に失敗しました\n");
            return RC_IO_ERROR;
        }
    }

    if (ferror(in) != 0) {
        fclose(out);
        fclose(in);
        fprintf(stderr, "SCEXECの読込に失敗しました\n");
        return RC_IO_ERROR;
    }

    if (fclose(out) != 0) {
        fclose(in);
        fprintf(stderr, "SCPNLFの確定に失敗しました\n");
        return RC_IO_ERROR;
    }
    fclose(in);
    return 0;
}

int main(void)
{
    int rc;

    rc = load_fee_file();
    if (rc != 0) {
        return rc;
    }

    rc = load_inst_file();
    if (rc != 0) {
        return rc;
    }

    rc = process_exec_file();
    if (rc != 0) {
        return rc;
    }

    return 0;
}
