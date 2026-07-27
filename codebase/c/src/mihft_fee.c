/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20231114  村上 健司 (E-301)  SCFEEFによる約定手数料計算を追加
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SCFEEF_MAX 256
#define BOARD_CODE_MAX 31
#define LINE_MAX_LEN 512

typedef struct {
    char    board_code[BOARD_CODE_MAX + 1];
    double  fee_rate;       /* 料率 (約定代金に対する率) */
    int64_t min_fee_amt;    /* 最低手数料 (最小通貨単位 ×100) */
} scfeef_row_t;

static void trim_field(char *s)
{
    char *p;
    size_t n;

    while (isspace((unsigned char)*s)) {
        memmove(s, s + 1, strlen(s));
    }

    n = strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1])) {
        s[n - 1] = '\0';
        --n;
    }

    if (n >= 2 && s[0] == '"' && s[n - 1] == '"') {
        memmove(s, s + 1, n - 2);
        s[n - 2] = '\0';
        p = s;
        while ((p = strstr(p, "\"\"")) != NULL) {
            memmove(p, p + 1, strlen(p));
            ++p;
        }
    }
}

static int split_csv3(char *line, char *out[3])
{
    int idx = 0;
    int quoted = 0;
    char *start = line;
    char *p;

    for (p = line; *p != '\0'; ++p) {
        if (*p == '"') {
            if (quoted && p[1] == '"') {
                ++p;
            } else {
                quoted = !quoted;
            }
        } else if (*p == ',' && !quoted) {
            if (idx >= 3) {
                return -1;
            }
            *p = '\0';
            out[idx++] = start;
            start = p + 1;
        } else if ((*p == '\n' || *p == '\r') && !quoted) {
            *p = '\0';
            break;
        }
    }

    if (quoted || idx != 2) {
        return -1;
    }

    out[idx] = start;
    for (idx = 0; idx < 3; ++idx) {
        trim_field(out[idx]);
    }

    return 0;
}

static int parse_double_field(const char *s, double min_value, double *out)
{
    char *end;
    double v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtod(s, &end);
    if (errno != 0 || end == s || *end != '\0' || !isfinite(v) || v < min_value) {
        return -1;
    }

    *out = v;
    return 0;
}

static int parse_i64_field(const char *s, int64_t min_value, int64_t *out)
{
    char *end;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }

    errno = 0;
    v = strtoll(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0' || (int64_t)v < min_value) {
        return -1;
    }

    *out = (int64_t)v;
    return 0;
}

static int same_board(const char *a, const char *b)
{
    while (*a != '\0' && *b != '\0') {
        if (toupper((unsigned char)*a) != toupper((unsigned char)*b)) {
            return 0;
        }
        ++a;
        ++b;
    }
    return *a == '\0' && *b == '\0';
}

static int load_scfeef(const char *path, scfeef_row_t *rows, size_t cap, size_t *count)
{
    FILE *fp;
    char line[LINE_MAX_LEN];
    unsigned long lineno = 0;
    size_t used = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "E001:SCFEEFを開けません:%s\n", path);
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *fields[3];
        size_t len;

        ++lineno;
        len = strlen(line);
        if (len == sizeof(line) - 1 && line[len - 1] != '\n') {
            fprintf(stderr, "E002:SCFEEF行長超過:%lu\n", lineno);
            fclose(fp);
            return -1;
        }

        if (lineno == 1 && strncmp(line, "BOARD-CODE", 10) == 0) {
            continue;
        }

        if (split_csv3(line, fields) != 0) {
            fprintf(stderr, "E003:SCFEEF項目不正:%lu\n", lineno);
            fclose(fp);
            return -1;
        }

        if (fields[0][0] == '\0' || strlen(fields[0]) > BOARD_CODE_MAX) {
            fprintf(stderr, "E004:BOARD-CODE不正:%lu\n", lineno);
            fclose(fp);
            return -1;
        }

        if (used >= cap) {
            fprintf(stderr, "E005:SCFEEF件数超過:%lu\n", lineno);
            fclose(fp);
            return -1;
        }

        if (parse_double_field(fields[1], 0.0, &rows[used].fee_rate) != 0 ||
            parse_i64_field(fields[2], 0, &rows[used].min_fee_amt) != 0) {
            fprintf(stderr, "E006:手数料項目不正:%lu\n", lineno);
            fclose(fp);
            return -1;
        }

        if (rows[used].fee_rate > 1.0) {
            fprintf(stderr, "E007:手数料範囲不正:%lu\n", lineno);
            fclose(fp);
            return -1;
        }

        for (size_t i = 0; i < used; ++i) {
            if (same_board(rows[i].board_code, fields[0])) {
                fprintf(stderr, "E008:BOARD-CODE重複:%lu\n", lineno);
                fclose(fp);
                return -1;
            }
        }

        strcpy(rows[used].board_code, fields[0]);
        ++used;
    }

    if (ferror(fp)) {
        fprintf(stderr, "E009:SCFEEF読込失敗\n");
        fclose(fp);
        return -1;
    }

    fclose(fp);

    if (used == 0) {
        fprintf(stderr, "E010:SCFEEF空\n");
        return -1;
    }

    *count = used;
    return 0;
}

static const scfeef_row_t *find_scfeef(const scfeef_row_t *rows, size_t count, const char *board_code)
{
    size_t i;

    for (i = 0; i < count; ++i) {
        if (same_board(rows[i].board_code, board_code)) {
            return &rows[i];
        }
    }

    return NULL;
}

static int calc_commission(int64_t notional, const scfeef_row_t *row, int64_t *commission)
{
    double raw;
    int64_t fee;

    if (notional < 0 || row == NULL || commission == NULL) {
        return -1;
    }

    /* 手数料 = 約定代金(最小通貨単位) × 料率。最小通貨単位へ丸め、最低手数料を下限とする。 */
    raw = (double)notional * row->fee_rate;
    if (!isfinite(raw) || raw > (double)INT64_MAX) {
        return -1;
    }

    fee = (int64_t)llround(raw);
    *commission = fee < row->min_fee_amt ? row->min_fee_amt : fee;
    return 0;
}

int main(void)
{
    const char *path = getenv("SCFEEF_CSV");
    const char *board_code = getenv("BOARD_CODE");
    const char *notional_text = getenv("FILL_NOTIONAL");
    scfeef_row_t rows[SCFEEF_MAX];
    size_t count = 0;
    int64_t notional;
    int64_t commission;
    const scfeef_row_t *fee;

    if (path == NULL || *path == '\0') {
        path = "SCFEEF.csv";
    }

    if (load_scfeef(path, rows, SCFEEF_MAX, &count) != 0) {
        return 2;
    }

    if (board_code == NULL || *board_code == '\0' || strlen(board_code) > BOARD_CODE_MAX) {
        fprintf(stderr, "E011:BOARD_CODE不正\n");
        return 2;
    }

    if (parse_i64_field(notional_text, 0, &notional) != 0) {
        fprintf(stderr, "E012:FILL_NOTIONAL不正\n");
        return 2;
    }

    fee = find_scfeef(rows, count, board_code);
    if (fee == NULL) {
        fprintf(stderr, "E013:BOARD-CODE未登録\n");
        return 2;
    }

    if (calc_commission(notional, fee, &commission) != 0) {
        fprintf(stderr, "E014:手数料計算失敗\n");
        return 2;
    }

    printf("%s,%lld,%lld\n", fee->board_code, (long long)notional, (long long)commission);

#ifdef MIHFT_DECISION_OK
    return MIHFT_DECISION_OK;
#elif defined(MIHFT_DECISION_ACCEPT)
    return MIHFT_DECISION_ACCEPT;
#elif defined(MIHFT_DECISION_PASS)
    return MIHFT_DECISION_PASS;
#else
    return 0;
#endif
}
