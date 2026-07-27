/* 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210715  篠原 健 (E-203)  初版作成
 * 1.01  20211215  渡辺 隆 (E-260)  SCCACT近接EX-DT判定と保守制限を追加
 * 1.02  20220515  渡辺 隆 (E-260)  CSV境界検査と桁あふれ検査を強化
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

#define SCCACT_PATH "SCCACT.csv"
#define SCPOSF_PATH "SCPOSF.csv"

#define MAX_LINE_LEN 512
#define MAX_FIELD_LEN 64
#define MAX_ACTIONS 4096
#define MAX_POSITIONS 8192
#define EX_WINDOW_DAYS 5
#define DECISION_ACCEPT 0
#define DECISION_REJECT_MARGIN 4
#define DECISION_REJECT_NOTIONAL 8
#define DECISION_REJECT_TICK 12
#define ERR_IO 64
#define ERR_PARSE 65
#define ERR_RANGE 66

typedef struct {
    char action_id[MAX_FIELD_LEN];
    char instr_code[MAX_FIELD_LEN];
    int ex_yyyymmdd;
    char action_kbn[MAX_FIELD_LEN];
    int64_t ratio_num;
    int64_t ratio_den;
    int64_t cash_amt;
    int near_flag;
} LocalAction;

typedef struct {
    char cif_no[MAX_FIELD_LEN];
    char instr_code[MAX_FIELD_LEN];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
    int ca_pending_flag;
    int decision_code;
} LocalPosition;

static void trim_ascii(char *s)
{
    size_t n;
    char *p = s;

    while (*p != '\0' && isspace((unsigned char)*p)) {
        ++p;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1U);
    }

    n = strlen(s);
    while (n > 0U && isspace((unsigned char)s[n - 1U])) {
        s[--n] = '\0';
    }
}

static int split_csv_line(char *line, char fields[][MAX_FIELD_LEN], size_t need)
{
    size_t col = 0U;
    char *p = line;

    while (col < need) {
        size_t len = 0U;
        int quoted = 0;

        if (*p == '"') {
            quoted = 1;
            ++p;
        }

        while (*p != '\0') {
            if (quoted != 0) {
                if (*p == '"' && p[1] == '"') {
                    if (len + 1U >= MAX_FIELD_LEN) {
                        return -1;
                    }
                    fields[col][len++] = '"';
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

            if (len + 1U >= MAX_FIELD_LEN) {
                return -1;
            }
            fields[col][len++] = *p++;
        }

        fields[col][len] = '\0';
        trim_ascii(fields[col]);

        if (quoted != 0) {
            while (*p == ' ' || *p == '\t') {
                ++p;
            }
        }

        if (col + 1U == need) {
            while (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n') {
                ++p;
            }
            return *p == '\0' ? 0 : -1;
        }

        if (*p != ',') {
            return -1;
        }
        ++p;
        ++col;
    }

    return -1;
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
    if (errno == ERANGE || end == s) {
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

static int parse_yyyymmdd(const char *s, int *out)
{
    int64_t v;
    int y;
    int m;
    int d;

    if (strlen(s) != 8U || parse_i64(s, &v) != 0 || v < 19000101 || v > 29991231) {
        return -1;
    }

    y = (int)(v / 10000);
    m = (int)((v / 100) % 100);
    d = (int)(v % 100);

    if (y < 1900 || m < 1 || m > 12 || d < 1 || d > 31) {
        return -1;
    }
    if ((m == 4 || m == 6 || m == 9 || m == 11) && d > 30) {
        return -1;
    }
    if (m == 2) {
        int leap = ((y % 4 == 0 && y % 100 != 0) || (y % 400 == 0));
        if (d > (leap != 0 ? 29 : 28)) {
            return -1;
        }
    }

    *out = (int)v;
    return 0;
}

static int yyyymmdd_to_daynum(int yyyymmdd, int64_t *out)
{
    struct tm tmv;
    time_t t;

    memset(&tmv, 0, sizeof(tmv));
    tmv.tm_year = (yyyymmdd / 10000) - 1900;
    tmv.tm_mon = ((yyyymmdd / 100) % 100) - 1;
    tmv.tm_mday = yyyymmdd % 100;
    tmv.tm_isdst = -1;

    t = mktime(&tmv);
    if (t == (time_t)-1) {
        return -1;
    }

    *out = (int64_t)(t / 86400);
    return 0;
}

static int today_yyyymmdd(void)
{
    time_t now = time(NULL);
    struct tm *lt = localtime(&now);

    if (lt == NULL) {
        return 19700101;
    }

    return (lt->tm_year + 1900) * 10000 + (lt->tm_mon + 1) * 100 + lt->tm_mday;
}

static int valid_action_kbn(const char *s)
{
    return strcmp(s, "SPLIT") == 0 ||
           strcmp(s, "RSPLIT") == 0 ||
           strcmp(s, "CASH") == 0 ||
           strcmp(s, "MERGER") == 0;
}

static int action_near_exdt(int ex_yyyymmdd, int base_yyyymmdd)
{
    int64_t ex_day;
    int64_t base_day;

    if (yyyymmdd_to_daynum(ex_yyyymmdd, &ex_day) != 0 ||
        yyyymmdd_to_daynum(base_yyyymmdd, &base_day) != 0) {
        return 0;
    }

    return ex_day >= base_day && ex_day <= base_day + EX_WINDOW_DAYS;
}

static int checked_abs_i64(int64_t v, int64_t *out)
{
    if (v == INT64_MIN) {
        return -1;
    }
    *out = v < 0 ? -v : v;
    return 0;
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

static int instr_tier(const char *instr_code)
{
    unsigned int h = 2166136261u;

    while (*instr_code != '\0') {
        h ^= (unsigned char)*instr_code++;
        h *= 16777619u;
    }

    return (int)(h % 3u) + 1;
}

static int tier_margin_bp(int tier)
{
    if (tier == 1) {
        return 1000;
    }
    if (tier == 2) {
        return 2000;
    }
    return 4000;
}

static int load_actions(LocalAction *actions, size_t *count, int today)
{
    FILE *fp = fopen(SCCACT_PATH, "r");
    char line[MAX_LINE_LEN];
    size_t n = 0U;
    unsigned long row = 0UL;

    if (fp == NULL) {
        fprintf(stderr, "SCCACTを開けません\n");
        return ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char f[7][MAX_FIELD_LEN];

        ++row;
        if (row == 1UL && strstr(line, "ACTION-ID") != NULL) {
            continue;
        }
        if (n >= MAX_ACTIONS) {
            fclose(fp);
            fprintf(stderr, "SCCACT件数が上限を超過しました\n");
            return ERR_RANGE;
        }
        if (split_csv_line(line, f, 7U) != 0) {
            fclose(fp);
            fprintf(stderr, "SCCACTのCSV形式が不正です\n");
            return ERR_PARSE;
        }

        strncpy(actions[n].action_id, f[0], sizeof(actions[n].action_id) - 1U);
        actions[n].action_id[sizeof(actions[n].action_id) - 1U] = '\0';
        strncpy(actions[n].instr_code, f[1], sizeof(actions[n].instr_code) - 1U);
        actions[n].instr_code[sizeof(actions[n].instr_code) - 1U] = '\0';
        strncpy(actions[n].action_kbn, f[3], sizeof(actions[n].action_kbn) - 1U);
        actions[n].action_kbn[sizeof(actions[n].action_kbn) - 1U] = '\0';

        if (actions[n].action_id[0] == '\0' ||
            actions[n].instr_code[0] == '\0' ||
            parse_yyyymmdd(f[2], &actions[n].ex_yyyymmdd) != 0 ||
            !valid_action_kbn(actions[n].action_kbn) ||
            parse_i64(f[4], &actions[n].ratio_num) != 0 ||
            parse_i64(f[5], &actions[n].ratio_den) != 0 ||
            parse_i64(f[6], &actions[n].cash_amt) != 0) {
            fclose(fp);
            fprintf(stderr, "SCCACTの項目値が不正です\n");
            return ERR_PARSE;
        }

        if (actions[n].ratio_num < 0 ||
            actions[n].ratio_den < 0 ||
            actions[n].cash_amt < 0 ||
            (strcmp(actions[n].action_kbn, "CASH") != 0 && actions[n].ratio_den == 0)) {
            fclose(fp);
            fprintf(stderr, "SCCACTの比率または金額が不正です\n");
            return ERR_PARSE;
        }

        actions[n].near_flag = action_near_exdt(actions[n].ex_yyyymmdd, today);
        ++n;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCCACTの読込に失敗しました\n");
        return ERR_IO;
    }

    fclose(fp);
    *count = n;
    return DECISION_ACCEPT;
}

static int has_pending_action(const LocalAction *actions, size_t action_count, const char *instr_code)
{
    size_t i;

    for (i = 0U; i < action_count; ++i) {
        if (actions[i].near_flag != 0 && strcmp(actions[i].instr_code, instr_code) == 0) {
            return 1;
        }
    }

    return 0;
}

static int decide_position(LocalPosition *p)
{
    int64_t qty_abs;
    int64_t notional;
    int64_t margin_need;
    int tier;

    p->decision_code = DECISION_ACCEPT;

    if (checked_abs_i64(p->net_qty, &qty_abs) != 0 ||
        checked_abs_i64(p->avg_amt, &notional) != 0 ||
        checked_mul_i64(qty_abs, notional, &notional) != 0) {
        p->decision_code = DECISION_REJECT_NOTIONAL;
        return p->decision_code;
    }

    if (p->ca_pending_flag != 0 && notional > (int64_t)MIHFT_MAX_NOTIONAL) {
        p->decision_code = DECISION_REJECT_NOTIONAL;
        return p->decision_code;
    }

    tier = instr_tier(p->instr_code);
    if (checked_mul_i64(notional, (int64_t)tier_margin_bp(tier), &margin_need) != 0) {
        p->decision_code = DECISION_REJECT_MARGIN;
        return p->decision_code;
    }
    margin_need = (margin_need + 9999) / 10000;

    if (p->ca_pending_flag != 0 && p->rlzd_amt < 0 && -p->rlzd_amt > margin_need) {
        p->decision_code = DECISION_REJECT_MARGIN;
        return p->decision_code;
    }

    if (p->ca_pending_flag != 0 && tier == 3 && (qty_abs % 100) != 0) {
        p->decision_code = DECISION_REJECT_TICK;
        return p->decision_code;
    }

    return p->decision_code;
}

static int load_positions_and_decide(const LocalAction *actions, size_t action_count, int *worst_decision)
{
    FILE *fp = fopen(SCPOSF_PATH, "r");
    char line[MAX_LINE_LEN];
    unsigned long row = 0UL;
    int worst = DECISION_ACCEPT;

    if (fp == NULL) {
        fprintf(stderr, "SCPOSFを開けません\n");
        return ERR_IO;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char f[5][MAX_FIELD_LEN];
        LocalPosition p;
        int d;

        ++row;
        if (row == 1UL && strstr(line, "CIF-NO") != NULL) {
            continue;
        }

        if (split_csv_line(line, f, 5U) != 0) {
            fclose(fp);
            fprintf(stderr, "SCPOSFのCSV形式が不正です\n");
            return ERR_PARSE;
        }

        memset(&p, 0, sizeof(p));
        strncpy(p.cif_no, f[0], sizeof(p.cif_no) - 1U);
        strncpy(p.instr_code, f[1], sizeof(p.instr_code) - 1U);

        if (p.cif_no[0] == '\0' ||
            p.instr_code[0] == '\0' ||
            parse_i64(f[2], &p.net_qty) != 0 ||
            parse_i64(f[3], &p.avg_amt) != 0 ||
            parse_i64(f[4], &p.rlzd_amt) != 0 ||
            p.avg_amt < 0) {
            fclose(fp);
            fprintf(stderr, "SCPOSFの項目値が不正です\n");
            return ERR_PARSE;
        }

        p.ca_pending_flag = has_pending_action(actions, action_count, p.instr_code);
        d = decide_position(&p);
        if (d > worst) {
            worst = d;
        }

        printf("%s,%s,%d,%d\n", p.cif_no, p.instr_code, p.ca_pending_flag, p.decision_code);
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCPOSFの読込に失敗しました\n");
        return ERR_IO;
    }

    fclose(fp);
    *worst_decision = worst;
    return DECISION_ACCEPT;
}

int main(void)
{
    LocalAction actions[MAX_ACTIONS];
    size_t action_count = 0U;
    int rc;
    int decision = DECISION_ACCEPT;
    int today = today_yyyymmdd();

    rc = load_actions(actions, &action_count, today);
    if (rc != DECISION_ACCEPT) {
        return rc;
    }

    rc = load_positions_and_decide(actions, action_count, &decision);
    if (rc != DECISION_ACCEPT) {
        return rc;
    }

    return decision;
}
