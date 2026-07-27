/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20191022  大野 修 (E-225)     初版作成、重複注文ガードの事前判定処理
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_RC_ACCEPT 0
#define MIHFT_RC_REJECT_MARGIN 4
#define MIHFT_RC_REJECT_NOTIONAL 8
#define MIHFT_RC_REJECT_TICK 12

#define MIHFT_ERR_IO 64
#define MIHFT_ERR_PARSE 65

#define MIHFT_MAX_LINE 512
#define MIHFT_MAX_KEY 96
#define MIHFT_MAX_DEC 4096

typedef struct {
    char order_id[33];
    char cif_no[33];
    char instr_code[17];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} MihftScordfRow;

typedef struct {
    char decision_id[33];
    char order_id[33];
    char cif_no[33];
    char instr_code[17];
    int decision_cd;
    char reason_cd[33];
    int64_t notional_amt;
    int64_t limit_used_amt;
    char decision_ts[32];
} MihftHfdecRow;

typedef struct {
    MihftHfdecRow row;
    uint64_t order_seq;
    int used;
} MihftDecisionSlot;

static int mihft_is_space(char c)
{
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

static void mihft_trim(char *s)
{
    size_t n;
    size_t i;

    while (mihft_is_space(*s)) {
        memmove(s, s + 1, strlen(s));
    }

    n = strlen(s);
    while (n > 0 && mihft_is_space(s[n - 1])) {
        s[n - 1] = '\0';
        n--;
    }

    if (n >= 2 && s[0] == '"' && s[n - 1] == '"') {
        memmove(s, s + 1, n - 2);
        s[n - 2] = '\0';
        for (i = 0; s[i] != '\0'; i++) {
            if (s[i] == '"' && s[i + 1] == '"') {
                memmove(s + i, s + i + 1, strlen(s + i));
            }
        }
    }
}

static int mihft_split_csv(char *line, char *cols[], size_t max_cols, size_t *out_cols)
{
    size_t col = 0;
    int quoted = 0;
    char *p = line;

    if (max_cols == 0) {
        return -1;
    }

    cols[col++] = p;
    for (; *p != '\0'; p++) {
        if (*p == '"') {
            if (quoted && p[1] == '"') {
                p++;
            } else {
                quoted = !quoted;
            }
        } else if (*p == ',' && !quoted) {
            *p = '\0';
            if (col == max_cols) {
                return -1;
            }
            cols[col++] = p + 1;
        }
    }

    if (quoted) {
        return -1;
    }

    for (size_t i = 0; i < col; i++) {
        mihft_trim(cols[i]);
    }

    *out_cols = col;
    return 0;
}

static int mihft_copy_text(char *dst, size_t dst_sz, const char *src)
{
    size_t n = strlen(src);

    if (dst_sz == 0 || n >= dst_sz) {
        return -1;
    }

    memcpy(dst, src, n + 1);
    return 0;
}

static int mihft_parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (*s == '\0') {
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

static uint64_t mihft_order_seq(const char *order_id)
{
    uint64_t v = 0;
    int seen = 0;

    for (const unsigned char *p = (const unsigned char *)order_id; *p != '\0'; p++) {
        if (*p >= '0' && *p <= '9') {
            seen = 1;
            if (v <= (UINT64_MAX - (uint64_t)(*p - '0')) / 10U) {
                v = v * 10U + (uint64_t)(*p - '0');
            }
        }
    }

    return seen ? v : UINT64_MAX;
}

static int mihft_read_scordf(char *line, MihftScordfRow *row)
{
    char *cols[9];
    size_t ncols = 0;

    if (mihft_split_csv(line, cols, 9, &ncols) != 0 || ncols != 9) {
        return -1;
    }

    memset(row, 0, sizeof(*row));
    if (mihft_copy_text(row->order_id, sizeof(row->order_id), cols[0]) != 0 ||
        mihft_copy_text(row->cif_no, sizeof(row->cif_no), cols[1]) != 0 ||
        mihft_copy_text(row->instr_code, sizeof(row->instr_code), cols[2]) != 0 ||
        mihft_copy_text(row->tif_code, sizeof(row->tif_code), cols[5]) != 0) {
        return -1;
    }

    if ((cols[3][0] != 'B' && cols[3][0] != 'S') || cols[3][1] != '\0') {
        return -1;
    }
    row->side_kbn = cols[3][0];

    if ((cols[4][0] != 'L' && cols[4][0] != 'M') || cols[4][1] != '\0') {
        return -1;
    }
    row->ord_type = cols[4][0];

    if (strcmp(row->tif_code, "DAY") != 0 &&
        strcmp(row->tif_code, "IOC") != 0 &&
        strcmp(row->tif_code, "FOK") != 0) {
        return -1;
    }

    if (mihft_parse_i64(cols[6], &row->ord_qty) != 0 ||
        mihft_parse_i64(cols[7], &row->price_amt) != 0 ||
        mihft_parse_int(cols[8], &row->instr_tier) != 0) {
        return -1;
    }

    if (row->ord_qty <= 0 || row->price_amt < 0 ||
        row->instr_tier < 1 || row->instr_tier > 3) {
        return -1;
    }

    return 0;
}

static int mihft_read_hfdec(char *line, MihftHfdecRow *row)
{
    char *cols[9];
    size_t ncols = 0;

    if (mihft_split_csv(line, cols, 9, &ncols) != 0 || ncols != 9) {
        return -1;
    }

    memset(row, 0, sizeof(*row));
    if (mihft_copy_text(row->decision_id, sizeof(row->decision_id), cols[0]) != 0 ||
        mihft_copy_text(row->order_id, sizeof(row->order_id), cols[1]) != 0 ||
        mihft_copy_text(row->cif_no, sizeof(row->cif_no), cols[2]) != 0 ||
        mihft_copy_text(row->instr_code, sizeof(row->instr_code), cols[3]) != 0 ||
        mihft_copy_text(row->reason_cd, sizeof(row->reason_cd), cols[5]) != 0 ||
        mihft_copy_text(row->decision_ts, sizeof(row->decision_ts), cols[8]) != 0) {
        return -1;
    }

    if (mihft_parse_int(cols[4], &row->decision_cd) != 0 ||
        mihft_parse_i64(cols[6], &row->notional_amt) != 0 ||
        mihft_parse_i64(cols[7], &row->limit_used_amt) != 0) {
        return -1;
    }

    if (row->decision_cd != MIHFT_RC_ACCEPT &&
        row->decision_cd != MIHFT_RC_REJECT_MARGIN &&
        row->decision_cd != MIHFT_RC_REJECT_NOTIONAL &&
        row->decision_cd != MIHFT_RC_REJECT_TICK) {
        return -1;
    }

    return 0;
}

static int mihft_same_key(const MihftScordfRow *ord, const MihftHfdecRow *dec)
{
    return strcmp(ord->order_id, dec->order_id) == 0 &&
           strcmp(ord->cif_no, dec->cif_no) == 0;
}

static int mihft_is_header(const char *line, const char *head)
{
    while (mihft_is_space(*line)) {
        line++;
    }
    return strncmp(line, head, strlen(head)) == 0;
}

static int mihft_load_hfdec(MihftDecisionSlot slots[], size_t *count)
{
    FILE *fp = fopen("HFDEC.csv", "r");
    char line[MIHFT_MAX_LINE];
    size_t n = 0;

    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        MihftHfdecRow row;
        uint64_t seq;

        if (line[0] == '\0' || line[0] == '\n' || mihft_is_header(line, "DECISION-ID")) {
            continue;
        }

        if (n == MIHFT_MAX_DEC || mihft_read_hfdec(line, &row) != 0) {
            fclose(fp);
            return -2;
        }

        seq = mihft_order_seq(row.order_id);
        slots[n].row = row;
        slots[n].order_seq = seq;
        slots[n].used = 1;
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    *count = n;
    return 0;
}

static const MihftHfdecRow *mihft_find_latest(const MihftDecisionSlot slots[], size_t count,
                                              const MihftScordfRow *ord)
{
    const MihftHfdecRow *best = NULL;
    uint64_t best_seq = 0;

    for (size_t i = 0; i < count; i++) {
        if (!slots[i].used || !mihft_same_key(ord, &slots[i].row)) {
            continue;
        }

        if (best == NULL ||
            strcmp(slots[i].row.decision_ts, best->decision_ts) > 0 ||
            (strcmp(slots[i].row.decision_ts, best->decision_ts) == 0 &&
             slots[i].order_seq >= best_seq)) {
            best = &slots[i].row;
            best_seq = slots[i].order_seq;
        }
    }

    return best;
}

static int mihft_make_reject_id(char *dst, size_t dst_sz, unsigned long serial)
{
    int n = snprintf(dst, dst_sz, "RJ%010lu", serial);
    return (n > 0 && (size_t)n < dst_sz) ? 0 : -1;
}

static int mihft_now_ts(char *dst, size_t dst_sz)
{
    time_t now = time(NULL);
    struct tm tmv;

    if (now == (time_t)-1) {
        return -1;
    }

#if defined(_POSIX_THREAD_SAFE_FUNCTIONS) || defined(__APPLE__)
    if (localtime_r(&now, &tmv) == NULL) {
        return -1;
    }
#else
    {
        struct tm *tmp = localtime(&now);
        if (tmp == NULL) {
            return -1;
        }
        tmv = *tmp;
    }
#endif

    return strftime(dst, dst_sz, "%Y%m%d%H%M%S", &tmv) > 0 ? 0 : -1;
}

static int mihft_write_reject(FILE *fp, unsigned long serial,
                              const MihftScordfRow *ord, int reject_cd,
                              const char *detail_cd)
{
    char reject_id[16];
    char ts[32];

    if (mihft_make_reject_id(reject_id, sizeof(reject_id), serial) != 0 ||
        mihft_now_ts(ts, sizeof(ts)) != 0) {
        return -1;
    }

    if (fprintf(fp, "%s,%s,%s,%s,%d,%s,%s\n",
                reject_id,
                ord->order_id,
                ord->cif_no,
                ord->instr_code,
                reject_cd,
                detail_cd,
                ts) < 0) {
        return -1;
    }

    return 0;
}

static int mihft_guard_order(const MihftDecisionSlot slots[], size_t slot_count,
                             const MihftScordfRow *ord,
                             FILE *reject_fp, unsigned long *reject_serial)
{
    const MihftHfdecRow *prior = mihft_find_latest(slots, slot_count, ord);

    if (prior == NULL) {
        return MIHFT_RC_ACCEPT;
    }

    if (prior->decision_cd != MIHFT_RC_ACCEPT) {
        if (mihft_write_reject(reject_fp, (*reject_serial)++, ord,
                               prior->decision_cd, prior->reason_cd) != 0) {
            return MIHFT_ERR_IO;
        }
        return prior->decision_cd;
    }

    if (mihft_write_reject(reject_fp, (*reject_serial)++, ord,
                           MIHFT_RC_REJECT_NOTIONAL, "DUP_UNPROC") != 0) {
        return MIHFT_ERR_IO;
    }

    return MIHFT_RC_REJECT_NOTIONAL;
}

int main(void)
{
    MihftDecisionSlot decisions[MIHFT_MAX_DEC];
    size_t decision_count = 0;
    FILE *in_fp = NULL;
    FILE *out_fp = NULL;
    char line[MIHFT_MAX_LINE];
    unsigned long reject_serial = 1;
    int last_decision = MIHFT_RC_ACCEPT;
    int load_rc;

    memset(decisions, 0, sizeof(decisions));

    load_rc = mihft_load_hfdec(decisions, &decision_count);
    if (load_rc != 0) {
        fprintf(stderr, "HFDEC読込失敗\n");
        return load_rc == -2 ? MIHFT_ERR_PARSE : MIHFT_ERR_IO;
    }

    in_fp = fopen("SCORDF.csv", "r");
    if (in_fp == NULL) {
        fprintf(stderr, "SCORDF読込失敗\n");
        return MIHFT_ERR_IO;
    }

    out_fp = fopen("HFRJCT.csv", "w");
    if (out_fp == NULL) {
        fprintf(stderr, "HFRJCT作成失敗\n");
        fclose(in_fp);
        return MIHFT_ERR_IO;
    }

    if (fprintf(out_fp, "REJECT-ID,ORDER-ID,CIF-NO,INSTR-CODE,REJECT-CD,DETAIL-CD,REJECT-TS\n") < 0) {
        fprintf(stderr, "HFRJCT書込失敗\n");
        fclose(out_fp);
        fclose(in_fp);
        return MIHFT_ERR_IO;
    }

    while (fgets(line, sizeof(line), in_fp) != NULL) {
        MihftScordfRow ord;
        int rc;

        if (line[0] == '\0' || line[0] == '\n' || mihft_is_header(line, "ORDER-ID")) {
            continue;
        }

        if (mihft_read_scordf(line, &ord) != 0) {
            fprintf(stderr, "SCORDF解析失敗\n");
            fclose(out_fp);
            fclose(in_fp);
            return MIHFT_ERR_PARSE;
        }

        rc = mihft_guard_order(decisions, decision_count, &ord, out_fp, &reject_serial);
        if (rc == MIHFT_ERR_IO) {
            fprintf(stderr, "HFRJCT書込失敗\n");
            fclose(out_fp);
            fclose(in_fp);
            return MIHFT_ERR_IO;
        }

        last_decision = rc;
    }

    if (ferror(in_fp)) {
        fprintf(stderr, "SCORDF読込失敗\n");
        fclose(out_fp);
        fclose(in_fp);
        return MIHFT_ERR_IO;
    }

    if (fclose(out_fp) != 0) {
        fprintf(stderr, "HFRJCT終了失敗\n");
        fclose(in_fp);
        return MIHFT_ERR_IO;
    }

    if (fclose(in_fp) != 0) {
        fprintf(stderr, "SCORDF終了失敗\n");
        return MIHFT_ERR_IO;
    }

    return last_decision;
}
