/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    20240213    今井 彩 (E-230)    SCKILLF共有キャッシュ初版作成
 * 1.01    20240713    今井 彩 (E-230)    CSV境界検査と更新時刻劣化判定を追加
 */
#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef MIHFT_KILL_MAX_ROWS
#define MIHFT_KILL_MAX_ROWS 8192u
#endif

#ifndef MIHFT_KILL_KEY_LEN
#define MIHFT_KILL_KEY_LEN 32u
#endif

#ifndef MIHFT_INSTR_CODE_LEN
#define MIHFT_INSTR_CODE_LEN 16u
#endif

#ifndef MIHFT_CIF_NO_LEN
#define MIHFT_CIF_NO_LEN 20u
#endif

#ifndef MIHFT_REASON_CODE_LEN
#define MIHFT_REASON_CODE_LEN 8u
#endif

#ifndef MIHFT_LINE_MAX
#define MIHFT_LINE_MAX 512u
#endif

#ifndef MIHFT_KILL_STALE_SEC
#define MIHFT_KILL_STALE_SEC 300L
#endif

#ifndef MIHFT_DECISION_ACCEPT
#define MIHFT_DECISION_ACCEPT 0
#endif

#ifndef MIHFT_DECISION_KILL_ALL
#define MIHFT_DECISION_KILL_ALL 10
#endif

#ifndef MIHFT_DECISION_KILL_INSTR
#define MIHFT_DECISION_KILL_INSTR 11
#endif

#ifndef MIHFT_DECISION_KILL_CIF
#define MIHFT_DECISION_KILL_CIF 12
#endif

#ifndef MIHFT_DECISION_FAIL_CLOSED
#define MIHFT_DECISION_FAIL_CLOSED 90
#endif

#ifndef MIHFT_DECISION_PARSE_ERROR
#define MIHFT_DECISION_PARSE_ERROR 91
#endif

#ifndef MIHFT_DECISION_IO_ERROR
#define MIHFT_DECISION_IO_ERROR 92
#endif

typedef struct {
    char kill_key[MIHFT_KILL_KEY_LEN + 1u];
    int scope_kbn;
    char instr_code[MIHFT_INSTR_CODE_LEN + 1u];
    char cif_no[MIHFT_CIF_NO_LEN + 1u];
    int active_flg;
    char reason_code[MIHFT_REASON_CODE_LEN + 1u];
    long updated_ts;
    unsigned int seq;
} kill_entry_t;

typedef struct {
    char instr_code[MIHFT_INSTR_CODE_LEN + 1u];
    char cif_no[MIHFT_CIF_NO_LEN + 1u];
} order_key_t;

static kill_entry_t g_kill_cache[MIHFT_KILL_MAX_ROWS];
static size_t g_kill_count;

static void trim_eol(char *s)
{
    size_t n = strlen(s);

    while (n > 0u && (s[n - 1u] == '\n' || s[n - 1u] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t cap, const char *src)
{
    size_t n = strlen(src);

    if (n == 0u || n >= cap) {
        return -1;
    }
    memcpy(dst, src, n + 1u);
    return 0;
}

static int parse_long_field(const char *s, long minv, long maxv, long *out)
{
    char *endp = NULL;
    long v;

    if (s[0] == '\0') {
        return -1;
    }

    errno = 0;
    v = strtol(s, &endp, 10);
    if (errno != 0 || endp == s || *endp != '\0' || v < minv || v > maxv) {
        return -1;
    }

    *out = v;
    return 0;
}

static int scope_from_field(const char *s, int *out)
{
    long v;

    if (parse_long_field(s, 1L, 3L, &v) != 0) {
        return -1;
    }

    *out = (int)v;
    return 0;
}

static int split_csv(char *line, char **field, size_t need)
{
    size_t n = 0u;
    char *p = line;

    while (n < need) {
        field[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }

    return n == need && strchr(field[need - 1u], ',') == NULL ? 0 : -1;
}

static int parse_kill_record(char *line, kill_entry_t *rec, unsigned int seq)
{
    char *f[7];
    long active;
    long updated;

    trim_eol(line);
    if (split_csv(line, f, 7u) != 0) {
        return -1;
    }
    if (copy_field(rec->kill_key, sizeof rec->kill_key, f[0]) != 0) {
        return -1;
    }
    if (scope_from_field(f[1], &rec->scope_kbn) != 0) {
        return -1;
    }
    if (copy_field(rec->instr_code, sizeof rec->instr_code, f[2]) != 0) {
        return -1;
    }
    if (copy_field(rec->cif_no, sizeof rec->cif_no, f[3]) != 0) {
        return -1;
    }
    if (parse_long_field(f[4], 0L, 1L, &active) != 0) {
        return -1;
    }
    if (copy_field(rec->reason_code, sizeof rec->reason_code, f[5]) != 0) {
        return -1;
    }
    if (parse_long_field(f[6], 1L, LONG_MAX, &updated) != 0) {
        return -1;
    }

    rec->active_flg = (int)active;
    rec->updated_ts = updated;
    rec->seq = seq;
    return 0;
}

static int parse_order_record(char *line, order_key_t *ord)
{
    char *f[2];

    trim_eol(line);
    if (split_csv(line, f, 2u) != 0) {
        return -1;
    }
    if (copy_field(ord->instr_code, sizeof ord->instr_code, f[0]) != 0) {
        return -1;
    }
    if (copy_field(ord->cif_no, sizeof ord->cif_no, f[1]) != 0) {
        return -1;
    }

    return 0;
}

static int load_sckillf(const char *path)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];
    unsigned int seq = 0u;

    g_kill_count = 0u;

    if (fp == NULL) {
        fprintf(stderr, "E001,SCKILLFオープン失敗,%s\n", path);
        return MIHFT_DECISION_IO_ERROR;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        if (strchr(line, '\n') == NULL && !feof(fp)) {
            fclose(fp);
            fprintf(stderr, "E002,SCKILLF行長過大\n");
            return MIHFT_DECISION_PARSE_ERROR;
        }
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '#') {
            continue;
        }
        if (g_kill_count >= MIHFT_KILL_MAX_ROWS) {
            fclose(fp);
            fprintf(stderr, "E003,SCKILLF件数超過\n");
            return MIHFT_DECISION_PARSE_ERROR;
        }
        if (parse_kill_record(line, &g_kill_cache[g_kill_count], ++seq) != 0) {
            fclose(fp);
            fprintf(stderr, "E004,SCKILLF項目不正,%u\n", seq);
            return MIHFT_DECISION_PARSE_ERROR;
        }
        ++g_kill_count;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "E005,SCKILLF読込失敗\n");
        return MIHFT_DECISION_IO_ERROR;
    }

    fclose(fp);
    return MIHFT_DECISION_ACCEPT;
}

static int is_stale(long updated_ts, long now_ts)
{
    if (updated_ts > now_ts) {
        return 1;
    }
    return now_ts - updated_ts > MIHFT_KILL_STALE_SEC;
}

static int scope_rank(int scope)
{
    if (scope == 3) {
        return 3;
    }
    if (scope == 2) {
        return 2;
    }
    if (scope == 1) {
        return 1;
    }
    return 0;
}

static int scope_matches(const kill_entry_t *e, const order_key_t *o)
{
    if (e->scope_kbn == 1) {
        return 1;
    }
    if (e->scope_kbn == 2) {
        return strcmp(e->instr_code, o->instr_code) == 0;
    }
    if (e->scope_kbn == 3) {
        return strcmp(e->cif_no, o->cif_no) == 0;
    }
    return 0;
}

static int decision_for_scope(int scope, int stale)
{
    if (stale) {
        return MIHFT_DECISION_FAIL_CLOSED;
    }
    if (scope == 3) {
        return MIHFT_DECISION_KILL_CIF;
    }
    if (scope == 2) {
        return MIHFT_DECISION_KILL_INSTR;
    }
    return MIHFT_DECISION_KILL_ALL;
}

static int judge_order(const order_key_t *ord, long now_ts)
{
    const kill_entry_t *best = NULL;
    int best_rank = -1;
    int best_stale = 0;
    size_t i;

    for (i = 0u; i < g_kill_count; ++i) {
        const kill_entry_t *e = &g_kill_cache[i];
        int rank;
        int stale;

        if (e->active_flg == 0 || !scope_matches(e, ord)) {
            continue;
        }

        rank = scope_rank(e->scope_kbn);
        stale = is_stale(e->updated_ts, now_ts);
        if (best == NULL || rank > best_rank ||
            (rank == best_rank && stale > best_stale) ||
            (rank == best_rank && stale == best_stale && e->seq > best->seq)) {
            best = e;
            best_rank = rank;
            best_stale = stale;
        }
    }

    if (best == NULL) {
        return MIHFT_DECISION_ACCEPT;
    }
    return decision_for_scope(best->scope_kbn, best_stale);
}

int main(void)
{
    const char *kill_path = getenv("SCKILLF_PATH");
    const char *order_path = getenv("MIHFT_ORDER_PATH");
    FILE *ofp;
    char line[MIHFT_LINE_MAX];
    int rc;
    long now_ts = (long)time(NULL);

    if (kill_path == NULL || kill_path[0] == '\0') {
        kill_path = "SCKILLF.csv";
    }

    rc = load_sckillf(kill_path);
    if (rc != MIHFT_DECISION_ACCEPT) {
        return rc;
    }

    ofp = stdin;
    if (order_path != NULL && order_path[0] != '\0') {
        ofp = fopen(order_path, "r");
        if (ofp == NULL) {
            fprintf(stderr, "E006,注文ファイルオープン失敗,%s\n", order_path);
            return MIHFT_DECISION_IO_ERROR;
        }
    }

    while (fgets(line, sizeof line, ofp) != NULL) {
        order_key_t ord;
        int decision;

        if (strchr(line, '\n') == NULL && !feof(ofp)) {
            if (ofp != stdin) {
                fclose(ofp);
            }
            fprintf(stderr, "E007,注文行長過大\n");
            return MIHFT_DECISION_PARSE_ERROR;
        }
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '#') {
            continue;
        }
        if (parse_order_record(line, &ord) != 0) {
            if (ofp != stdin) {
                fclose(ofp);
            }
            fprintf(stderr, "E008,注文項目不正\n");
            return MIHFT_DECISION_PARSE_ERROR;
        }

        decision = judge_order(&ord, now_ts);
        printf("%s,%s,%d\n", ord.instr_code, ord.cif_no, decision);
    }

    if (ferror(ofp)) {
        if (ofp != stdin) {
            fclose(ofp);
        }
        fprintf(stderr, "E009,注文読込失敗\n");
        return MIHFT_DECISION_IO_ERROR;
    }

    if (ofp != stdin) {
        fclose(ofp);
    }

    return MIHFT_DECISION_ACCEPT;
}
