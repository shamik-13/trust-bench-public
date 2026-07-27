/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20240213  渡辺 隆 (E-260)   FIXシーケンス監視の初版作成
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_MAX_LINE       1024
#define MIHFT_MAX_FIELD      128
#define MIHFT_MAX_RECORDS    4096
#define MIHFT_MAX_SESSIONS   256
#define MIHFT_DECISION_BASE  900000000LL

#ifndef MIHFT_FIXSEQGUARD_SUCCESS
#define MIHFT_FIXSEQGUARD_SUCCESS 0
#endif

#ifndef MIHFT_FIXSEQGUARD_IOERR
#define MIHFT_FIXSEQGUARD_IOERR 70
#endif

#ifndef MIHFT_FIXSEQGUARD_PARSEERR
#define MIHFT_FIXSEQGUARD_PARSEERR 71
#endif

#ifndef MIHFT_FIXSEQGUARD_STOP_ACTION
#define MIHFT_FIXSEQGUARD_STOP_ACTION 9101
#endif

#ifndef MIHFT_FIXSEQGUARD_RESYNC_ACTION
#define MIHFT_FIXSEQGUARD_RESYNC_ACTION 9102
#endif

#ifndef MIHFT_FIXSEQGUARD_GAP_REASON
#define MIHFT_FIXSEQGUARD_GAP_REASON 9201
#endif

#ifndef MIHFT_FIXSEQGUARD_DUP_REASON
#define MIHFT_FIXSEQGUARD_DUP_REASON 9202
#endif

#ifndef MIHFT_FIXSEQGUARD_RESET_REASON
#define MIHFT_FIXSEQGUARD_RESET_REASON 9203
#endif

#ifndef MIHFT_FIXSEQGUARD_BAD_REASON
#define MIHFT_FIXSEQGUARD_BAD_REASON 9204
#endif

typedef struct {
    long long decision_id;
    char order_id[MIHFT_MAX_FIELD];
    char instr_code[MIHFT_MAX_FIELD];
    int action_code;
    int reason_code;
    char decision_ts[MIHFT_MAX_FIELD];
    int session_no;
    long long seq_no;
} HfdeclogRecord;

typedef struct {
    int session_no;
    long long expected_seq;
    long long max_decision_id;
    int active;
} SessionState;

static void trim(char *s)
{
    size_t n;
    char *p = s;

    while (*p != '\0' && isspace((unsigned char)*p)) {
        p++;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1U);
    }

    n = strlen(s);
    while (n > 0U && isspace((unsigned char)s[n - 1U])) {
        s[--n] = '\0';
    }
}

static int parse_ll_strict(const char *s, long long *out)
{
    char *endp;
    long long v;

    errno = 0;
    v = strtoll(s, &endp, 10);
    if (s == endp || errno == ERANGE) {
        return -1;
    }
    while (*endp != '\0') {
        if (!isspace((unsigned char)*endp)) {
            return -1;
        }
        endp++;
    }
    *out = v;
    return 0;
}

static int parse_int_strict(const char *s, int *out)
{
    long long v;

    if (parse_ll_strict(s, &v) != 0 || v < INT_MIN || v > INT_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int split_csv_line(char *line, char fields[][MIHFT_MAX_FIELD], size_t need)
{
    size_t idx = 0U;
    char *p = line;

    while (idx < need) {
        char *start = p;
        char *dst = fields[idx];
        size_t len = 0U;
        int quoted = 0;

        if (*p == '"') {
            quoted = 1;
            p++;
            start = p;
        }

        while (*p != '\0') {
            if (quoted) {
                if (*p == '"' && p[1] == '"') {
                    if (len + 1U >= MIHFT_MAX_FIELD) {
                        return -1;
                    }
                    dst[len++] = '"';
                    p += 2;
                    start = p;
                    continue;
                }
                if (*p == '"') {
                    p++;
                    break;
                }
            } else if (*p == ',') {
                break;
            }

            if (len + 1U >= MIHFT_MAX_FIELD) {
                return -1;
            }
            dst[len++] = *p++;
        }

        (void)start;
        dst[len] = '\0';
        trim(dst);

        if (quoted) {
            while (*p != '\0' && *p != ',') {
                if (!isspace((unsigned char)*p)) {
                    return -1;
                }
                p++;
            }
        }

        if (*p == ',') {
            p++;
        } else if (idx + 1U < need) {
            return -1;
        }
        idx++;
    }

    while (*p != '\0') {
        if (!isspace((unsigned char)*p)) {
            return -1;
        }
        p++;
    }
    return 0;
}

static int token_number(const char *s, const char *key, long long *out)
{
    size_t klen = strlen(key);
    const char *p = s;

    while ((p = strstr(p, key)) != NULL) {
        const char *v = p + klen;
        if (*v == '=' || *v == ':' || *v == '-') {
            v++;
            if (isdigit((unsigned char)*v)) {
                return parse_ll_strict(v, out);
            }
        }
        p += klen;
    }
    return -1;
}

static int derive_fix_keys(HfdeclogRecord *r)
{
    long long sid = 0;
    long long seq = 0;

    if (token_number(r->reason_code == 0 ? "" : r->order_id, "S", &sid) != 0 &&
        token_number(r->order_id, "SID", &sid) != 0 &&
        token_number(r->instr_code, "S", &sid) != 0) {
        sid = (long long)((r->decision_id % 97LL) + 1LL);
    }

    if (token_number(r->order_id, "Q", &seq) != 0 &&
        token_number(r->order_id, "SEQ", &seq) != 0 &&
        token_number(r->instr_code, "Q", &seq) != 0) {
        seq = r->decision_id > 0 ? r->decision_id : 1LL;
    }

    if (sid <= 0LL || sid > INT_MAX || seq <= 0LL) {
        return -1;
    }

    r->session_no = (int)sid;
    r->seq_no = seq;
    return 0;
}

static int parse_hfdeclog_record(char *line, HfdeclogRecord *r)
{
    char fields[6][MIHFT_MAX_FIELD];

    if (split_csv_line(line, fields, 6U) != 0) {
        return -1;
    }
    if (parse_ll_strict(fields[0], &r->decision_id) != 0 ||
        parse_int_strict(fields[3], &r->action_code) != 0 ||
        parse_int_strict(fields[4], &r->reason_code) != 0) {
        return -1;
    }

    snprintf(r->order_id, sizeof(r->order_id), "%s", fields[1]);
    snprintf(r->instr_code, sizeof(r->instr_code), "%s", fields[2]);
    snprintf(r->decision_ts, sizeof(r->decision_ts), "%s", fields[5]);

    return derive_fix_keys(r);
}

static int read_hfdeclog(const char *path, HfdeclogRecord *records, size_t *count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];
    size_t n = 0U;
    unsigned long lineno = 0UL;

    fp = fopen(path, "r");
    if (fp == NULL) {
        return -2;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        size_t len = strlen(line);
        lineno++;

        if (len > 0U && line[len - 1U] != '\n' && !feof(fp)) {
            fclose(fp);
            return -1;
        }
        while (len > 0U && (line[len - 1U] == '\n' || line[len - 1U] == '\r')) {
            line[--len] = '\0';
        }
        trim(line);
        if (line[0] == '\0') {
            continue;
        }
        if (lineno == 1UL && strstr(line, "DECISION-ID") != NULL) {
            continue;
        }
        if (n >= MIHFT_MAX_RECORDS) {
            fclose(fp);
            return -1;
        }
        if (parse_hfdeclog_record(line, &records[n]) != 0) {
            fclose(fp);
            return -1;
        }
        n++;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -2;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static SessionState *find_session(SessionState *sessions, size_t *count, int session_no)
{
    size_t i;

    for (i = 0U; i < *count; i++) {
        if (sessions[i].active && sessions[i].session_no == session_no) {
            return &sessions[i];
        }
    }

    if (*count >= MIHFT_MAX_SESSIONS) {
        return NULL;
    }

    sessions[*count].session_no = session_no;
    sessions[*count].expected_seq = 1LL;
    sessions[*count].max_decision_id = 0LL;
    sessions[*count].active = 1;
    (*count)++;
    return &sessions[*count - 1U];
}

static int is_reset_candidate(const HfdeclogRecord *r, const SessionState *s)
{
    if (r->seq_no == 1LL && s->expected_seq > 5000LL) {
        return 1;
    }
    if (r->reason_code == MIHFT_FIXSEQGUARD_RESET_REASON && r->seq_no <= s->expected_seq) {
        return 1;
    }
    if (strstr(r->order_id, "RESET") != NULL || strstr(r->instr_code, "RESET") != NULL) {
        return r->seq_no == 1LL;
    }
    return 0;
}

static void make_timestamp(char *buf, size_t bufsz)
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_WIN32)
    localtime_s(&tmv, &now);
#else
    localtime_r(&now, &tmv);
#endif
    (void)strftime(buf, bufsz, "%Y%m%d%H%M%S", &tmv);
}

static int append_event(FILE *fp, long long decision_id, const HfdeclogRecord *src,
                        int action_code, int reason_code, const char *ts)
{
    int rc = fprintf(fp, "%lld,%s,%s,%d,%d,%s\n",
                     decision_id,
                     src->order_id,
                     src->instr_code,
                     action_code,
                     reason_code,
                     ts);
    return rc < 0 ? -1 : 0;
}

static int inspect_and_append(const char *path, HfdeclogRecord *records, size_t count)
{
    SessionState sessions[MIHFT_MAX_SESSIONS];
    size_t session_count = 0U;
    size_t i;
    long long next_decision_id = MIHFT_DECISION_BASE;
    char ts[MIHFT_MAX_FIELD];
    FILE *fp;

    memset(sessions, 0, sizeof(sessions));

    for (i = 0U; i < count; i++) {
        if (records[i].decision_id >= next_decision_id) {
            next_decision_id = records[i].decision_id + 1LL;
        }
    }

    make_timestamp(ts, sizeof(ts));

    fp = fopen(path, "a");
    if (fp == NULL) {
        return -2;
    }

    for (i = 0U; i < count; i++) {
        SessionState *s = find_session(sessions, &session_count, records[i].session_no);
        if (s == NULL) {
            fclose(fp);
            return -1;
        }

        if (records[i].decision_id > s->max_decision_id) {
            s->max_decision_id = records[i].decision_id;
        }

        if (records[i].seq_no == s->expected_seq) {
            if (s->expected_seq == LLONG_MAX) {
                fclose(fp);
                return -1;
            }
            s->expected_seq++;
            continue;
        }

        if (records[i].seq_no > s->expected_seq) {
            if (is_reset_candidate(&records[i], s)) {
                if (append_event(fp, next_decision_id++, &records[i],
                                 MIHFT_FIXSEQGUARD_RESYNC_ACTION,
                                 MIHFT_FIXSEQGUARD_RESET_REASON, ts) != 0) {
                    fclose(fp);
                    return -2;
                }
                s->expected_seq = records[i].seq_no + 1LL;
            } else {
                if (append_event(fp, next_decision_id++, &records[i],
                                 MIHFT_FIXSEQGUARD_STOP_ACTION,
                                 MIHFT_FIXSEQGUARD_GAP_REASON, ts) != 0) {
                    fclose(fp);
                    return -2;
                }
            }
            continue;
        }

        if (records[i].seq_no < s->expected_seq) {
            if (is_reset_candidate(&records[i], s)) {
                if (append_event(fp, next_decision_id++, &records[i],
                                 MIHFT_FIXSEQGUARD_RESYNC_ACTION,
                                 MIHFT_FIXSEQGUARD_RESET_REASON, ts) != 0) {
                    fclose(fp);
                    return -2;
                }
                s->expected_seq = records[i].seq_no + 1LL;
            } else {
                if (append_event(fp, next_decision_id++, &records[i],
                                 MIHFT_FIXSEQGUARD_STOP_ACTION,
                                 MIHFT_FIXSEQGUARD_DUP_REASON, ts) != 0) {
                    fclose(fp);
                    return -2;
                }
            }
        }
    }

    if (fflush(fp) != 0) {
        fclose(fp);
        return -2;
    }
    if (fclose(fp) != 0) {
        return -2;
    }

    return 0;
}

int main(void)
{
    HfdeclogRecord records[MIHFT_MAX_RECORDS];
    size_t count = 0U;
    const char *paths[] = { "HFDECLOG", "HFDECLOG.csv", NULL };
    const char **p;
    const char *selected = NULL;
    int rc = -2;

    for (p = paths; *p != NULL; p++) {
        rc = read_hfdeclog(*p, records, &count);
        if (rc == 0) {
            selected = *p;
            break;
        }
        if (rc == -1) {
            fprintf(stderr, "HFDECLOG解析異常\n");
            return MIHFT_FIXSEQGUARD_PARSEERR;
        }
    }

    if (selected == NULL) {
        fprintf(stderr, "HFDECLOG入出力異常\n");
        return MIHFT_FIXSEQGUARD_IOERR;
    }

    rc = inspect_and_append(selected, records, count);
    if (rc == -1) {
        fprintf(stderr, "HFDECLOG監視データ異常\n");
        return MIHFT_FIXSEQGUARD_PARSEERR;
    }
    if (rc != 0) {
        fprintf(stderr, "HFDECLOG書込異常\n");
        return MIHFT_FIXSEQGUARD_IOERR;
    }

    return MIHFT_FIXSEQGUARD_SUCCESS;
}
