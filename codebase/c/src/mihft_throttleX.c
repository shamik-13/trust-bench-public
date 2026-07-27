/* 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240213  藤田 和也 (E-271)  初版作成
 * 1.01  20240713  渡辺 隆 (E-260)  CSV解析の桁あふれ検知を追加
 * 1.02  20241213  村上 健司 (E-301)  マイクロバースト検知のリング集計を追加
 */
#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define X_DECISION_ACCEPT    0
#define X_DECISION_THROTTLED 1
#define X_DECISION_ERROR     9
#define X_REASON_NONE        0
#define X_REASON_MICROBURST  101

#define X_ID_LEN             64
#define X_CODE_LEN           32
#define X_LINE_MAX           512
#define X_FIELD_MAX          96
#define X_BUCKETS            4096
#define X_RING_CAP           256
#define X_WINDOW_NS          1000000000LL
#define X_MAX_ORDERS         100U

typedef struct {
    char decision_id[X_ID_LEN];
    char order_id[X_ID_LEN];
    char instr_code[X_CODE_LEN];
    int action_code;
    int reason_code;
    int64_t decision_ts;
} x_record;

typedef struct {
    char client_code[X_CODE_LEN];
    char instr_code[X_CODE_LEN];
    int action_code;
    int64_t ts[X_RING_CAP];
    size_t head;
    size_t count;
    int used;
} x_bucket;

static x_bucket buckets[X_BUCKETS];

static void trim_field(char *s)
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

static int split_csv(char *line, char fields[][X_FIELD_MAX], size_t need)
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
                    if (len + 1U >= X_FIELD_MAX) {
                        return -1;
                    }
                    fields[col][len++] = '"';
                    p += 2;
                    continue;
                }
                if (*p == '"') {
                    ++p;
                    quoted = 0;
                    break;
                }
            } else if (*p == ',' || *p == '\n' || *p == '\r') {
                break;
            }

            if (len + 1U >= X_FIELD_MAX) {
                return -1;
            }
            fields[col][len++] = *p++;
        }

        fields[col][len] = '\0';
        trim_field(fields[col]);

        if (quoted != 0) {
            return -1;
        }

        if (*p == ',') {
            ++p;
        } else if (col + 1U != need) {
            return -1;
        }

        ++col;
    }

    while (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n') {
        ++p;
    }

    return *p == '\0' ? 0 : -1;
}

static int parse_i32(const char *s, int *out)
{
    char *end = NULL;
    long v;

    errno = 0;
    v = strtol(s, &end, 10);
    if (s == end || errno == ERANGE || v < INT_MIN || v > INT_MAX) {
        return -1;
    }

    while (*end != '\0') {
        if (!isspace((unsigned char)*end)) {
            return -1;
        }
        ++end;
    }

    *out = (int)v;
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    errno = 0;
    v = strtoll(s, &end, 10);
    if (s == end || errno == ERANGE) {
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

static int parse_record(char *line, x_record *rec)
{
    char f[6][X_FIELD_MAX];

    if (split_csv(line, f, 6U) != 0) {
        return -1;
    }

    if (f[0][0] == '\0' || f[1][0] == '\0' || f[2][0] == '\0') {
        return -1;
    }

    if (strlen(f[0]) >= sizeof(rec->decision_id) ||
        strlen(f[1]) >= sizeof(rec->order_id) ||
        strlen(f[2]) >= sizeof(rec->instr_code)) {
        return -1;
    }

    if (parse_i32(f[3], &rec->action_code) != 0 ||
        parse_i32(f[4], &rec->reason_code) != 0 ||
        parse_i64(f[5], &rec->decision_ts) != 0) {
        return -1;
    }

    strcpy(rec->decision_id, f[0]);
    strcpy(rec->order_id, f[1]);
    strcpy(rec->instr_code, f[2]);
    return 0;
}

static void client_from_order(const char *order_id, char *client_code, size_t cap)
{
    size_t i = 0U;

    while (order_id[i] != '\0' &&
           order_id[i] != '-' &&
           order_id[i] != '_' &&
           order_id[i] != ':' &&
           i + 1U < cap) {
        client_code[i] = order_id[i];
        ++i;
    }

    if (i == 0U) {
        size_t n = strlen(order_id);
        size_t take = n < 8U ? n : 8U;

        if (take + 1U > cap) {
            take = cap - 1U;
        }

        memcpy(client_code, order_id, take);
        i = take;
    }

    client_code[i] = '\0';
}

static uint32_t hash_key(const char *a, const char *b, int c)
{
    uint32_t h = 2166136261u;
    const unsigned char *p;

    for (p = (const unsigned char *)a; *p != '\0'; ++p) {
        h = (h ^ *p) * 16777619u;
    }

    for (p = (const unsigned char *)b; *p != '\0'; ++p) {
        h = (h ^ *p) * 16777619u;
    }

    h = (h ^ (uint32_t)c) * 16777619u;
    return h;
}

static x_bucket *find_bucket(const char *client_code,
                             const char *instr_code,
                             int action_code)
{
    uint32_t h = hash_key(client_code, instr_code, action_code);
    size_t pos = (size_t)(h % X_BUCKETS);
    size_t step;

    for (step = 0U; step < X_BUCKETS; ++step) {
        x_bucket *b = &buckets[(pos + step) % X_BUCKETS];

        if (b->used == 0) {
            b->used = 1;
            strncpy(b->client_code, client_code, sizeof(b->client_code) - 1U);
            b->client_code[sizeof(b->client_code) - 1U] = '\0';
            strncpy(b->instr_code, instr_code, sizeof(b->instr_code) - 1U);
            b->instr_code[sizeof(b->instr_code) - 1U] = '\0';
            b->action_code = action_code;
            return b;
        }

        if (b->action_code == action_code &&
            strcmp(b->client_code, client_code) == 0 &&
            strcmp(b->instr_code, instr_code) == 0) {
            return b;
        }
    }

    return NULL;
}

static size_t purge_old(x_bucket *b, int64_t now)
{
    while (b->count > 0U) {
        int64_t oldest = b->ts[b->head];

        if (now < oldest || now - oldest <= X_WINDOW_NS) {
            break;
        }

        b->head = (b->head + 1U) % X_RING_CAP;
        --b->count;
    }

    return b->count;
}

static int should_throttle(const x_record *rec)
{
    char client_code[X_CODE_LEN];
    x_bucket *b;
    size_t tail;

    if (X_MAX_ORDERS == 0U || X_MAX_ORDERS >= X_RING_CAP) {
        return -1;
    }

    client_from_order(rec->order_id, client_code, sizeof(client_code));

    b = find_bucket(client_code, rec->instr_code, rec->action_code);
    if (b == NULL) {
        return -1;
    }

    if (purge_old(b, rec->decision_ts) >= X_MAX_ORDERS) {
        return 1;
    }

    tail = (b->head + b->count) % X_RING_CAP;
    b->ts[tail] = rec->decision_ts;

    if (b->count < X_RING_CAP) {
        ++b->count;
    } else {
        b->head = (b->head + 1U) % X_RING_CAP;
    }

    return 0;
}

static int write_record(FILE *out, const x_record *rec)
{
    return fprintf(out, "%s,%s,%s,%d,%d,%lld\n",
                   rec->decision_id,
                   rec->order_id,
                   rec->instr_code,
                   rec->action_code,
                   rec->reason_code,
                   (long long)rec->decision_ts) < 0 ? -1 : 0;
}

static FILE *open_input(void)
{
    FILE *fp = fopen("HFDECLOG.csv", "r");

    if (fp != NULL) {
        return fp;
    }

    return fopen("HFDECLOG", "r");
}

int main(void)
{
    FILE *in = open_input();
    FILE *out;
    char line[X_LINE_MAX];
    unsigned long line_no = 0UL;
    int final_code = X_DECISION_ACCEPT;

    if (in == NULL) {
        fputs("E001:入力オープン失敗\n", stderr);
        return X_DECISION_ERROR;
    }

    out = fopen("HFDECLOG.out.csv", "w");
    if (out == NULL) {
        fputs("E002:出力オープン失敗\n", stderr);
        fclose(in);
        return X_DECISION_ERROR;
    }

    while (fgets(line, sizeof(line), in) != NULL) {
        x_record rec;
        int verdict;

        ++line_no;

        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0') {
            continue;
        }

        if (line_no == 1UL && strncmp(line, "DECISION-ID,", 12U) == 0) {
            continue;
        }

        if (strchr(line, '\n') == NULL && !feof(in)) {
            fputs("E003:入力行長超過\n", stderr);
            final_code = X_DECISION_ERROR;
            break;
        }

        if (parse_record(line, &rec) != 0) {
            fputs("E004:入力解析失敗\n", stderr);
            final_code = X_DECISION_ERROR;
            break;
        }

        verdict = should_throttle(&rec);
        if (verdict < 0) {
            fputs("E005:抑止表更新失敗\n", stderr);
            final_code = X_DECISION_ERROR;
            break;
        }

        if (verdict > 0) {
            rec.action_code = X_DECISION_THROTTLED;
            rec.reason_code = X_REASON_MICROBURST;
            final_code = X_DECISION_THROTTLED;
        } else if (rec.reason_code == X_REASON_MICROBURST) {
            rec.reason_code = X_REASON_NONE;
        }

        if (write_record(out, &rec) != 0) {
            fputs("E006:出力書込失敗\n", stderr);
            final_code = X_DECISION_ERROR;
            break;
        }
    }

    if (ferror(in) != 0 && final_code != X_DECISION_ERROR) {
        fputs("E007:入力読込失敗\n", stderr);
        final_code = X_DECISION_ERROR;
    }

    if (fclose(out) != 0 && final_code != X_DECISION_ERROR) {
        fputs("E008:出力クローズ失敗\n", stderr);
        final_code = X_DECISION_ERROR;
    }

    if (fclose(in) != 0 && final_code != X_DECISION_ERROR) {
        fputs("E009:入力クローズ失敗\n", stderr);
        final_code = X_DECISION_ERROR;
    }

    return final_code;
}
