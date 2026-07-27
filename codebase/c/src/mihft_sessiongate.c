/*
 * 変更履歴
 * 版数  年月日    担当    概要
 * 1.00  20250603  中川 美和 (E-283)  初版作成
 * 1.01  20251103  渡辺 隆 (E-260)  CSV境界検査と時刻検証を追加
 * 1.02  20250603  西村 亮 (E-204)  休場・昼休み・引け後の判定理由を分離
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_OK 0
#define MIHFT_ERR_IO 12
#define MIHFT_ERR_PARSE 16
#define MIHFT_ERR_DATA 20

#define MIHFT_MAX_LINE 512
#define MIHFT_MAX_FIELD 96
#define MIHFT_MAX_CAL 64

#define MIHFT_SCCALF_PATH "SCCALF.csv"
#define MIHFT_ORDER_PATH "HFORDER.csv"
#define MIHFT_DECLOG_PATH "HFDECLOG.csv"

#define MIHFT_REASON_OPEN "00"
#define MIHFT_REASON_CLOSED "91"
#define MIHFT_REASON_LUNCH "92"
#define MIHFT_REASON_AFTER "93"
#define MIHFT_REASON_BEFORE "94"
#define MIHFT_REASON_CALERR "95"

#define MIHFT_ACTION_KEEP "KEEP"
#define MIHFT_ACTION_REPLACE "REPL"
#define MIHFT_ACTION_REJECT "RJCT"

typedef struct {
    int year;
    int month;
    int day;
} MihftDateKey;

typedef struct {
    int hour;
    int minute;
    int second;
    int millis;
} MihftTimeKey;

typedef struct {
    MihftDateKey date;
    char sess_kbn[MIHFT_MAX_FIELD];
    MihftTimeKey open_ts;
    MihftTimeKey close_ts;
    int open_value;
    int close_value;
} MihftCalendarRow;

typedef struct {
    char order_id[MIHFT_MAX_FIELD];
    char instr_code[MIHFT_MAX_FIELD];
    char action_code[MIHFT_MAX_FIELD];
    MihftDateKey date;
    MihftTimeKey order_ts;
    int order_value;
} MihftOrderRow;

static int mihft_copy_field(char *dst, size_t dst_size, const char *src)
{
    size_t len;

    if (dst == NULL || src == NULL || dst_size == 0) {
        return MIHFT_ERR_PARSE;
    }

    len = strlen(src);
    if (len >= dst_size) {
        return MIHFT_ERR_PARSE;
    }

    memcpy(dst, src, len + 1U);
    return MIHFT_OK;
}

static void mihft_chomp(char *line)
{
    size_t len;

    if (line == NULL) {
        return;
    }

    len = strlen(line);
    while (len > 0U && (line[len - 1U] == '\n' || line[len - 1U] == '\r')) {
        line[len - 1U] = '\0';
        --len;
    }
}

static int mihft_split_csv(char *line, char *fields[], size_t max_fields, size_t *count)
{
    char *p;
    size_t n;

    if (line == NULL || fields == NULL || count == NULL || max_fields == 0U) {
        return MIHFT_ERR_PARSE;
    }

    n = 0U;
    p = line;

    for (;;) {
        if (n >= max_fields) {
            return MIHFT_ERR_PARSE;
        }

        fields[n++] = p;

        while (*p != '\0' && *p != ',') {
            ++p;
        }

        if (*p == '\0') {
            break;
        }

        *p = '\0';
        ++p;
    }

    *count = n;
    return MIHFT_OK;
}

static int mihft_parse_uint_fixed(const char *s, size_t pos, size_t len, int *out)
{
    size_t i;
    int value;

    if (s == NULL || out == NULL) {
        return MIHFT_ERR_PARSE;
    }

    value = 0;
    for (i = 0U; i < len; ++i) {
        unsigned char c = (unsigned char)s[pos + i];
        if (c < (unsigned char)'0' || c > (unsigned char)'9') {
            return MIHFT_ERR_PARSE;
        }
        value = value * 10 + (int)(c - (unsigned char)'0');
    }

    *out = value;
    return MIHFT_OK;
}

static int mihft_parse_date(const char *s, MihftDateKey *out)
{
    int y;
    int m;
    int d;

    if (s == NULL || out == NULL || strlen(s) != 8U) {
        return MIHFT_ERR_PARSE;
    }

    if (mihft_parse_uint_fixed(s, 0U, 4U, &y) != MIHFT_OK ||
        mihft_parse_uint_fixed(s, 4U, 2U, &m) != MIHFT_OK ||
        mihft_parse_uint_fixed(s, 6U, 2U, &d) != MIHFT_OK) {
        return MIHFT_ERR_PARSE;
    }

    if (m < 1 || m > 12 || d < 1 || d > 31) {
        return MIHFT_ERR_DATA;
    }

    out->year = y;
    out->month = m;
    out->day = d;
    return MIHFT_OK;
}

static int mihft_parse_time(const char *s, MihftTimeKey *out, int *sortable)
{
    size_t len;
    int hh;
    int mm;
    int ss;
    int ms;

    if (s == NULL || out == NULL || sortable == NULL) {
        return MIHFT_ERR_PARSE;
    }

    len = strlen(s);
    if (len != 9U && len != 12U) {
        return MIHFT_ERR_PARSE;
    }

    if (mihft_parse_uint_fixed(s, 0U, 2U, &hh) != MIHFT_OK ||
        mihft_parse_uint_fixed(s, 2U, 2U, &mm) != MIHFT_OK ||
        mihft_parse_uint_fixed(s, 4U, 2U, &ss) != MIHFT_OK) {
        return MIHFT_ERR_PARSE;
    }

    if (s[6] != '.') {
        return MIHFT_ERR_PARSE;
    }

    if (len == 9U) {
        if (mihft_parse_uint_fixed(s, 7U, 2U, &ms) != MIHFT_OK) {
            return MIHFT_ERR_PARSE;
        }
        ms *= 10;
    } else {
        if (mihft_parse_uint_fixed(s, 7U, 3U, &ms) != MIHFT_OK) {
            return MIHFT_ERR_PARSE;
        }
    }

    if (hh < 0 || hh > 23 || mm < 0 || mm > 59 || ss < 0 || ss > 59 || ms < 0 || ms > 999) {
        return MIHFT_ERR_DATA;
    }

    out->hour = hh;
    out->minute = mm;
    out->second = ss;
    out->millis = ms;
    *sortable = (((hh * 60 + mm) * 60 + ss) * 1000) + ms;
    return MIHFT_OK;
}

static int mihft_parse_ts(const char *s, MihftDateKey *date, MihftTimeKey *time, int *sortable)
{
    char d[9];
    char t[13];
    size_t len;

    if (s == NULL || date == NULL || time == NULL || sortable == NULL) {
        return MIHFT_ERR_PARSE;
    }

    len = strlen(s);
    if (len != 18U && len != 21U) {
        return MIHFT_ERR_PARSE;
    }

    if (s[8] != '-' && s[8] != 'T') {
        return MIHFT_ERR_PARSE;
    }

    memcpy(d, s, 8U);
    d[8] = '\0';

    if (len == 18U) {
        memcpy(t, s + 9U, 9U);
        t[9] = '\0';
    } else {
        memcpy(t, s + 9U, 12U);
        t[12] = '\0';
    }

    if (mihft_parse_date(d, date) != MIHFT_OK) {
        return MIHFT_ERR_PARSE;
    }

    return mihft_parse_time(t, time, sortable);
}

static int mihft_same_date(const MihftDateKey *a, const MihftDateKey *b)
{
    return a->year == b->year && a->month == b->month && a->day == b->day;
}

static int mihft_load_calendar(MihftCalendarRow rows[], size_t max_rows, size_t *row_count)
{
    FILE *fp;
    char line[MIHFT_MAX_LINE];
    size_t count;

    if (rows == NULL || row_count == NULL || max_rows == 0U) {
        return MIHFT_ERR_PARSE;
    }

    fp = fopen(MIHFT_SCCALF_PATH, "r");
    if (fp == NULL) {
        fprintf(stderr, "SCCALF読込失敗:%s\n", strerror(errno));
        return MIHFT_ERR_IO;
    }

    count = 0U;
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *fields[4];
        size_t nf;
        int rc;

        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }

        rc = mihft_split_csv(line, fields, 4U, &nf);
        if (rc != MIHFT_OK || nf != 4U) {
            fclose(fp);
            fprintf(stderr, "SCCALF項目数不正\n");
            return MIHFT_ERR_PARSE;
        }

        if (strcmp(fields[0], "SESS-DT") == 0) {
            continue;
        }

        if (count >= max_rows) {
            fclose(fp);
            fprintf(stderr, "SCCALF件数超過\n");
            return MIHFT_ERR_DATA;
        }

        rc = mihft_parse_date(fields[0], &rows[count].date);
        if (rc != MIHFT_OK) {
            fclose(fp);
            fprintf(stderr, "SCCALF日付不正\n");
            return rc;
        }

        rc = mihft_copy_field(rows[count].sess_kbn, sizeof(rows[count].sess_kbn), fields[1]);
        if (rc != MIHFT_OK) {
            fclose(fp);
            fprintf(stderr, "SCCALF区分長不正\n");
            return rc;
        }

        rc = mihft_parse_time(fields[2], &rows[count].open_ts, &rows[count].open_value);
        if (rc != MIHFT_OK) {
            fclose(fp);
            fprintf(stderr, "SCCALF開始時刻不正\n");
            return rc;
        }

        rc = mihft_parse_time(fields[3], &rows[count].close_ts, &rows[count].close_value);
        if (rc != MIHFT_OK || rows[count].open_value >= rows[count].close_value) {
            fclose(fp);
            fprintf(stderr, "SCCALF終了時刻不正\n");
            return rc == MIHFT_OK ? MIHFT_ERR_DATA : rc;
        }

        ++count;
    }

    if (ferror(fp)) {
        fclose(fp);
        fprintf(stderr, "SCCALF読込中断\n");
        return MIHFT_ERR_IO;
    }

    fclose(fp);
    *row_count = count;
    return MIHFT_OK;
}

static const MihftCalendarRow *mihft_find_calendar(const MihftCalendarRow rows[], size_t count, const MihftDateKey *date)
{
    size_t i;

    for (i = 0U; i < count; ++i) {
        if (mihft_same_date(&rows[i].date, date)) {
            return &rows[i];
        }
    }

    return NULL;
}

static int mihft_parse_order_line(char *line, MihftOrderRow *order)
{
    char *fields[4];
    size_t nf;
    int rc;

    if (line == NULL || order == NULL) {
        return MIHFT_ERR_PARSE;
    }

    rc = mihft_split_csv(line, fields, 4U, &nf);
    if (rc != MIHFT_OK || nf != 4U) {
        return MIHFT_ERR_PARSE;
    }

    if (mihft_copy_field(order->order_id, sizeof(order->order_id), fields[0]) != MIHFT_OK ||
        mihft_copy_field(order->instr_code, sizeof(order->instr_code), fields[1]) != MIHFT_OK ||
        mihft_copy_field(order->action_code, sizeof(order->action_code), fields[2]) != MIHFT_OK) {
        return MIHFT_ERR_PARSE;
    }

    return mihft_parse_ts(fields[3], &order->date, &order->order_ts, &order->order_value);
}

static int mihft_is_closed_session(const char *sess_kbn)
{
    return strcmp(sess_kbn, "0") == 0 ||
           strcmp(sess_kbn, "CLOSED") == 0 ||
           strcmp(sess_kbn, "HOL") == 0 ||
           strcmp(sess_kbn, "休場") == 0;
}

static int mihft_is_lunch_time(int sortable)
{
    const int lunch_start = ((11 * 60 + 30) * 60) * 1000;
    const int lunch_end = ((12 * 60 + 30) * 60) * 1000;

    return sortable >= lunch_start && sortable < lunch_end;
}

static void mihft_decide(const MihftOrderRow *order, const MihftCalendarRow *cal, char *action, size_t action_size, const char **reason)
{
    if (cal == NULL) {
        (void)mihft_copy_field(action, action_size, MIHFT_ACTION_REJECT);
        *reason = MIHFT_REASON_CALERR;
        return;
    }

    if (mihft_is_closed_session(cal->sess_kbn)) {
        (void)mihft_copy_field(action, action_size, MIHFT_ACTION_REJECT);
        *reason = MIHFT_REASON_CLOSED;
        return;
    }

    if (order->order_value < cal->open_value) {
        (void)mihft_copy_field(action, action_size, MIHFT_ACTION_REPLACE);
        *reason = MIHFT_REASON_BEFORE;
        return;
    }

    if (mihft_is_lunch_time(order->order_value)) {
        (void)mihft_copy_field(action, action_size, MIHFT_ACTION_REPLACE);
        *reason = MIHFT_REASON_LUNCH;
        return;
    }

    if (order->order_value >= cal->close_value) {
        (void)mihft_copy_field(action, action_size, MIHFT_ACTION_REJECT);
        *reason = MIHFT_REASON_AFTER;
        return;
    }

    (void)mihft_copy_field(action, action_size, order->action_code);
    *reason = MIHFT_REASON_OPEN;
}

static int mihft_write_decision(FILE *out, unsigned long seq, const MihftOrderRow *order, const char *action, const char *reason)
{
    int written;

    if (out == NULL || order == NULL || action == NULL || reason == NULL) {
        return MIHFT_ERR_PARSE;
    }

    written = fprintf(out,
                      "D%010lu,%s,%s,%s,%s,%04d%02d%02d-%02d%02d%02d.%03d\n",
                      seq,
                      order->order_id,
                      order->instr_code,
                      action,
                      reason,
                      order->date.year,
                      order->date.month,
                      order->date.day,
                      order->order_ts.hour,
                      order->order_ts.minute,
                      order->order_ts.second,
                      order->order_ts.millis);

    if (written < 0) {
        fprintf(stderr, "HFDECLOG書込失敗\n");
        return MIHFT_ERR_IO;
    }

    return MIHFT_OK;
}

int main(void)
{
    MihftCalendarRow calendar[MIHFT_MAX_CAL];
    size_t calendar_count;
    FILE *in;
    FILE *out;
    char line[MIHFT_MAX_LINE];
    unsigned long seq;
    int rc;

    rc = mihft_load_calendar(calendar, MIHFT_MAX_CAL, &calendar_count);
    if (rc != MIHFT_OK) {
        return rc;
    }

    in = fopen(MIHFT_ORDER_PATH, "r");
    if (in == NULL) {
        fprintf(stderr, "注文CSV読込失敗:%s\n", strerror(errno));
        return MIHFT_ERR_IO;
    }

    out = fopen(MIHFT_DECLOG_PATH, "w");
    if (out == NULL) {
        fclose(in);
        fprintf(stderr, "HFDECLOG作成失敗:%s\n", strerror(errno));
        return MIHFT_ERR_IO;
    }

    if (fprintf(out, "DECISION-ID,ORDER-ID,INSTR-CODE,ACTION-CODE,REASON-CODE,DECISION-TS\n") < 0) {
        fclose(out);
        fclose(in);
        fprintf(stderr, "HFDECLOGヘッダ書込失敗\n");
        return MIHFT_ERR_IO;
    }

    seq = 1UL;
    while (fgets(line, sizeof(line), in) != NULL) {
        MihftOrderRow order;
        const MihftCalendarRow *cal;
        char action[MIHFT_MAX_FIELD];
        const char *reason;

        mihft_chomp(line);
        if (line[0] == '\0') {
            continue;
        }

        if (strncmp(line, "ORDER-ID,", 9U) == 0) {
            continue;
        }

        rc = mihft_parse_order_line(line, &order);
        if (rc != MIHFT_OK) {
            fclose(out);
            fclose(in);
            fprintf(stderr, "注文CSV解析失敗\n");
            return rc;
        }

        cal = mihft_find_calendar(calendar, calendar_count, &order.date);
        mihft_decide(&order, cal, action, sizeof(action), &reason);

        if (strcmp(reason, MIHFT_REASON_OPEN) != 0) {
            rc = mihft_write_decision(out, seq, &order, action, reason);
            if (rc != MIHFT_OK) {
                fclose(out);
                fclose(in);
                return rc;
            }
            ++seq;
        }
    }

    if (ferror(in)) {
        fclose(out);
        fclose(in);
        fprintf(stderr, "注文CSV読込中断\n");
        return MIHFT_ERR_IO;
    }

    if (fclose(out) != 0) {
        fclose(in);
        fprintf(stderr, "HFDECLOGクローズ失敗\n");
        return MIHFT_ERR_IO;
    }

    if (fclose(in) != 0) {
        fprintf(stderr, "注文CSVクローズ失敗\n");
        return MIHFT_ERR_IO;
    }

    return MIHFT_OK;
}
