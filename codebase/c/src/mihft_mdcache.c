/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20200902  西村 亮 (E-204)  初版作成
 * 1.01  20210202  今井 彩 (E-230)  気配検証と通知集計を追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MDCACHE_DECISION_OK 0
#define MDCACHE_DECISION_REJECT 1
#define MDCACHE_HARD_ERROR 2

#define MDCACHE_MAX_INSTRUMENTS 16
#define MDCACHE_MAX_EVENTS 48
#define MDCACHE_MAX_STALE_GAP_NS 250000000LL

typedef struct {
    int64_t instrument_id;
    int64_t bid_px;
    int64_t ask_px;
    int64_t bid_qty;
    int64_t ask_qty;
    int64_t exchange_ts_ns;
    uint64_t sequence;
} mdcache_tick_t;

typedef struct {
    int used;
    int64_t instrument_id;
    int64_t bid_px;
    int64_t ask_px;
    int64_t bid_qty;
    int64_t ask_qty;
    int64_t exchange_ts_ns;
    uint64_t sequence;
    uint64_t updates;
    uint64_t rejects;
} mdcache_entry_t;

typedef struct {
    mdcache_entry_t entries[MDCACHE_MAX_INSTRUMENTS];
    size_t used;
    uint64_t notified;
    uint64_t rejected;
} mdcache_book_t;

static int64_t parse_i64_field(const char *text, int64_t min_value, int64_t max_value, int *ok)
{
    char *endp;
    long long value;

    errno = 0;
    value = strtoll(text, &endp, 10);
    if (text == endp || *endp != '\0' || errno == ERANGE ||
        value < min_value || value > max_value) {
        *ok = 0;
        return 0;
    }

    return (int64_t)value;
}

static uint64_t parse_u64_field(const char *text, int *ok)
{
    char *endp;
    unsigned long long value;

    errno = 0;
    value = strtoull(text, &endp, 10);
    if (text == endp || *endp != '\0' || errno == ERANGE) {
        *ok = 0;
        return 0U;
    }

    return (uint64_t)value;
}

static int parse_tick_line(const char *line, mdcache_tick_t *tick)
{
    char work[160];
    char *fields[7];
    char *cursor;
    size_t field_count = 0U;
    int ok = 1;

    if (line == NULL || tick == NULL || strlen(line) >= sizeof(work)) {
        return -1;
    }

    memcpy(work, line, strlen(line) + 1U);
    cursor = work;

    while (field_count < 7U) {
        char *comma = strchr(cursor, ',');
        fields[field_count++] = cursor;
        if (comma == NULL) {
            break;
        }
        *comma = '\0';
        cursor = comma + 1;
    }

    if (field_count != 7U || strchr(fields[6], ',') != NULL) {
        return -1;
    }

    tick->instrument_id = parse_i64_field(fields[0], 1, INT64_MAX, &ok);
    tick->bid_px = parse_i64_field(fields[1], 1, INT64_MAX, &ok);
    tick->ask_px = parse_i64_field(fields[2], 1, INT64_MAX, &ok);
    tick->bid_qty = parse_i64_field(fields[3], 0, INT64_MAX, &ok);
    tick->ask_qty = parse_i64_field(fields[4], 0, INT64_MAX, &ok);
    tick->exchange_ts_ns = parse_i64_field(fields[5], 1, INT64_MAX, &ok);
    tick->sequence = parse_u64_field(fields[6], &ok);

    return ok ? 0 : -1;
}

static int validate_tick(const mdcache_tick_t *tick, const mdcache_entry_t *prev)
{
    if (tick->bid_px <= 0 || tick->ask_px <= 0 || tick->bid_px >= tick->ask_px) {
        return MDCACHE_DECISION_REJECT;
    }

    if (tick->bid_qty < 0 || tick->ask_qty < 0) {
        return MDCACHE_DECISION_REJECT;
    }

    if (tick->bid_qty == 0 && tick->ask_qty == 0) {
        return MDCACHE_DECISION_REJECT;
    }

    if (prev != NULL && prev->used != 0) {
        if (tick->sequence <= prev->sequence) {
            return MDCACHE_DECISION_REJECT;
        }

        if (tick->exchange_ts_ns + MDCACHE_MAX_STALE_GAP_NS < prev->exchange_ts_ns) {
            return MDCACHE_DECISION_REJECT;
        }
    }

    return MDCACHE_DECISION_OK;
}

static mdcache_entry_t *find_or_create_entry(mdcache_book_t *book, int64_t instrument_id)
{
    size_t i;

    for (i = 0U; i < book->used; i++) {
        if (book->entries[i].used != 0 && book->entries[i].instrument_id == instrument_id) {
            return &book->entries[i];
        }
    }

    if (book->used >= MDCACHE_MAX_INSTRUMENTS) {
        return NULL;
    }

    memset(&book->entries[book->used], 0, sizeof(book->entries[book->used]));
    book->entries[book->used].used = 1;
    book->entries[book->used].instrument_id = instrument_id;

    return &book->entries[book->used++];
}

static int top_changed(const mdcache_entry_t *entry, const mdcache_tick_t *tick)
{
    return entry->bid_px != tick->bid_px ||
           entry->ask_px != tick->ask_px ||
           entry->bid_qty != tick->bid_qty ||
           entry->ask_qty != tick->ask_qty;
}

static void notify_book(mdcache_book_t *book, const mdcache_entry_t *entry)
{
    if (entry->bid_qty > 0 || entry->ask_qty > 0) {
        book->notified++;
    }
}

static int apply_tick(mdcache_book_t *book, const mdcache_tick_t *tick)
{
    mdcache_entry_t *entry;
    int decision;
    int changed;

    entry = find_or_create_entry(book, tick->instrument_id);
    if (entry == NULL) {
        book->rejected++;
        return MDCACHE_DECISION_REJECT;
    }

    decision = validate_tick(tick, entry);
    if (decision != MDCACHE_DECISION_OK) {
        entry->rejects++;
        book->rejected++;
        return decision;
    }

    changed = top_changed(entry, tick);

    entry->bid_px = tick->bid_px;
    entry->ask_px = tick->ask_px;
    entry->bid_qty = tick->bid_qty;
    entry->ask_qty = tick->ask_qty;
    entry->exchange_ts_ns = tick->exchange_ts_ns;
    entry->sequence = tick->sequence;
    entry->updates++;

    if (changed != 0) {
        notify_book(book, entry);
    }

    return MDCACHE_DECISION_OK;
}

int main(void)
{
    static const char *const staged_events[] = {
        "7203,310050,310060,400,300,1793000000000000000,101",
        "6758,143250,143260,100,200,1793000000000000100,201",
        "8306,162110,162120,900,600,1793000000000000200,301",
        "7203,310055,310065,500,300,1793000000000000300,102",
        "6758,143260,143270,100,100,1793000000000000400,202",
        "7203,310070,310060,200,200,1793000000000000500,103",
        "8306,162100,162115,700,800,1793000000000000600,302",
        "9984,895500,895520,50,60,1793000000000000700,401",
        "6758,143265,143275,0,400,1793000000000000800,203",
        "9984,895510,895530,80,40,1793000000000000900,402"
    };

    mdcache_book_t book;
    int final_decision = MDCACHE_DECISION_OK;
    size_t i;

    memset(&book, 0, sizeof(book));

    if (sizeof(staged_events) / sizeof(staged_events[0]) > MDCACHE_MAX_EVENTS) {
        fputs("内部件数上限超過\n", stderr);
        return MDCACHE_HARD_ERROR;
    }

    for (i = 0U; i < sizeof(staged_events) / sizeof(staged_events[0]); i++) {
        mdcache_tick_t tick;
        int decision;

        if (parse_tick_line(staged_events[i], &tick) != 0) {
            fprintf(stderr, "入力解析失敗:%zu\n", i + 1U);
            return MDCACHE_HARD_ERROR;
        }

        decision = apply_tick(&book, &tick);
        if (decision != MDCACHE_DECISION_OK) {
            final_decision = decision;
        }
    }

    if (book.used == 0U || book.notified == 0U) {
        fputs("配信対象なし\n", stderr);
        return MDCACHE_HARD_ERROR;
    }

    return final_decision;
}
