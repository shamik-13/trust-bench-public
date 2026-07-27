/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20250603  三宅 拓也 (E-241)  初版作成
 * 1.01  20251103  岡本 涼 (E-294)  ジャーナル再読込検査を追加
 * 1.02  20250603  岡本 涼 (E-294)  採番上限と順序検証を強化
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MIHFT_SEQ_NORMAL_CODE
# ifdef MIHFT_DECISION_ACCEPT
#  define MIHFT_SEQ_NORMAL_CODE MIHFT_DECISION_ACCEPT
# elif defined(MIHFT_ACCEPT)
#  define MIHFT_SEQ_NORMAL_CODE MIHFT_ACCEPT
# elif defined(MIHFT_DECISION_OK)
#  define MIHFT_SEQ_NORMAL_CODE MIHFT_DECISION_OK
# elif defined(MIHFT_OK)
#  define MIHFT_SEQ_NORMAL_CODE MIHFT_OK
# elif defined(DECISION_ACCEPT)
#  define MIHFT_SEQ_NORMAL_CODE DECISION_ACCEPT
# elif defined(DECISION_OK)
#  define MIHFT_SEQ_NORMAL_CODE DECISION_OK
# else
#  define MIHFT_SEQ_NORMAL_CODE 0
# endif
#endif

#define MIHFT_SEQ_IO_ERROR 71
#define MIHFT_SEQ_DATA_ERROR 72
#define MIHFT_SEQ_LIMIT_ERROR 73

#define MIHFT_SEQ_MAX_ORDERS 16u
#define MIHFT_SEQ_ACCOUNT_LEN 12u
#define MIHFT_SEQ_SYMBOL_LEN 12u
#define MIHFT_SEQ_SIDE_LEN 2u
#define MIHFT_SEQ_JOURNAL_LINE 192u

typedef struct {
    uint64_t arrival_ns;
    char account[MIHFT_SEQ_ACCOUNT_LEN];
    char symbol[MIHFT_SEQ_SYMBOL_LEN];
    char side[MIHFT_SEQ_SIDE_LEN];
    uint32_t quantity;
    uint32_t price;
} mihft_seq_in_order;

typedef struct {
    uint64_t sequence_id;
    uint64_t arrival_ns;
    char account[MIHFT_SEQ_ACCOUNT_LEN];
    char symbol[MIHFT_SEQ_SYMBOL_LEN];
    char side;
    uint32_t quantity;
    uint32_t price;
    uint64_t notional;
} mihft_seq_record;

static int mihft_seq_copy_text(char *dst, size_t dst_len, const char *src)
{
    size_t len;

    if (dst == NULL || src == NULL || dst_len == 0u) {
        return -1;
    }

    len = strlen(src);
    if (len == 0u || len >= dst_len) {
        return -1;
    }

    memcpy(dst, src, len + 1u);
    return 0;
}

static int mihft_seq_make_record(const mihft_seq_in_order *in,
                                 uint64_t seq,
                                 mihft_seq_record *out)
{
    uint64_t notional;

    if (in == NULL || out == NULL) {
        return -1;
    }
    if (seq == 0u || in->arrival_ns == 0u) {
        return -1;
    }
    if (in->quantity == 0u || in->price == 0u) {
        return -1;
    }
    if (in->side[0] != 'B' && in->side[0] != 'S') {
        return -1;
    }
    if (in->side[1] != '\0') {
        return -1;
    }
    if ((uint64_t)in->quantity > UINT64_MAX / (uint64_t)in->price) {
        return -1;
    }

    notional = (uint64_t)in->quantity * (uint64_t)in->price;

    memset(out, 0, sizeof(*out));
    out->sequence_id = seq;
    out->arrival_ns = in->arrival_ns;
    out->side = in->side[0];
    out->quantity = in->quantity;
    out->price = in->price;
    out->notional = notional;

    if (mihft_seq_copy_text(out->account, sizeof(out->account), in->account) != 0) {
        return -1;
    }
    if (mihft_seq_copy_text(out->symbol, sizeof(out->symbol), in->symbol) != 0) {
        return -1;
    }

    return 0;
}

static int mihft_seq_write_journal(FILE *fp, const mihft_seq_record *rec)
{
    int written;

    if (fp == NULL || rec == NULL) {
        return -1;
    }

    written = fprintf(fp,
                      "%" PRIu64 ",%" PRIu64 ",%s,%s,%c,%" PRIu32 ",%" PRIu32 ",%" PRIu64 "\n",
                      rec->sequence_id,
                      rec->arrival_ns,
                      rec->account,
                      rec->symbol,
                      rec->side,
                      rec->quantity,
                      rec->price,
                      rec->notional);
    if (written <= 0 || written >= (int)MIHFT_SEQ_JOURNAL_LINE) {
        return -1;
    }

    return ferror(fp) ? -1 : 0;
}

static int mihft_seq_read_u64(const char *text, uint64_t *value)
{
    char *endp;
    unsigned long long parsed;

    if (text == NULL || value == NULL || text[0] == '\0') {
        return -1;
    }

    errno = 0;
    parsed = strtoull(text, &endp, 10);
    if (errno != 0 || endp == text || *endp != '\0') {
        return -1;
    }

    *value = (uint64_t)parsed;
    return 0;
}

static int mihft_seq_read_u32(const char *text, uint32_t *value)
{
    uint64_t wide;

    if (mihft_seq_read_u64(text, &wide) != 0 || wide > UINT32_MAX) {
        return -1;
    }

    *value = (uint32_t)wide;
    return 0;
}

static int mihft_seq_parse_line(char *line, mihft_seq_record *rec)
{
    char *field[8];
    char *cursor;
    size_t i;

    if (line == NULL || rec == NULL) {
        return -1;
    }

    cursor = line;
    for (i = 0u; i < 8u; i++) {
        field[i] = strsep(&cursor, ",");
        if (field[i] == NULL || field[i][0] == '\0') {
            return -1;
        }
    }
    if (cursor != NULL) {
        return -1;
    }

    field[7][strcspn(field[7], "\r\n")] = '\0';

    memset(rec, 0, sizeof(*rec));
    if (mihft_seq_read_u64(field[0], &rec->sequence_id) != 0) {
        return -1;
    }
    if (mihft_seq_read_u64(field[1], &rec->arrival_ns) != 0) {
        return -1;
    }
    if (mihft_seq_copy_text(rec->account, sizeof(rec->account), field[2]) != 0) {
        return -1;
    }
    if (mihft_seq_copy_text(rec->symbol, sizeof(rec->symbol), field[3]) != 0) {
        return -1;
    }
    if ((field[4][0] != 'B' && field[4][0] != 'S') || field[4][1] != '\0') {
        return -1;
    }
    rec->side = field[4][0];

    if (mihft_seq_read_u32(field[5], &rec->quantity) != 0) {
        return -1;
    }
    if (mihft_seq_read_u32(field[6], &rec->price) != 0) {
        return -1;
    }
    if (mihft_seq_read_u64(field[7], &rec->notional) != 0) {
        return -1;
    }
    if (rec->quantity == 0u || rec->price == 0u) {
        return -1;
    }
    if ((uint64_t)rec->quantity > UINT64_MAX / (uint64_t)rec->price) {
        return -1;
    }
    if (rec->notional != (uint64_t)rec->quantity * (uint64_t)rec->price) {
        return -1;
    }

    return 0;
}

static int mihft_seq_handoff(FILE *fp, size_t expected_count)
{
    char line[MIHFT_SEQ_JOURNAL_LINE];
    mihft_seq_record rec;
    uint64_t prior_seq;
    uint64_t prior_arrival;
    size_t count;

    if (fp == NULL) {
        return -1;
    }
    if (fseek(fp, 0L, SEEK_SET) != 0) {
        return -1;
    }

    prior_seq = 0u;
    prior_arrival = 0u;
    count = 0u;

    while (fgets(line, sizeof(line), fp) != NULL) {
        if (strchr(line, '\n') == NULL) {
            return -1;
        }
        if (mihft_seq_parse_line(line, &rec) != 0) {
            return -1;
        }
        if (rec.sequence_id != prior_seq + 1u) {
            return -1;
        }
        if (prior_arrival != 0u && rec.arrival_ns < prior_arrival) {
            return -1;
        }

        prior_seq = rec.sequence_id;
        prior_arrival = rec.arrival_ns;
        count++;
    }

    if (ferror(fp)) {
        return -1;
    }

    return count == expected_count ? 0 : -1;
}

int main(void)
{
    static const mihft_seq_in_order staged[] = {
        {1719367200000000100ULL, "KABU01", "7203", "B", 300u, 321500u},
        {1719367200000000100ULL, "KABU02", "7203", "S", 100u, 321600u},
        {1719367200000000115ULL, "KABU01", "9984", "B", 200u, 1042500u},
        {1719367200000000121ULL, "KABU03", "6758", "B", 500u, 132950u},
        {1719367200000000122ULL, "KABU02", "8306", "S", 1000u, 15890u},
        {1719367200000000135ULL, "KABU04", "6861", "B", 100u, 642000u}
    };
    mihft_seq_record rec;
    FILE *journal;
    uint64_t next_seq;
    size_t i;
    int status;

    if (sizeof(staged) / sizeof(staged[0]) > MIHFT_SEQ_MAX_ORDERS) {
        return MIHFT_SEQ_LIMIT_ERROR;
    }

    journal = tmpfile();
    if (journal == NULL) {
        return MIHFT_SEQ_IO_ERROR;
    }

    next_seq = 1u;
    status = MIHFT_SEQ_NORMAL_CODE;

    for (i = 0u; i < sizeof(staged) / sizeof(staged[0]); i++) {
        if (next_seq == UINT64_MAX) {
            status = MIHFT_SEQ_LIMIT_ERROR;
            break;
        }
        if (mihft_seq_make_record(&staged[i], next_seq, &rec) != 0) {
            status = MIHFT_SEQ_DATA_ERROR;
            break;
        }
        if (mihft_seq_write_journal(journal, &rec) != 0) {
            status = MIHFT_SEQ_IO_ERROR;
            break;
        }
        next_seq++;
    }

    if (status == MIHFT_SEQ_NORMAL_CODE) {
        if (fflush(journal) != 0) {
            status = MIHFT_SEQ_IO_ERROR;
        } else if (mihft_seq_handoff(journal, sizeof(staged) / sizeof(staged[0])) != 0) {
            status = MIHFT_SEQ_DATA_ERROR;
        }
    }

    if (fclose(journal) != 0 && status == MIHFT_SEQ_NORMAL_CODE) {
        status = MIHFT_SEQ_IO_ERROR;
    }

    return status;
}
