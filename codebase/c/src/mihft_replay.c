/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20230418  市場基盤部  初版作成、ジャーナル順序再生および状態照合を実装
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_REPLAY_OK 0
#define MIHFT_REPLAY_IOERR 20
#define MIHFT_REPLAY_PARSEERR 21
#define MIHFT_REPLAY_SEQGAP 22
#define MIHFT_REPLAY_HASHERR 23
#define MIHFT_REPLAY_STATEERR 24

#define MIHFT_MAX_LINE 1024
#define MIHFT_MAX_ORDERS 8192
#define MIHFT_MAX_EXEC 32768
#define MIHFT_ID_LEN 32
#define MIHFT_CODE_LEN 32
#define MIHFT_TS_LEN 32

typedef struct {
    uint64_t seq_no;
    char event_ts[MIHFT_TS_LEN];
    char event_kbn[16];
    char order_id[MIHFT_ID_LEN];
    char instr_code[MIHFT_CODE_LEN];
    uint32_t payload_hash;
} JournalRecord;

typedef struct {
    char order_id[MIHFT_ID_LEN];
    char cif_no[MIHFT_ID_LEN];
    char instr_code[MIHFT_CODE_LEN];
    char side_kbn;
    char ord_type;
    char tif_code[8];
    uint64_t ord_qty;
    uint64_t price_amt;
    int instr_tier;
    uint32_t payload_hash;
} OrderPayload;

typedef enum {
    STATE_NONE = 0,
    STATE_ACTIVE = 1,
    STATE_FILLED = 2,
    STATE_CANCELLED = 3,
    STATE_REJECTED = 4
} ReplayState;

typedef struct {
    OrderPayload payload;
    ReplayState state;
    uint64_t leaves_qty;
    uint64_t cum_qty;
    uint64_t avg_fill_amt;
    char last_upd_ts[MIHFT_TS_LEN];
    int seen_payload;
} OrderState;

typedef struct {
    char exec_id[MIHFT_ID_LEN];
    char order_id[MIHFT_ID_LEN];
    char instr_code[MIHFT_CODE_LEN];
    char side_kbn;
    uint64_t fill_qty;
    uint64_t fill_amt;
    char exec_ts[MIHFT_TS_LEN];
} ExecutionRecord;

typedef struct {
    OrderPayload items[MIHFT_MAX_ORDERS];
    size_t count;
} PayloadTable;

typedef struct {
    OrderState items[MIHFT_MAX_ORDERS];
    size_t count;
} StateTable;

typedef struct {
    ExecutionRecord items[MIHFT_MAX_EXEC];
    size_t count;
} ExecTable;

static void trim_eol(char *s)
{
    size_t n = strlen(s);
    while (n > 0U && (s[n - 1U] == '\n' || s[n - 1U] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t dst_len, const char *src)
{
    size_t n = strlen(src);
    if (n == 0U || n >= dst_len) {
        return -1;
    }
    memcpy(dst, src, n + 1U);
    return 0;
}

static int split_csv(char *line, char **cols, size_t need)
{
    size_t i = 0U;
    char *p = line;

    while (i < need) {
        cols[i++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return i == need && strchr(cols[need - 1U], ',') == NULL ? 0 : -1;
}

static int parse_u64(const char *s, uint64_t *out)
{
    char *end = NULL;
    unsigned long long v;

    if (*s == '\0' || *s == '-') {
        return -1;
    }
    errno = 0;
    v = strtoull(s, &end, 10);
    if (errno != 0 || *end != '\0') {
        return -1;
    }
    *out = (uint64_t)v;
    return 0;
}

static int parse_i32(const char *s, int *out)
{
    char *end = NULL;
    long v;

    if (*s == '\0') {
        return -1;
    }
    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || *end != '\0' || v < INT_MIN || v > INT_MAX) {
        return -1;
    }
    *out = (int)v;
    return 0;
}

static int parse_hash(const char *s, uint32_t *out)
{
    char *end = NULL;
    unsigned long v;

    if (*s == '\0') {
        return -1;
    }
    errno = 0;
    v = strtoul(s, &end, 0);
    if (errno != 0 || *end != '\0' || v > UINT32_MAX) {
        return -1;
    }
    *out = (uint32_t)v;
    return 0;
}

static uint32_t fnv1a_update(uint32_t h, const char *s)
{
    while (*s != '\0') {
        h ^= (unsigned char)*s++;
        h *= 16777619U;
    }
    return h;
}

static uint32_t payload_hash(const OrderPayload *p)
{
    char buf[128];
    uint32_t h = 2166136261U;

    h = fnv1a_update(h, p->order_id);
    h = fnv1a_update(h, ",");
    h = fnv1a_update(h, p->cif_no);
    h = fnv1a_update(h, ",");
    h = fnv1a_update(h, p->instr_code);
    h = fnv1a_update(h, ",");
    buf[0] = p->side_kbn;
    buf[1] = '\0';
    h = fnv1a_update(h, buf);
    h = fnv1a_update(h, ",");
    buf[0] = p->ord_type;
    buf[1] = '\0';
    h = fnv1a_update(h, buf);
    h = fnv1a_update(h, ",");
    h = fnv1a_update(h, p->tif_code);
    h = fnv1a_update(h, ",");
    (void)snprintf(buf, sizeof(buf), "%" PRIu64 ",%" PRIu64 ",%d",
                   p->ord_qty, p->price_amt, p->instr_tier);
    h = fnv1a_update(h, buf);
    return h;
}

static int tick_size(int tier, uint64_t *tick)
{
    if (tier == 1) {
        *tick = 100U;
        return 0;
    }
    if (tier == 2) {
        *tick = 500U;
        return 0;
    }
    if (tier == 3) {
        *tick = 1000U;
        return 0;
    }
    return -1;
}

static int notional(uint64_t qty, uint64_t price, uint64_t *out)
{
    if (qty != 0U && price > UINT64_MAX / qty) {
        return -1;
    }
    *out = qty * price;
    return 0;
}

static int validate_order(const OrderPayload *p)
{
    uint64_t tick = 0U;
    uint64_t amt = 0U;

    if (p->side_kbn != 'B' && p->side_kbn != 'S') {
        return 8;
    }
    if (p->ord_type != 'L' && p->ord_type != 'M') {
        return 8;
    }
    if (strcmp(p->tif_code, "DAY") != 0 &&
        strcmp(p->tif_code, "IOC") != 0 &&
        strcmp(p->tif_code, "FOK") != 0) {
        return 8;
    }
    if (p->ord_qty == 0U) {
        return 8;
    }
    if (notional(p->ord_qty, p->price_amt, &amt) != 0 || amt > MIHFT_MAX_NOTIONAL) {
        return 8;
    }
    if (tick_size(p->instr_tier, &tick) != 0) {
        return 4;
    }
    if (p->ord_type == 'L' && (p->price_amt == 0U || (p->price_amt % tick) != 0U)) {
        return 12;
    }
    return 0;
}

static const char *state_code(ReplayState state)
{
    if (state == STATE_ACTIVE) {
        return "A";
    }
    if (state == STATE_FILLED) {
        return "F";
    }
    if (state == STATE_CANCELLED) {
        return "C";
    }
    if (state == STATE_REJECTED) {
        return "R";
    }
    return "N";
}

static OrderPayload *find_payload(PayloadTable *table, const char *order_id)
{
    size_t i;
    for (i = 0U; i < table->count; i++) {
        if (strcmp(table->items[i].order_id, order_id) == 0) {
            return &table->items[i];
        }
    }
    return NULL;
}

static OrderState *find_state(StateTable *table, const char *order_id)
{
    size_t i;
    for (i = 0U; i < table->count; i++) {
        if (strcmp(table->items[i].payload.order_id, order_id) == 0) {
            return &table->items[i];
        }
    }
    return NULL;
}

static OrderState *add_state(StateTable *table, const OrderPayload *payload)
{
    OrderState *s;

    if (table->count >= MIHFT_MAX_ORDERS) {
        return NULL;
    }
    s = &table->items[table->count++];
    memset(s, 0, sizeof(*s));
    s->payload = *payload;
    s->state = STATE_NONE;
    s->seen_payload = 1;
    return s;
}

static int load_payloads(const char *path, PayloadTable *table)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_MAX_LINE];

    if (fp == NULL) {
        return -1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *cols[9];
        OrderPayload p;
        uint64_t qty;
        uint64_t price;
        int tier;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }
        if (split_csv(line, cols, 9U) != 0) {
            fclose(fp);
            return -2;
        }
        if (strcmp(cols[0], "ORDER-ID") == 0) {
            continue;
        }
        memset(&p, 0, sizeof(p));
        if (copy_field(p.order_id, sizeof(p.order_id), cols[0]) != 0 ||
            copy_field(p.cif_no, sizeof(p.cif_no), cols[1]) != 0 ||
            copy_field(p.instr_code, sizeof(p.instr_code), cols[2]) != 0 ||
            strlen(cols[3]) != 1U || strlen(cols[4]) != 1U ||
            copy_field(p.tif_code, sizeof(p.tif_code), cols[5]) != 0 ||
            parse_u64(cols[6], &qty) != 0 ||
            parse_u64(cols[7], &price) != 0 ||
            parse_i32(cols[8], &tier) != 0) {
            fclose(fp);
            return -2;
        }
        if (table->count >= MIHFT_MAX_ORDERS || find_payload(table, p.order_id) != NULL) {
            fclose(fp);
            return -2;
        }
        p.side_kbn = cols[3][0];
        p.ord_type = cols[4][0];
        p.ord_qty = qty;
        p.price_amt = price;
        p.instr_tier = tier;
        p.payload_hash = payload_hash(&p);
        table->items[table->count++] = p;
    }

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }
    fclose(fp);
    return 0;
}

static int parse_journal(char *line, JournalRecord *j)
{
    char *cols[6];

    if (split_csv(line, cols, 6U) != 0) {
        return -1;
    }
    if (strcmp(cols[0], "SEQ-NO") == 0) {
        return 1;
    }
    memset(j, 0, sizeof(*j));
    if (parse_u64(cols[0], &j->seq_no) != 0 ||
        copy_field(j->event_ts, sizeof(j->event_ts), cols[1]) != 0 ||
        copy_field(j->event_kbn, sizeof(j->event_kbn), cols[2]) != 0 ||
        copy_field(j->order_id, sizeof(j->order_id), cols[3]) != 0 ||
        copy_field(j->instr_code, sizeof(j->instr_code), cols[4]) != 0 ||
        parse_hash(cols[5], &j->payload_hash) != 0) {
        return -1;
    }
    return 0;
}

static int record_exec(ExecTable *execs, const OrderState *s, uint64_t qty, uint64_t amt, const char *ts)
{
    ExecutionRecord *e;

    if (execs->count >= MIHFT_MAX_EXEC) {
        return -1;
    }
    e = &execs->items[execs->count];
    memset(e, 0, sizeof(*e));
    (void)snprintf(e->exec_id, sizeof(e->exec_id), "E%012zu", execs->count + 1U);
    if (copy_field(e->order_id, sizeof(e->order_id), s->payload.order_id) != 0 ||
        copy_field(e->instr_code, sizeof(e->instr_code), s->payload.instr_code) != 0 ||
        copy_field(e->exec_ts, sizeof(e->exec_ts), ts) != 0) {
        return -1;
    }
    e->side_kbn = s->payload.side_kbn;
    e->fill_qty = qty;
    e->fill_amt = amt;
    execs->count++;
    return 0;
}

static int apply_event(const JournalRecord *j, const OrderPayload *p, StateTable *states, ExecTable *execs)
{
    OrderState *s = find_state(states, j->order_id);

    if (strcmp(j->instr_code, p->instr_code) != 0) {
        return MIHFT_REPLAY_STATEERR;
    }

    if (strcmp(j->event_kbn, "ENTRY") == 0 || strcmp(j->event_kbn, "ENT") == 0) {
        int decision;
        if (s != NULL && s->state != STATE_NONE) {
            return MIHFT_REPLAY_STATEERR;
        }
        s = add_state(states, p);
        if (s == NULL) {
            return MIHFT_REPLAY_STATEERR;
        }
        decision = validate_order(p);
        s->leaves_qty = decision == 0 ? p->ord_qty : 0U;
        s->cum_qty = 0U;
        s->avg_fill_amt = 0U;
        s->state = decision == 0 ? STATE_ACTIVE : STATE_REJECTED;
        if (copy_field(s->last_upd_ts, sizeof(s->last_upd_ts), j->event_ts) != 0) {
            return MIHFT_REPLAY_STATEERR;
        }
        return decision;
    }

    if (s == NULL || s->state != STATE_ACTIVE) {
        return MIHFT_REPLAY_STATEERR;
    }

    if (strcmp(j->event_kbn, "FILL") == 0 || strcmp(j->event_kbn, "EXE") == 0) {
        uint64_t fill_qty = s->leaves_qty;
        uint64_t gross_amt;
        uint64_t new_cum;
        uint64_t total_amt;

        if (fill_qty == 0U || notional(fill_qty, p->price_amt, &gross_amt) != 0) {
            return MIHFT_REPLAY_STATEERR;
        }
        if (UINT64_MAX - s->cum_qty < fill_qty) {
            return MIHFT_REPLAY_STATEERR;
        }
        new_cum = s->cum_qty + fill_qty;
        total_amt = s->avg_fill_amt * s->cum_qty;
        if (fill_qty != 0U && p->price_amt > (UINT64_MAX - total_amt) / fill_qty) {
            return MIHFT_REPLAY_STATEERR;
        }
        total_amt += p->price_amt * fill_qty;
        s->cum_qty = new_cum;
        s->leaves_qty = 0U;
        s->avg_fill_amt = new_cum == 0U ? 0U : total_amt / new_cum;
        s->state = STATE_FILLED;
        if (copy_field(s->last_upd_ts, sizeof(s->last_upd_ts), j->event_ts) != 0 ||
            record_exec(execs, s, fill_qty, p->price_amt, j->event_ts) != 0) {
            return MIHFT_REPLAY_STATEERR;
        }
        (void)gross_amt;
        return 0;
    }

    if (strcmp(j->event_kbn, "CANCEL") == 0 || strcmp(j->event_kbn, "CXL") == 0) {
        if (s->leaves_qty == 0U) {
            return MIHFT_REPLAY_STATEERR;
        }
        s->leaves_qty = 0U;
        s->state = STATE_CANCELLED;
        if (copy_field(s->last_upd_ts, sizeof(s->last_upd_ts), j->event_ts) != 0) {
            return MIHFT_REPLAY_STATEERR;
        }
        return 0;
    }

    if (strcmp(j->event_kbn, "MODIFY") == 0 || strcmp(j->event_kbn, "MOD") == 0) {
        uint64_t old_cum = s->cum_qty;
        int decision = validate_order(p);

        if (decision != 0 || p->ord_qty < old_cum) {
            return MIHFT_REPLAY_STATEERR;
        }
        s->payload = *p;
        s->leaves_qty = p->ord_qty - old_cum;
        s->state = s->leaves_qty == 0U ? STATE_FILLED : STATE_ACTIVE;
        if (copy_field(s->last_upd_ts, sizeof(s->last_upd_ts), j->event_ts) != 0) {
            return MIHFT_REPLAY_STATEERR;
        }
        return 0;
    }

    return MIHFT_REPLAY_STATEERR;
}

static int write_states(const char *path, const StateTable *states)
{
    FILE *fp = fopen(path, "w");
    size_t i;

    if (fp == NULL) {
        return -1;
    }
    if (fprintf(fp, "ORDER-ID,CIF-NO,INSTR-CODE,STATE-KBN,LEAVES-QTY,CUM-QTY,AVG-FILL-AMT,LAST-UPD-TS\n") < 0) {
        fclose(fp);
        return -1;
    }
    for (i = 0U; i < states->count; i++) {
        const OrderState *s = &states->items[i];
        if (fprintf(fp, "%s,%s,%s,%s,%" PRIu64 ",%" PRIu64 ",%" PRIu64 ",%s\n",
                    s->payload.order_id, s->payload.cif_no, s->payload.instr_code,
                    state_code(s->state), s->leaves_qty, s->cum_qty,
                    s->avg_fill_amt, s->last_upd_ts) < 0) {
            fclose(fp);
            return -1;
        }
    }
    if (fclose(fp) != 0) {
        return -1;
    }
    return 0;
}

static int write_execs(const char *path, const ExecTable *execs)
{
    FILE *fp = fopen(path, "w");
    size_t i;

    if (fp == NULL) {
        return -1;
    }
    if (fprintf(fp, "EXEC-ID,ORDER-ID,INSTR-CODE,SIDE-KBN,FILL-QTY,FILL-AMT,EXEC-TS\n") < 0) {
        fclose(fp);
        return -1;
    }
    for (i = 0U; i < execs->count; i++) {
        const ExecutionRecord *e = &execs->items[i];
        if (fprintf(fp, "%s,%s,%s,%c,%" PRIu64 ",%" PRIu64 ",%s\n",
                    e->exec_id, e->order_id, e->instr_code, e->side_kbn,
                    e->fill_qty, e->fill_amt, e->exec_ts) < 0) {
            fclose(fp);
            return -1;
        }
    }
    if (fclose(fp) != 0) {
        return -1;
    }
    return 0;
}

int main(void)
{
    PayloadTable payloads;
    StateTable states;
    ExecTable execs;
    FILE *journal;
    char line[MIHFT_MAX_LINE];
    uint64_t expect_seq = 0U;
    int final_code = 0;

    memset(&payloads, 0, sizeof(payloads));
    memset(&states, 0, sizeof(states));
    memset(&execs, 0, sizeof(execs));

    if (load_payloads("SCORDF.csv", &payloads) != 0 && load_payloads("SCORDF", &payloads) != 0) {
        fprintf(stderr, "SCORDF読込失敗\n");
        return MIHFT_REPLAY_IOERR;
    }

    journal = fopen("SCJRNF.csv", "r");
    if (journal == NULL) {
        journal = fopen("SCJRNF", "r");
    }
    if (journal == NULL) {
        fprintf(stderr, "SCJRNF読込失敗\n");
        return MIHFT_REPLAY_IOERR;
    }

    while (fgets(line, sizeof(line), journal) != NULL) {
        JournalRecord j;
        OrderPayload *p;
        int rc;

        trim_eol(line);
        if (line[0] == '\0') {
            continue;
        }

        rc = parse_journal(line, &j);
        if (rc > 0) {
            continue;
        }
        if (rc < 0) {
            fprintf(stderr, "SCJRNF解析失敗\n");
            fclose(journal);
            return MIHFT_REPLAY_PARSEERR;
        }

        if (expect_seq == 0U) {
            expect_seq = j.seq_no;
        }
        if (j.seq_no != expect_seq) {
            fprintf(stderr, "SEQ不連続:%" PRIu64 "\n", j.seq_no);
            fclose(journal);
            return MIHFT_REPLAY_SEQGAP;
        }
        expect_seq++;

        p = find_payload(&payloads, j.order_id);
        if (p == NULL) {
            fprintf(stderr, "PAYLOAD未検出:%s\n", j.order_id);
            fclose(journal);
            return MIHFT_REPLAY_STATEERR;
        }
        if (p->payload_hash != j.payload_hash) {
            fprintf(stderr, "PAYLOAD-HASH不一致:%s\n", j.order_id);
            fclose(journal);
            return MIHFT_REPLAY_HASHERR;
        }

        rc = apply_event(&j, p, &states, &execs);
        if (rc == MIHFT_REPLAY_STATEERR) {
            fprintf(stderr, "状態遷移不一致:%s\n", j.order_id);
            fclose(journal);
            return MIHFT_REPLAY_STATEERR;
        }
        final_code = rc;
    }

    if (ferror(journal)) {
        fprintf(stderr, "SCJRNF読込中断\n");
        fclose(journal);
        return MIHFT_REPLAY_IOERR;
    }
    if (fclose(journal) != 0) {
        fprintf(stderr, "SCJRNF終了失敗\n");
        return MIHFT_REPLAY_IOERR;
    }

    if (write_states("SCORDS.csv", &states) != 0) {
        fprintf(stderr, "SCORDS書込失敗\n");
        return MIHFT_REPLAY_IOERR;
    }
    if (write_execs("SCEXEC.csv", &execs) != 0) {
        fprintf(stderr, "SCEXEC書込失敗\n");
        return MIHFT_REPLAY_IOERR;
    }

    return final_code;
}
