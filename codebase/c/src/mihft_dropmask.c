/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20191022  中川 美和 (E-283)  初版作成
 * 1.01  20200322  福田 亮太 (E-211)  約定状態別の送信区分と項目マスク判定を追加
 */

#include "mihft_types.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DROP_MASK_EXEC_ID     UINT32_C(0x00000001)
#define DROP_MASK_ORDER_ID    UINT32_C(0x00000002)
#define DROP_MASK_INSTR_CODE  UINT32_C(0x00000004)
#define DROP_MASK_SIDE        UINT32_C(0x00000008)
#define DROP_MASK_FILL_QTY    UINT32_C(0x00000010)
#define DROP_MASK_FILL_AMT    UINT32_C(0x00000020)
#define DROP_MASK_EXEC_TS     UINT32_C(0x00000040)
#define DROP_MASK_BUY_MARK    UINT32_C(0x00000100)
#define DROP_MASK_SELL_MARK   UINT32_C(0x00000200)
#define DROP_MASK_CANCEL_MARK UINT32_C(0x00000400)

#define SEND_CLASS_NORMAL "0"
#define SEND_CLASS_ZERO   "1"
#define SEND_CLASS_CANCEL "2"
#define SEND_CLASS_ALERT  "3"

#define RET_REJECT_NOTIONAL 8
#define RET_REJECT_TICK     12
#define RET_PARSE_ERROR     64
#define RET_IO_ERROR        65

#define FIELD_COUNT 7
#define LINE_SIZE 1024
#define EXEC_ID_SIZE 32
#define ORDER_ID_SIZE 32
#define INSTR_CODE_SIZE 32
#define EXEC_TS_SIZE 40

typedef struct {
    char exec_id[EXEC_ID_SIZE];
    char order_id[ORDER_ID_SIZE];
    char instr_code[INSTR_CODE_SIZE];
    char side_kbn;
    uint64_t fill_qty;
    uint64_t fill_amt;
    char exec_ts[EXEC_TS_SIZE];
} scexec_record;

static char *trim_field(char *s)
{
    unsigned char *p = (unsigned char *)s;
    char *e;

    while (*p != '\0' && isspace(*p)) {
        ++p;
    }

    e = (char *)p + strlen((char *)p);
    while (e > (char *)p && isspace((unsigned char)e[-1])) {
        --e;
    }
    *e = '\0';

    return (char *)p;
}

static int copy_token(char *dst, size_t dst_size, const char *src)
{
    size_t n = strlen(src);

    if (n == 0 || n >= dst_size) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_u64(const char *s, uint64_t *out)
{
    char *endp;
    unsigned long long v;

    if (*s == '\0' || *s == '-') {
        return -1;
    }

    errno = 0;
    v = strtoull(s, &endp, 10);
    if (errno == ERANGE || *endp != '\0') {
        return -1;
    }

    *out = (uint64_t)v;
    return 0;
}

static int parse_csv_line(char *line, scexec_record *rec)
{
    char *fields[FIELD_COUNT];
    char *p = line;
    size_t i;

    for (i = 0; i < FIELD_COUNT; ++i) {
        fields[i] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            if (i + 1 != FIELD_COUNT) {
                return -1;
            }
            break;
        }
        *p++ = '\0';
    }

    if (strchr(fields[FIELD_COUNT - 1], ',') != NULL) {
        return -1;
    }

    for (i = 0; i < FIELD_COUNT; ++i) {
        fields[i] = trim_field(fields[i]);
    }

    if (copy_token(rec->exec_id, sizeof(rec->exec_id), fields[0]) != 0 ||
        copy_token(rec->order_id, sizeof(rec->order_id), fields[1]) != 0 ||
        copy_token(rec->instr_code, sizeof(rec->instr_code), fields[2]) != 0 ||
        copy_token(rec->exec_ts, sizeof(rec->exec_ts), fields[6]) != 0) {
        return -1;
    }

    if ((fields[3][0] != 'B' && fields[3][0] != 'S') || fields[3][1] != '\0') {
        return -1;
    }
    rec->side_kbn = fields[3][0];

    if (parse_u64(fields[4], &rec->fill_qty) != 0 ||
        parse_u64(fields[5], &rec->fill_amt) != 0) {
        return -1;
    }

    return 0;
}

static int is_header_line(const char *line)
{
    return strncmp(line, "EXEC-ID,", 8) == 0;
}

static int is_timestamp_shape(const char *s)
{
    size_t n = strlen(s);

    if (n < 14 || n >= EXEC_TS_SIZE) {
        return 0;
    }

    return isdigit((unsigned char)s[0]) &&
           isdigit((unsigned char)s[1]) &&
           isdigit((unsigned char)s[2]) &&
           isdigit((unsigned char)s[3]);
}

static uint32_t select_drop_mask(const scexec_record *rec)
{
    uint32_t mask = DROP_MASK_EXEC_ID |
                    DROP_MASK_ORDER_ID |
                    DROP_MASK_INSTR_CODE |
                    DROP_MASK_SIDE |
                    DROP_MASK_EXEC_TS;

    if (rec->fill_qty > 0) {
        mask |= DROP_MASK_FILL_QTY;
    }

    if (rec->fill_amt > 0) {
        mask |= DROP_MASK_FILL_AMT;
    }

    if (rec->side_kbn == 'B') {
        mask |= DROP_MASK_BUY_MARK;
    } else {
        mask |= DROP_MASK_SELL_MARK;
    }

    if (rec->fill_qty == 0 && rec->fill_amt == 0) {
        mask |= DROP_MASK_CANCEL_MARK;
    }

    return mask;
}

static const char *select_send_class(const scexec_record *rec)
{
    if (rec->fill_qty == 0 && rec->fill_amt == 0) {
        return SEND_CLASS_CANCEL;
    }

    if (rec->fill_qty == 0 || rec->fill_amt == 0) {
        return SEND_CLASS_ZERO;
    }

    if (rec->fill_amt > (uint64_t)MIHFT_MAX_NOTIONAL) {
        return SEND_CLASS_ALERT;
    }

    return SEND_CLASS_NORMAL;
}

static int decide_record(const scexec_record *rec)
{
    if (!is_timestamp_shape(rec->exec_ts)) {
        return RET_REJECT_TICK;
    }

    if (rec->fill_amt > (uint64_t)MIHFT_MAX_NOTIONAL) {
        return RET_REJECT_NOTIONAL;
    }

    return 0;
}

int main(void)
{
    char line[LINE_SIZE];
    int final_decision = 0;
    unsigned long row_no = 0;
    int emitted = 0;

    while (fgets(line, sizeof(line), stdin) != NULL) {
        scexec_record rec;
        uint32_t mask;
        int decision;
        size_t len = strlen(line);

        ++row_no;

        if (len > 0 && line[len - 1] != '\n' && !feof(stdin)) {
            fprintf(stderr, "入力行が長すぎます: %lu\n", row_no);
            return RET_PARSE_ERROR;
        }

        while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r')) {
            line[--len] = '\0';
        }

        if (len == 0) {
            continue;
        }

        if (row_no == 1 && is_header_line(line)) {
            continue;
        }

        if (parse_csv_line(line, &rec) != 0) {
            fprintf(stderr, "入力形式不正: %lu\n", row_no);
            return RET_PARSE_ERROR;
        }

        decision = decide_record(&rec);
        mask = select_drop_mask(&rec);

        printf("%s,%s,%08" PRIX32 ",%s,%d\n",
               rec.exec_id,
               rec.order_id,
               mask,
               select_send_class(&rec),
               decision);

        emitted = 1;

        if (decision != 0 && final_decision == 0) {
            final_decision = decision;
        }
    }

    if (ferror(stdin)) {
        fprintf(stderr, "入力読取失敗\n");
        return RET_IO_ERROR;
    }

    if (!emitted) {
        fprintf(stderr, "有効な入力がありません\n");
        return RET_PARSE_ERROR;
    }

    return final_decision;
}
