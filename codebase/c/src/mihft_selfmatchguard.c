/**************************************************************
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20250121  三宅 拓也 (E-241)  初版作成
 * 1.01  20250621  渡辺 隆 (E-260)  CSV桁あふれ検査を追加
 **************************************************************/

#include "mihft_types.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIHFT_RC_IOERR 16
#define MIHFT_RC_PARSEERR 20
#define MIHFT_LINE_MAX 512
#define MIHFT_SCBOOK_MAX 2048
#define MIHFT_SCORDF_MAX 1024
#define MIHFT_FIELD_MAX 16

typedef struct {
    char instr_code[32];
    char side_kbn;
    int level_cnt;
    int64_t price_amt;
    int64_t book_qty;
    int order_cnt;
    char entry_ts[32];
} MihftBookRow;

typedef struct {
    char order_id[32];
    char cif_no[32];
    char instr_code[32];
    char side_kbn;
    char ord_type;
    char tif_code[4];
    int64_t ord_qty;
    int64_t price_amt;
    int instr_tier;
} MihftOrderRow;

typedef struct {
    const MihftBookRow *book;
    int64_t overlap_qty;
} MihftCandidate;

static void mihft_chomp(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int mihft_copy_field(char *dst, size_t dstsz, const char *src)
{
    size_t n = strlen(src);
    if (n + 1 > dstsz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int mihft_split_csv(char *line, char **fields, int expected)
{
    int n = 0;
    char *p = line;

    while (n < expected) {
        fields[n++] = p;
        p = strchr(p, ',');
        if (p == NULL) {
            break;
        }
        *p++ = '\0';
    }
    return n == expected && strchr(fields[expected - 1], ',') == NULL ? 0 : -1;
}

static int mihft_parse_i64(const char *s, int64_t *out)
{
    char *end = NULL;
    long long v;

    if (s[0] == '\0') {
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

static int mihft_side_ok(char c)
{
    return c == 'B' || c == 'S';
}

static int mihft_order_type_ok(char c)
{
    return c == 'L' || c == 'M';
}

static int mihft_tif_ok(const char *s)
{
    return strcmp(s, "DAY") == 0 || strcmp(s, "IOC") == 0 || strcmp(s, "FOK") == 0;
}

static int mihft_tier_ok(int tier)
{
    return tier == 1 || tier == 2 || tier == 3;
}

static char mihft_opposite_side(char side)
{
    return side == 'B' ? 'S' : 'B';
}

static int mihft_price_crosses(const MihftOrderRow *ord, const MihftBookRow *book)
{
    if (ord->ord_type == 'M') {
        return 1;
    }
    if (ord->side_kbn == 'B') {
        return ord->price_amt >= book->price_amt;
    }
    return ord->price_amt <= book->price_amt;
}

static int mihft_read_scbook(const char *path, MihftBookRow *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];
    size_t n = 0;
    int lineno = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCBOOKを開けません:%s\n", path);
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[7];
        MihftBookRow r;

        lineno++;
        mihft_chomp(line);
        if (lineno == 1 && strncmp(line, "INSTR-CODE,", 11) == 0) {
            continue;
        }
        if (line[0] == '\0') {
            continue;
        }
        if (n >= cap || mihft_split_csv(line, f, 7) != 0) {
            fprintf(stderr, "SCBOOK形式不正:%d\n", lineno);
            fclose(fp);
            return -1;
        }

        memset(&r, 0, sizeof r);
        if (mihft_copy_field(r.instr_code, sizeof r.instr_code, f[0]) != 0 ||
            strlen(f[1]) != 1 ||
            mihft_parse_int(f[2], &r.level_cnt) != 0 ||
            mihft_parse_i64(f[3], &r.price_amt) != 0 ||
            mihft_parse_i64(f[4], &r.book_qty) != 0 ||
            mihft_parse_int(f[5], &r.order_cnt) != 0 ||
            mihft_copy_field(r.entry_ts, sizeof r.entry_ts, f[6]) != 0) {
            fprintf(stderr, "SCBOOK値不正:%d\n", lineno);
            fclose(fp);
            return -1;
        }

        r.side_kbn = f[1][0];
        if (!mihft_side_ok(r.side_kbn) || r.level_cnt < 1 ||
            r.price_amt <= 0 || r.book_qty <= 0 || r.order_cnt <= 0) {
            fprintf(stderr, "SCBOOK範囲不正:%d\n", lineno);
            fclose(fp);
            return -1;
        }

        rows[n++] = r;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCBOOK読込失敗\n");
        fclose(fp);
        return -1;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static int mihft_read_scordf(const char *path, MihftOrderRow *rows, size_t cap, size_t *count)
{
    FILE *fp = fopen(path, "r");
    char line[MIHFT_LINE_MAX];
    size_t n = 0;
    int lineno = 0;

    if (fp == NULL) {
        fprintf(stderr, "SCORDFを開けません:%s\n", path);
        return -1;
    }

    while (fgets(line, sizeof line, fp) != NULL) {
        char *f[9];
        MihftOrderRow r;

        lineno++;
        mihft_chomp(line);
        if (lineno == 1 && strncmp(line, "ORDER-ID,", 9) == 0) {
            continue;
        }
        if (line[0] == '\0') {
            continue;
        }
        if (n >= cap || mihft_split_csv(line, f, 9) != 0) {
            fprintf(stderr, "SCORDF形式不正:%d\n", lineno);
            fclose(fp);
            return -1;
        }

        memset(&r, 0, sizeof r);
        if (mihft_copy_field(r.order_id, sizeof r.order_id, f[0]) != 0 ||
            mihft_copy_field(r.cif_no, sizeof r.cif_no, f[1]) != 0 ||
            mihft_copy_field(r.instr_code, sizeof r.instr_code, f[2]) != 0 ||
            strlen(f[3]) != 1 ||
            strlen(f[4]) != 1 ||
            mihft_copy_field(r.tif_code, sizeof r.tif_code, f[5]) != 0 ||
            mihft_parse_i64(f[6], &r.ord_qty) != 0 ||
            mihft_parse_i64(f[7], &r.price_amt) != 0 ||
            mihft_parse_int(f[8], &r.instr_tier) != 0) {
            fprintf(stderr, "SCORDF値不正:%d\n", lineno);
            fclose(fp);
            return -1;
        }

        r.side_kbn = f[3][0];
        r.ord_type = f[4][0];
        if (!mihft_side_ok(r.side_kbn) || !mihft_order_type_ok(r.ord_type) ||
            !mihft_tif_ok(r.tif_code) || !mihft_tier_ok(r.instr_tier) ||
            r.ord_qty <= 0 || (r.ord_type == 'L' && r.price_amt <= 0)) {
            fprintf(stderr, "SCORDF範囲不正:%d\n", lineno);
            fclose(fp);
            return -1;
        }

        rows[n++] = r;
    }

    if (ferror(fp)) {
        fprintf(stderr, "SCORDF読込失敗\n");
        fclose(fp);
        return -1;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static int mihft_same_instr(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

static int mihft_pick_candidate(const MihftOrderRow *ord,
                                const MihftBookRow *book,
                                size_t book_count,
                                MihftCandidate *cand)
{
    size_t i;
    const MihftBookRow *best = NULL;

    cand->book = NULL;
    cand->overlap_qty = 0;

    for (i = 0; i < book_count; i++) {
        const MihftBookRow *b = &book[i];

        if (!mihft_same_instr(ord->instr_code, b->instr_code) ||
            b->side_kbn != mihft_opposite_side(ord->side_kbn) ||
            !mihft_price_crosses(ord, b)) {
            continue;
        }

        if (best == NULL ||
            (ord->side_kbn == 'B' && b->price_amt < best->price_amt) ||
            (ord->side_kbn == 'S' && b->price_amt > best->price_amt)) {
            best = b;
        }
    }

    if (best == NULL) {
        return 0;
    }

    cand->book = best;
    cand->overlap_qty = ord->ord_qty < best->book_qty ? ord->ord_qty : best->book_qty;
    return cand->overlap_qty > 0;
}

static int mihft_make_decision_id(char *buf, size_t bufsz, unsigned long seq)
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_WIN32)
    if (localtime_s(&tmv, &now) != 0) {
        return -1;
    }
#else
    if (localtime_r(&now, &tmv) == NULL) {
        return -1;
    }
#endif

    if (snprintf(buf, bufsz, "D%04d%02d%02d%02d%02d%02d%06lu",
                 tmv.tm_year + 1900, tmv.tm_mon + 1, tmv.tm_mday,
                 tmv.tm_hour, tmv.tm_min, tmv.tm_sec, seq) >= (int)bufsz) {
        return -1;
    }
    return 0;
}

static int mihft_make_ts(char *buf, size_t bufsz)
{
    time_t now = time(NULL);
    struct tm tmv;

#if defined(_WIN32)
    if (localtime_s(&tmv, &now) != 0) {
        return -1;
    }
#else
    if (localtime_r(&now, &tmv) == NULL) {
        return -1;
    }
#endif

    if (snprintf(buf, bufsz, "%04d%02d%02d%02d%02d%02d",
                 tmv.tm_year + 1900, tmv.tm_mon + 1, tmv.tm_mday,
                 tmv.tm_hour, tmv.tm_min, tmv.tm_sec) >= (int)bufsz) {
        return -1;
    }
    return 0;
}

static int mihft_write_hfdeclog(FILE *fp,
                                unsigned long seq,
                                const MihftOrderRow *ord,
                                int action_code,
                                const char *reason_code)
{
    char decision_id[32];
    char ts[16];

    if (mihft_make_decision_id(decision_id, sizeof decision_id, seq) != 0 ||
        mihft_make_ts(ts, sizeof ts) != 0) {
        fprintf(stderr, "時刻生成失敗\n");
        return -1;
    }

    if (fprintf(fp, "%s,%s,%s,%d,%s,%s\n",
                decision_id, ord->order_id, ord->instr_code,
                action_code, reason_code, ts) < 0) {
        fprintf(stderr, "HFDECLOG書込失敗\n");
        return -1;
    }
    return 0;
}

static int mihft_process(FILE *logfp,
                         const MihftBookRow *book,
                         size_t book_count,
                         const MihftOrderRow *orders,
                         size_t order_count)
{
    size_t i;
    int final_code = 0;
    unsigned long seq = 1;

    for (i = 0; i < order_count; i++) {
        MihftCandidate cand;
        const MihftOrderRow *ord = &orders[i];

        if (mihft_pick_candidate(ord, book, book_count, &cand)) {
            if (mihft_write_hfdeclog(logfp, seq++, ord, 4, "SELF_MATCH_CANDIDATE") != 0) {
                return MIHFT_RC_IOERR;
            }
            fprintf(stderr, "自己対当候補:%s:%s:%lld:%lld:%lld\n",
                    ord->order_id,
                    cand.book->instr_code,
                    (long long)cand.book->price_amt,
                    (long long)cand.book->book_qty,
                    (long long)cand.overlap_qty);
            final_code = 4;
        } else if (mihft_write_hfdeclog(logfp, seq++, ord, 0, "NO_CROSS") != 0) {
            return MIHFT_RC_IOERR;
        }
    }

    return final_code;
}

int main(void)
{
    const char *scbook_path = getenv("MIHFT_SCBOOK");
    const char *scordf_path = getenv("MIHFT_SCORDF");
    const char *hfdeclog_path = getenv("MIHFT_HFDECLOG");
    MihftBookRow book[MIHFT_SCBOOK_MAX];
    MihftOrderRow orders[MIHFT_SCORDF_MAX];
    size_t book_count = 0;
    size_t order_count = 0;
    FILE *logfp;
    int rc;

    if (scbook_path == NULL || scbook_path[0] == '\0') {
        scbook_path = "SCBOOK.csv";
    }
    if (scordf_path == NULL || scordf_path[0] == '\0') {
        scordf_path = "SCORDF.csv";
    }
    if (hfdeclog_path == NULL || hfdeclog_path[0] == '\0') {
        hfdeclog_path = "HFDECLOG.csv";
    }

    if (mihft_read_scbook(scbook_path, book, MIHFT_SCBOOK_MAX, &book_count) != 0 ||
        mihft_read_scordf(scordf_path, orders, MIHFT_SCORDF_MAX, &order_count) != 0) {
        return MIHFT_RC_PARSEERR;
    }

    logfp = fopen(hfdeclog_path, "w");
    if (logfp == NULL) {
        fprintf(stderr, "HFDECLOGを開けません:%s\n", hfdeclog_path);
        return MIHFT_RC_IOERR;
    }

    if (fprintf(logfp, "DECISION-ID,ORDER-ID,INSTR-CODE,ACTION-CODE,REASON-CODE,DECISION-TS\n") < 0) {
        fprintf(stderr, "HFDECLOG見出し書込失敗\n");
        fclose(logfp);
        return MIHFT_RC_IOERR;
    }

    rc = mihft_process(logfp, book, book_count, orders, order_count);

    if (fclose(logfp) != 0) {
        fprintf(stderr, "HFDECLOG終了失敗\n");
        return MIHFT_RC_IOERR;
    }

    return rc;
}
