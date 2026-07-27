/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20200310  今井 彩 (E-230)    初版作成、約定反映前の平均取得単価更新処理を実装
 * 1.01  20200810  今井 彩 (E-230)    反対売買時の実現損益計算と建玉ゼロ時の単価初期化を追加
 */

#include "mihft_types.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIHFT_RC_ERR_IO 20
#define MIHFT_RC_ERR_PARSE 21
#define MIHFT_LINE_MAX 512
#define MIHFT_CODE_MAX 32
#define MIHFT_POS_MAX 4096
#define MIHFT_EXEC_MAX 16384

struct engine_exec_rec {
    char exec_id[MIHFT_CODE_MAX];
    char order_id[MIHFT_CODE_MAX];
    char instr_code[MIHFT_CODE_MAX];
    char side_kbn;
    int64_t fill_qty;
    int64_t fill_amt;
    char exec_ts[MIHFT_CODE_MAX];
};

struct engine_pos_rec {
    char cif_no[MIHFT_CODE_MAX];
    char instr_code[MIHFT_CODE_MAX];
    int64_t net_qty;
    int64_t avg_amt;
    int64_t rlzd_amt;
};

static void strip_line(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r')) {
        s[--n] = '\0';
    }
}

static int copy_field(char *dst, size_t dst_sz, const char *src)
{
    size_t n = strlen(src);
    if (n == 0 || n >= dst_sz) {
        return -1;
    }
    memcpy(dst, src, n + 1);
    return 0;
}

static int parse_i64(const char *s, int64_t *out)
{
    char *endp = NULL;
    long long v;

    if (s == NULL || *s == '\0') {
        return -1;
    }
    errno = 0;
    v = strtoll(s, &endp, 10);
    if (errno != 0 || endp == s || *endp != '\0') {
        return -1;
    }
    *out = (int64_t)v;
    return 0;
}

static int next_field(char **cur, char **field)
{
    char *p = *cur;
    char *comma;

    if (p == NULL) {
        return 0;
    }
    comma = strchr(p, ',');
    if (comma != NULL) {
        *comma = '\0';
        *cur = comma + 1;
    } else {
        *cur = NULL;
    }
    *field = p;
    return 1;
}

static int parse_exec_line(char *line, struct engine_exec_rec *rec)
{
    char *cur = line;
    char *f[7];
    int i;

    for (i = 0; i < 7; i++) {
        if (!next_field(&cur, &f[i])) {
            return -1;
        }
    }
    if (cur != NULL) {
        return -1;
    }
    if (copy_field(rec->exec_id, sizeof(rec->exec_id), f[0]) != 0 ||
        copy_field(rec->order_id, sizeof(rec->order_id), f[1]) != 0 ||
        copy_field(rec->instr_code, sizeof(rec->instr_code), f[2]) != 0 ||
        copy_field(rec->exec_ts, sizeof(rec->exec_ts), f[6]) != 0) {
        return -1;
    }
    if ((f[3][0] != 'B' && f[3][0] != 'S') || f[3][1] != '\0') {
        return -1;
    }
    rec->side_kbn = f[3][0];
    if (parse_i64(f[4], &rec->fill_qty) != 0 ||
        parse_i64(f[5], &rec->fill_amt) != 0) {
        return -1;
    }
    if (rec->fill_qty <= 0 || rec->fill_amt <= 0) {
        return -1;
    }
    return 0;
}

static int parse_pos_line(char *line, struct engine_pos_rec *rec)
{
    char *cur = line;
    char *f[5];
    int i;

    for (i = 0; i < 5; i++) {
        if (!next_field(&cur, &f[i])) {
            return -1;
        }
    }
    if (cur != NULL) {
        return -1;
    }
    if (copy_field(rec->cif_no, sizeof(rec->cif_no), f[0]) != 0 ||
        copy_field(rec->instr_code, sizeof(rec->instr_code), f[1]) != 0) {
        return -1;
    }
    if (parse_i64(f[2], &rec->net_qty) != 0 ||
        parse_i64(f[3], &rec->avg_amt) != 0 ||
        parse_i64(f[4], &rec->rlzd_amt) != 0) {
        return -1;
    }
    if (rec->net_qty == 0 && rec->avg_amt != 0) {
        rec->avg_amt = 0;
    }
    return 0;
}

static int read_execs(const char *path, struct engine_exec_rec *execs, size_t *count)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    size_t n = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "約定ファイルを開けません: %s\n", path);
        return MIHFT_RC_ERR_IO;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        strip_line(line);
        if (line[0] == '\0') {
            continue;
        }
        if (strncmp(line, "EXEC-ID,", 8) == 0) {
            continue;
        }
        if (n >= MIHFT_EXEC_MAX) {
            fprintf(stderr, "約定件数が上限を超過しました\n");
            fclose(fp);
            return MIHFT_RC_ERR_PARSE;
        }
        if (parse_exec_line(line, &execs[n]) != 0) {
            fprintf(stderr, "約定行の形式が不正です\n");
            fclose(fp);
            return MIHFT_RC_ERR_PARSE;
        }
        n++;
    }
    if (ferror(fp)) {
        fprintf(stderr, "約定ファイルの読込に失敗しました\n");
        fclose(fp);
        return MIHFT_RC_ERR_IO;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static int read_positions(const char *path, struct engine_pos_rec *poses, size_t *count)
{
    FILE *fp;
    char line[MIHFT_LINE_MAX];
    size_t n = 0;

    fp = fopen(path, "r");
    if (fp == NULL) {
        fprintf(stderr, "建玉ファイルを開けません: %s\n", path);
        return MIHFT_RC_ERR_IO;
    }
    while (fgets(line, sizeof(line), fp) != NULL) {
        strip_line(line);
        if (line[0] == '\0') {
            continue;
        }
        if (strncmp(line, "CIF-NO,", 7) == 0) {
            continue;
        }
        if (n >= MIHFT_POS_MAX) {
            fprintf(stderr, "建玉件数が上限を超過しました\n");
            fclose(fp);
            return MIHFT_RC_ERR_PARSE;
        }
        if (parse_pos_line(line, &poses[n]) != 0) {
            fprintf(stderr, "建玉行の形式が不正です\n");
            fclose(fp);
            return MIHFT_RC_ERR_PARSE;
        }
        n++;
    }
    if (ferror(fp)) {
        fprintf(stderr, "建玉ファイルの読込に失敗しました\n");
        fclose(fp);
        return MIHFT_RC_ERR_IO;
    }
    fclose(fp);
    *count = n;
    return 0;
}

static int write_positions(const char *path, const struct engine_pos_rec *poses, size_t count)
{
    FILE *fp;
    size_t i;

    fp = fopen(path, "w");
    if (fp == NULL) {
        fprintf(stderr, "建玉出力ファイルを開けません: %s\n", path);
        return MIHFT_RC_ERR_IO;
    }
    fprintf(fp, "CIF-NO,INSTR-CODE,NET-QTY,AVG-AMT,RLZD-AMT\n");
    for (i = 0; i < count; i++) {
        if (fprintf(fp, "%s,%s,%" PRId64 ",%" PRId64 ",%" PRId64 "\n",
                    poses[i].cif_no,
                    poses[i].instr_code,
                    poses[i].net_qty,
                    poses[i].avg_amt,
                    poses[i].rlzd_amt) < 0) {
            fprintf(stderr, "建玉出力に失敗しました\n");
            fclose(fp);
            return MIHFT_RC_ERR_IO;
        }
    }
    if (fclose(fp) != 0) {
        fprintf(stderr, "建玉出力ファイルの確定に失敗しました\n");
        return MIHFT_RC_ERR_IO;
    }
    return 0;
}

static int same_key(const struct engine_pos_rec *p, const char *cif_no, const char *instr_code)
{
    return strcmp(p->cif_no, cif_no) == 0 && strcmp(p->instr_code, instr_code) == 0;
}

static int find_pos(struct engine_pos_rec *poses, size_t count, const char *cif_no, const char *instr_code)
{
    size_t i;

    for (i = 0; i < count; i++) {
        if (same_key(&poses[i], cif_no, instr_code)) {
            return (int)i;
        }
    }
    return -1;
}

static int add_pos(struct engine_pos_rec *poses, size_t *count, const char *cif_no, const char *instr_code)
{
    struct engine_pos_rec *p;

    if (*count >= MIHFT_POS_MAX) {
        return -1;
    }
    p = &poses[*count];
    if (copy_field(p->cif_no, sizeof(p->cif_no), cif_no) != 0 ||
        copy_field(p->instr_code, sizeof(p->instr_code), instr_code) != 0) {
        return -1;
    }
    p->net_qty = 0;
    p->avg_amt = 0;
    p->rlzd_amt = 0;
    (*count)++;
    return (int)(*count - 1);
}

static int64_t abs_i64(int64_t v)
{
    return v < 0 ? -v : v;
}

static int checked_add_i64(int64_t a, int64_t b, int64_t *out)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
        return -1;
    }
    *out = a + b;
    return 0;
}

static int checked_mul_i64(int64_t a, int64_t b, int64_t *out)
{
    if (a != 0 && b != 0) {
        if (a > INT64_MAX / b || a < INT64_MIN / b) {
            return -1;
        }
    }
    *out = a * b;
    return 0;
}

static int instr_tick(const char *instr_code)
{
    unsigned char c = (unsigned char)instr_code[0];

    if (c == '1' || c == '2' || c == '3') {
        return 100;
    }
    if (c == '4' || c == '5' || c == '6' || c == '7') {
        return 500;
    }
    return 1000;
}

static int validate_exec(const struct engine_exec_rec *e)
{
    int64_t price;

    if (e->fill_amt > MIHFT_MAX_NOTIONAL) {
        return 8;
    }
    price = e->fill_amt / e->fill_qty;
    if (price <= 0 || price % instr_tick(e->instr_code) != 0) {
        return 12;
    }
    return 0;
}

/*
 * 平均取得単価 (AVG-AMT) の算定そのものは mihft_pos 本体の責務であり、当エンジン
 * では再算定しない。当処理は SCPOSF が保持する AVG-AMT を所与として引き継ぎつつ、
 * 反対売買分について実現損益 (RLZD-AMT) のみを増分し、数量 (NET-QTY) を更新する。
 * 建玉がゼロになった銘柄は AVG-AMT を明示的にゼロへ戻すだけで、加重平均の再計算は
 * 行わない (本体出力との二重計算を避けるため)。
 */
static int apply_exec(struct engine_pos_rec *p, const struct engine_exec_rec *e)
{
    int64_t signed_qty = e->side_kbn == 'B' ? e->fill_qty : -e->fill_qty;
    int64_t old_qty = p->net_qty;
    int64_t old_abs = abs_i64(old_qty);
    int64_t fill_abs = e->fill_qty;
    int64_t close_qty;
    int64_t unit_amt;
    int64_t old_cost;
    int64_t fill_cost;
    int64_t new_qty;
    int64_t pnl;
    int64_t tmp;

    /* 追加建て: 数量のみ反映し、AVG-AMT は本体算定値をそのまま保持する。 */
    if (old_qty == 0 || (old_qty > 0 && signed_qty > 0) || (old_qty < 0 && signed_qty < 0)) {
        p->net_qty = old_qty + signed_qty;
        return 0;
    }

    /* 反対売買: 実現損益のみ増分する。AVG-AMT (取得単価) は本体に従う。 */
    unit_amt = e->fill_amt / e->fill_qty;
    close_qty = old_abs < fill_abs ? old_abs : fill_abs;
    if (checked_mul_i64(p->avg_amt, close_qty, &old_cost) != 0 ||
        checked_mul_i64(unit_amt, close_qty, &fill_cost) != 0) {
        return -1;
    }
    pnl = old_qty > 0 ? fill_cost - old_cost : old_cost - fill_cost;
    if (checked_add_i64(p->rlzd_amt, pnl, &tmp) != 0) {
        return -1;
    }
    p->rlzd_amt = tmp;

    new_qty = old_qty + signed_qty;
    p->net_qty = new_qty;
    if (new_qty == 0) {
        p->avg_amt = 0;
    }
    return 0;
}

int main(int argc, char **argv)
{
    const char *exec_path = argc > 1 ? argv[1] : "SCEXEC.csv";
    const char *pos_in_path = argc > 2 ? argv[2] : "SCPOSF.csv";
    const char *pos_out_path = argc > 3 ? argv[3] : "SCPOSF.out.csv";
    const char *cif_no = argc > 4 ? argv[4] : "CIF00000001";
    struct engine_exec_rec execs[MIHFT_EXEC_MAX];
    struct engine_pos_rec poses[MIHFT_POS_MAX];
    size_t exec_count = 0;
    size_t pos_count = 0;
    size_t i;
    int rc;
    int decision = 0;

    rc = read_execs(exec_path, execs, &exec_count);
    if (rc != 0) {
        return rc;
    }
    rc = read_positions(pos_in_path, poses, &pos_count);
    if (rc != 0) {
        return rc;
    }

    for (i = 0; i < exec_count; i++) {
        int pos_ix;
        int v = validate_exec(&execs[i]);

        if (v != 0) {
            decision = v;
            continue;
        }
        pos_ix = find_pos(poses, pos_count, cif_no, execs[i].instr_code);
        if (pos_ix < 0) {
            pos_ix = add_pos(poses, &pos_count, cif_no, execs[i].instr_code);
            if (pos_ix < 0) {
                fprintf(stderr, "建玉の追加に失敗しました\n");
                return MIHFT_RC_ERR_PARSE;
            }
        }
        if (apply_exec(&poses[pos_ix], &execs[i]) != 0) {
            fprintf(stderr, "平均取得単価の計算で桁あふれを検知しました\n");
            return MIHFT_RC_ERR_PARSE;
        }
    }

    rc = write_positions(pos_out_path, poses, pos_count);
    if (rc != 0) {
        return rc;
    }
    return decision;
}
