/*
 * 変更履歴
 * 版数  年月日    担当        概要
 * 1.00  20240709  市場基盤部  初版作成
 * 1.01  20241209  市場基盤部  セッション別秒間発注上限の判定を追加
 * 1.02  20250509  市場基盤部  キルスイッチ判定と集計検査を追加
 */

#include "mihft_types.h"

#include <limits.h>
#include <stddef.h>
#include <stdint.h>

#ifndef MIHFT_DECISION_ALLOW
#define MIHFT_DECISION_ALLOW 0
#endif

#ifndef MIHFT_DECISION_DENY
#define MIHFT_DECISION_DENY 1
#endif

#define MIHFT_LOCAL_SESSION_COUNT 8u
#define MIHFT_LOCAL_WINDOW_NS 1000000000LL
#define MIHFT_LOCAL_RATE_CEILING 9000u
#define MIHFT_LOCAL_BURST_MARGIN 80u

typedef struct {
    int32_t session_id;
    uint32_t orders_in_window;
    int64_t window_start_ns;
    int64_t now_ns;
    uint8_t kill_seen;
} mihft_local_session_state;

static int add_u32_checked(uint32_t a, uint32_t b, uint32_t *out)
{
    if (out == (uint32_t *)0) {
        return -1;
    }

    if (UINT_MAX - a < b) {
        return -1;
    }

    *out = a + b;
    return 0;
}

static int64_t elapsed_ns(int64_t now_ns, int64_t start_ns, int *ok)
{
    if (ok == (int *)0) {
        return 0;
    }

    if ((now_ns >= 0 && start_ns < 0 && now_ns > INT64_MAX + start_ns) ||
        (now_ns < 0 && start_ns > 0 && now_ns < INT64_MIN + start_ns)) {
        *ok = 0;
        return 0;
    }

    *ok = 1;
    return now_ns - start_ns;
}

static int evaluate_session(const mihft_local_session_state *state, uint32_t ceiling)
{
    uint32_t projected;
    int ok;
    int64_t elapsed;

    if (state == (const mihft_local_session_state *)0 || ceiling == 0u) {
        return MIHFT_DECISION_DENY;
    }

    if (state->kill_seen != 0u) {
        return MIHFT_DECISION_DENY;
    }

    elapsed = elapsed_ns(state->now_ns, state->window_start_ns, &ok);
    if (ok == 0 || elapsed < 0 || elapsed > MIHFT_LOCAL_WINDOW_NS) {
        return MIHFT_DECISION_DENY;
    }

    if (add_u32_checked(state->orders_in_window, 1u, &projected) != 0) {
        return MIHFT_DECISION_DENY;
    }

    if (projected > ceiling + MIHFT_LOCAL_BURST_MARGIN) {
        return MIHFT_DECISION_DENY;
    }

    return MIHFT_DECISION_ALLOW;
}

int main(void)
{
    static const mihft_local_session_state staged[MIHFT_LOCAL_SESSION_COUNT] = {
        { 101, 8840u, 3500000000000LL, 3500000100000LL, 0u },
        { 102, 9015u, 3500000000000LL, 3500000200000LL, 0u },
        { 103, 8975u, 3500000000000LL, 3500000300000LL, 0u },
        { 104, 2140u, 3500000000000LL, 3500000400000LL, 0u },
        { 105, 9079u, 3500000000000LL, 3500000500000LL, 0u },
        { 106, 4500u, 3500000000000LL, 3500000600000LL, 1u },
        { 107, 8999u, 3500000000000LL, 3500000700000LL, 0u },
        { 108, 9080u, 3500000000000LL, 3500000800000LL, 0u }
    };

    size_t i;
    uint32_t allow_count = 0u;
    uint32_t deny_count = 0u;
    int final_decision = MIHFT_DECISION_ALLOW;

    for (i = 0u; i < MIHFT_LOCAL_SESSION_COUNT; i++) {
        int decision = evaluate_session(&staged[i], MIHFT_LOCAL_RATE_CEILING);

        if (decision == MIHFT_DECISION_ALLOW) {
            if (add_u32_checked(allow_count, 1u, &allow_count) != 0) {
                return 2;
            }
        } else {
            if (add_u32_checked(deny_count, 1u, &deny_count) != 0) {
                return 2;
            }
            final_decision = decision;
        }
    }

    if (allow_count == 0u || deny_count == 0u) {
        return 3;
    }

    return final_decision;
}
