#!/usr/bin/env python3
"""Partial-credit fallback (secondary signal).

The PRIMARY grade is behavioral pass/fail (grader.py). But when an agent's code
won't compile/run, behavioral grading yields a flat 0 — indistinguishable between
"almost right, one typo" and "did nothing". This fallback asks a small model
(haiku) to read the candidate's CODE against the firewalled reference + the same
criteria and estimate how close it got, per criterion. It NEVER contaminates the
headline pass/fail — it's reported separately as `partial_credit`. Same reference +
rubric for every arm (fair). Only invoked on a compile/run failure.
"""
from __future__ import annotations
import json, re, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "gen"))
from llm import call_llm   # noqa: E402

PROMPT = """You are grading a COBOL modernization attempt that FAILED to compile or
run, so behavioral tests could not score it. Estimate how close the CANDIDATE code is
to correct — judge the LOGIC, not whether it compiled.

EXPECTED BEHAVIOR — the criteria a correct solution must satisfy:
{criteria}

REFERENCE — one correct implementation (not the only valid form):
```cobol
{reference}
```

CANDIDATE — the attempt under review (did not run):
```cobol
{candidate}
```

For EACH criterion, score the candidate's code: 1.0 = clearly implements it,
0.5 = partial/attempted-but-flawed, 0.0 = absent/wrong. Base it on the logic present,
not on syntax errors. Return ONLY JSON, no prose:
{{"criteria":[{{"id":"<criterion id>","score":0.0,"note":"<=12 words"}}],"overall":<mean>}}"""


def _criteria_block(criteria):
    return "\n".join(f"- {c['id']}: {c['label']} (expected: {c['expected']})" for c in criteria)


def _parse_json(text):
    m = re.search(r"\{.*\}", text, re.S)
    return json.loads(m.group(0)) if m else {"criteria": [], "overall": 0.0}


def judge_partial(criteria, reference_text, candidate_text, model="haiku", backend="claude"):
    """Return {"criteria":[{id,score,note}], "overall": float}. Robust to model
    noise; never raises into the grader (returns overall 0.0 on failure)."""
    prompt = PROMPT.format(criteria=_criteria_block(criteria),
                           reference=reference_text[:12000], candidate=candidate_text[:12000])
    try:
        out = call_llm(prompt, backend=backend, model=model, timeout=120)
        res = _parse_json(out)
        # clamp + recompute overall defensively
        for c in res.get("criteria", []):
            c["score"] = max(0.0, min(1.0, float(c.get("score", 0))))
        scores = [c["score"] for c in res.get("criteria", [])]
        res["overall"] = round(sum(scores) / len(scores), 3) if scores else 0.0
        return res
    except Exception as e:
        return {"criteria": [], "overall": 0.0, "error": str(e)[:200]}


# demo: score the START program set (a realistic "fails the test" candidate) against
# the GOLDEN reference. Expect ~low overall (start gets only exemption right).
if __name__ == "__main__":
    sys.path.insert(0, str(HERE))
    from grader import Grader
    SRC = HERE.parent / "out" / "codebase" / "cobol" / "src"
    REF = HERE / "reference"
    DELTA = ["KZ120S", "KZ125S", "KZ115B"]
    cand = "\n".join((SRC / f"{p}.cbl").read_text(errors="replace") for p in DELTA)
    ref = "\n".join((REF / f"{p}.cbl").read_text(errors="replace") for p in DELTA)
    print("judging START set vs GOLDEN reference (haiku)...", flush=True)
    res = judge_partial(Grader().criteria, ref, cand)
    print(json.dumps(res, ensure_ascii=False, indent=2))
