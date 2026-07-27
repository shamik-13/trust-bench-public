# trust-bench

**A benchmark measuring the causal effect of enterprise document access on coding agents.**

This is the public release accompanying the paper *"trust-bench: Measuring the Value of
Enterprise Document Access for Coding Agents on a Synthetic Japanese Financial Group"*
(under review). It contains the benchmark itself: the codebase, the document corpus, the
task set, and the procedures that grade the build tasks by execution.

Everything here is **fully synthetic**. No real institution's code, documents, customer
data, or identifiers were used.

---

## What the benchmark is

`trust-bench` simulates the legacy codebase of **みらいフィナンシャルグループ** (Mirai
Financial Group), a fictional Japanese financial group of six companies — a trust bank, a
securities firm, a card issuer, a code-payments provider, a life insurer, and a shared
platform subsidiary. 34 scenarios contribute coherent subsystems, and they are **merged
into one flat codebase**: locating the right program among 1,105 candidates is part of the
difficulty.
Alongside the code sits a corpus of 149 Japanese business documents — design guides,
record layouts, rate sheets, approval minutes, incident reports, migration
decks, operations manuals. Filenames are deliberately opaque;
documents identify and cite each other by internal title and document number.

Each task is anchored to one **decisive fact**, and every decisive fact carries a
*visibility class* recording where the truth about it can be recovered — in a document
only, in code only, or split across both.

| | |
|---|---|
| Scenarios (six group companies) | 34 |
| Programs | 1,105 (COBOL 339 / Java 524 / C 156 / PL-I 54 / RPG 32) |
| Copybooks, headers, includes | 418 |
| Lines of code | ~287k |
| Documents | 149 (126 PDF / 15 PPTX / 8 XLSX) |
| Decisive facts (visibility-tagged) | 92 |
| Tasks | 93 — 63 retrieval (LLM-judged) + 30 build (execution-graded) |

All prompts, documents, and code comments are in **Japanese**, the working language of the
simulated group.

---

## Layout

```
codebase/            the merged estate — this is what the agent works on
  cobol/             339 .cbl + 242 .cpy, flat (bank, card, insurance, shared platform)
  c/     src/ include/          156 .c + 90 .h        (securities, payments hot paths)
  java/  *.java jp/mirai/... model/   524 .java
  pli/   src/ inc/              54 + 25               (securities settlement, policy master)
  rpg/   src/ qrpgleref/        32 + 61               (card online)
docs/                149 business documents, opaque names (DOC-#####.pdf|xlsx|pptx)
tasks.json           all 93 tasks: prompt, grading method, answer key
tasks_prompts_only.json   the same list with every answer key stripped — the
                     copy it is safe to put in front of the agent
eval_subsets.json    the 15-scenario / 50-task stratified subset used in the paper
golden/              GRADER-SIDE ONLY — never give any of this to the agent
  doc-map.json         which document carries which decisive fact
  facts.json           the 92 decisive facts with their visibility class, docs, programs
  explorer/            self-contained HTML map of the estate: programs, datasets,
                       documents, facts, and every task with its key. Open
                       golden/explorer/index.html to get oriented.
verify/              the build-task rigs: fixtures, test cases, golden references
MANIFEST.json        file counts + SHA-256 of every file in this release
```

**The agent under test gets `codebase/` and a task `prompt` — plus, in a document
condition, access to `docs/` through whatever channel is being studied.** It must never
see the `golden` keys in `tasks.json`, anything under `golden/`, or `verify/`.

---

## The tasks

`tasks.json` is a flat list of records:

| field | meaning |
|---|---|
| `task_id` | globally unique, scenario-prefixed (`S1-F6`, `S12-1`, `S31-1`) |
| `scenario` | one of the 34 scenarios |
| `tier` | 1 = retrieval / question · 2 = build / implement |
| `mechanic` | what the task exercises (locate, retrieve, reconcile-drift, build, …) |
| `grading` | `answer-judged` or `behavioral` |
| `prompt` | the task as a developer or analyst would pose it — contains no answer |
| `golden` | answer-judged → `{correctness[], completeness[]}`; behavioral → `{expected_fix[]}` |
| `engine`, `verify_dir` | behavioral only: the program(s) to change and the rig that scores it |

### Retrieval tasks (63) — LLM-judged

The key splits into **correctness** points (the decisive facts that must be present) and
**completeness** points (supporting analysis). A judge model, shown only the question, the
numbered key points, and the answer scores each
point 0/1; the task score is the mean over correctness points. The judge instructions used
in the paper are quoted verbatim in its Appendix A.

### Build tasks (30) — execution-graded

The agent edits the codebase; the rig compiles the edited tree, runs it against a fixed
fixture, and checks each test case. Two numbers per task: `tests` (fraction of test cases
passed) and `pass` (1 only if all pass). A workspace that fails to compile fails every
test case.

Both properties that make a build task meaningful are enforced and re-checkable:
the hidden golden implementation passes **every** test case (solvable), and the shipped
start state fails **at least one** (not pre-solved). `verify/run_all_gates.py` re-asserts
both across all 30 tasks — run it first.

Scenarios written in PL/I and RPG have no publicly available off-platform compiler, so
their facts are readable but not executable; those tasks are answer-judged and serve as
cross-language retrieval targets.

See `verify/README.md` for how to run the gates and score a workspace.

---

## Visibility classes

`golden/facts.json` gives each of the 92 decisive facts a `visibility` class. They group
into three families:

- **Family A — document-resident (56 facts).** The fact is recoverable only, or decisively,
  from a document: `doc-code-drift`, `source-invisible`, `doc-text`, `locate-authority`,
  `doc-structural`, `cross-modal-link`, `tribal`, `provenance`, `precedence-ordering`.
- **Hybrid (3 facts).** `code-partial->doc-completes` — code shows half the rule, a
  document supplies the rest.
- **Family B — code-resident (33 facts).** Fully recoverable from code: `code`,
  `code-cross-language`, `code-scattered`, `code-implicit`, `code-drift`. Nearby documents
  carry only context and act as decoys.

---

## Reproducing the paper's baseline

The baseline study uses a stratified subset — 15 scenarios, 50 tasks (36 retrieval,
14 build) — chosen to cover all 15 visibility classes and all 5 languages. The task ids
are in `eval_subsets.json` under `paper_baseline`.

The four context conditions differ **only** in whether and how `docs/` is reachable; the
prompt and `codebase/` are identical in all of them:

| condition | what the agent gets |
|---|---|
| `code` | `codebase/` only — no document reachable at all (the control) |
| `code+docs` | `codebase/` + `docs/` as raw files on disk, with standard extraction tooling |
| `code+rag` | `codebase/` + one hybrid-retrieval tool over the corpus, served over MCP |
| `code+graph` | `codebase/` + knowledge-graph tools over the corpus, served over MCP |

Runs are sandboxed: file reads confined to the condition's own copy of `codebase/`, no
network, documents reachable only through that condition's channel. The control workspace
contains no documents at all.

---

## What this release does not include

- **The generation pipeline** (decision oracles, document templates and seeds, renderers,
  the merge/firewall tooling). The methodology is described in full in the paper.
- **The agent harness and the retrieval / knowledge-graph servers** used for the baseline
  conditions. They are implementation choices, not part of the benchmark; the benchmark is
  the codebase, the corpus, the tasks, and the grading rigs in this folder.
- **The LLM judge implementation.** The judge instructions are quoted verbatim in the
  paper's appendix and are reproducible from `tasks.json` keys with any capable model.

---

## Requirements

Python 3.11+ for the verification rigs, plus the compilers for whichever gates you run:

```bash
brew install gnu-cobol berkeley-db@5     # cobc — the 20 COBOL gates (macOS)
# clang/gcc for the 5 C gates; a JDK (javac/java) for the 5 Java gates
```

Verify the install with the gold sanity run:

```bash
python verify/run_all_gates.py
```

## Integrity

`MANIFEST.json` lists the SHA-256 of every file. Verify with:

```bash
python - <<'EOF'
import hashlib, json, pathlib
m = json.load(open("MANIFEST.json"))
bad = [f for f, h in m["sha256"].items()
       if hashlib.sha256(pathlib.Path(f).read_bytes()).hexdigest() != h]
print("MISMATCH:", bad) if bad else print(f"{len(m['sha256'])} files OK")
EOF
```

## License

- **Code** (the verification rigs under `verify/`): MIT — see `LICENSE`.
- **Data** (the synthetic codebase, the documents, the tasks and keys): CC BY 4.0 — see
  `LICENSE-data`.

If you use trust-bench, please cite the paper. The citation is withheld here while the
paper is under anonymous review.
