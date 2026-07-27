# verify/ — the build-task rigs

The 30 build tasks are graded by **execution**, not by diffing against a reference: a
correct fix can be written many ways, so what is checked is observable behavior. Each
scenario with a build task ships a rig here.

```
verify/
  _gatelib.py          shared COBOL compile+run machinery (cobc)
  _gatelib_native.py   shared C / Java compile+run machinery (clang, javac)
  run_all_gates.py     validity self-test across all 30 tasks — the gold sanity run
  grade_workspace.py   score one agent-edited workspace against one task
  scenarios/<scn>/
    run_gates.py         thin config: engine programs, golden-delta set, scaffolding
    grader.py            the test cases — parses the run output, checks each criterion
    fixture/             the fixed test inputs
    harness/             COBOL only: LOADER/DUMP programs + the fixture->flat-file writer
    reference/           the hidden golden implementation (grader-side; never given to the agent)
```

## Run the validity self-test first

```bash
python verify/run_all_gates.py            # all 30
python verify/run_all_gates.py s1-fees    # one
python verify/run_all_gates.py --json
```

For every task this compiles and runs two versions of the codebase and asserts the two
properties that make the task meaningful:

- **solvable** — the golden implementation passes *every* test case;
- **not pre-solved** — the shipped start state fails *at least one*.

On a correct install every gate is `PASS`. Scenarios whose compiler is not installed are
reported `SKIP`.

## Score an agent's work

```bash
cp -R codebase /tmp/agent-run          # give the agent its own copy
# ... the agent edits /tmp/agent-run to solve the prompt of task S1-F6 ...
python verify/grade_workspace.py --task S1-F6 --workspace /tmp/agent-run
```

```
task S1-F6  (s1-fees)   workspace: /tmp/agent-run

  test cases
    PASS  C1-rate-revision  — ACCT000001 fee = 3600
    PASS  C2-prem-deprecated  — ACCT000002 fee = 3600
    FAIL  C3-cycle-cap  — ACCT000003 fee = 50000, cap = Y
    ...

  tests = 5/6 (0.83)   pass = 0
```

`tests` is the fraction of test cases passed; `pass` is 1 only when all of them pass —
the headline build metric. Exit status is 0 iff `pass` is 1. `--json` emits the same
result as a record.

The engine programs are located **by name** anywhere under `--workspace`, so the agent may
reorganize files freely. A workspace that does not compile scores 0 on every test case.

## How a rig works

`run_gates.py` names the *engine* programs the task's fix path runs through and the
*delta* subset that ships a golden reference. To build the **golden** side, the rig takes
the delta programs from `reference/` and everything else from the merged `codebase/`; to
build the **start** side (or an agent workspace), it takes everything from the tree it is
pointed at. Both sides are compiled into a private temp dir together with the scaffolding
in `harness/`, run against `fixture/`, and their dumped output handed to `grader.py`.

`grader.py` is the test suite. Its `criteria` list is the authoritative statement of what
the task is graded on — each entry asserts one observable outcome of one business rule on
one fixture record, never an implementation detail. Read it to see exactly what a task
requires.

Test cases were authored from the frozen decision registry and the shared data domain —
not from the golden code — so they cannot encode an implementation the fix must match.

## Notes

- The gates source the start side from `../codebase` (the merged estate). Point them
  elsewhere with `TRUSTBENCH_CODEBASE=/path/to/codebase`.
- COBOL builds use `cobc -x -fformat=variable`. `-fformat=variable` keeps fixed-form
  source but drops the column-72 right margin so Japanese (3-byte UTF-8) `DISPLAY`
  literals do not truncate mid-character.
- Each build runs in its own temp dir and cleans up after itself; nothing is written into
  this tree.
- The Java rigs pass a `-sourcepath` derived from the engine's package, and the C rigs
  resolve link-time undefined symbols from sibling helper files, so a fix that refactors
  logic into a new same-package class or helper `.c` still links.
