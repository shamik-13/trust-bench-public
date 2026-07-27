      *================================================================
      * KZFEEJFC -- KZFEEJF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZFEEJF-REC.
           05  FJ-ACCT-NO               PIC X(16).
           05  FJ-POST-DATE             PIC X(10).
           05  FJ-TRAN-CODE             PIC X(04).
           05  FJ-POST-AMT              PIC S9(11)V99 COMP-3.
           05  FJ-BAL-AFTER             PIC X(10).
           05  FJ-FEE-REASON            PIC X(04).
