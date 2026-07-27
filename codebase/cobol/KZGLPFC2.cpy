      *================================================================
      * KZGLPFC -- KZGLPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZGLPF-REC.
           05  GP-POSTING-DATE          PIC X(10).
           05  GP-ACCT-NO               PIC X(16).
           05  GP-ACCT-TYPE             PIC X(02).
           05  GP-WITHHOLDING-LIAB-AMT  PIC S9(11)V99 COMP-3.
           05  GP-LOCAL-LIAB-AMT        PIC S9(11)V99 COMP-3.
           05  GP-NET-PAYABLE-AMT       PIC S9(11)V99 COMP-3.
           05  GP-GL-BRANCH-CD          PIC X(10).
           05  GP-JOURNAL-NO            PIC X(16).
