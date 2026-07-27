      *================================================================
      * LFLOANC -- LFLOANF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFLOANF-REC.
           05  LN-POL-NO                PIC X(16).
           05  LN-LOAN-NO               PIC X(16).
           05  LN-LOAN-BAL-AMT          PIC S9(11)V99 COMP-3.
           05  LN-ACCRUED-INT-AMT       PIC S9(11)V99 COMP-3.
           05  LN-LAST-INT-DATE         PIC X(10).
           05  LN-LOAN-STATUS-KBN       PIC X(02).
