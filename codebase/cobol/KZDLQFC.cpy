      *================================================================
      * KZDLQFC -- KZDLQF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZDLQF-REC.
           05  DQ-ACCT-NO               PIC X(16).
           05  DQ-OVERDUE-AMT           PIC S9(11)V99 COMP-3.
           05  DQ-DUE-DT                PIC 9(08).
           05  DQ-ASOF-DT               PIC 9(08).
           05  DQ-CURR-STATUS           PIC X(02).
