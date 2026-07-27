      *================================================================
      * CCMONC -- CCMONF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CCMONF-REC.
           05  MN-ORG-CD                PIC X(10).
           05  MN-YYYYMM                PIC X(10).
           05  MN-TOTAL-INSTR-AMT       PIC S9(11)V99 COMP-3.
           05  MN-TOTAL-VALUE-AMT       PIC S9(11)V99 COMP-3.
           05  MN-COUNT-INSTR           PIC X(10).
           05  MN-COUNT-VALUE           PIC X(10).
