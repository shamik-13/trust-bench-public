      *================================================================
      * LFCVPFC -- LFCVPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFCVPF-REC.
           05  CI-POL-NO                PIC X(16).
           05  CI-RESERVE-AMT           PIC S9(11)V99 COMP-3.
           05  CI-NEWBIZ-COST-AMT       PIC S9(11)V99 COMP-3.
           05  CI-ELAPSED-MONTH-CNT     PIC 9(08).
           05  CI-CV-STATUS-KBN         PIC X(02).
