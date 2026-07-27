      *================================================================
      * LFCVRFC -- LFCVRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFCVRF-REC.
           05  CO-CV-ID                 PIC X(10).
           05  CO-POL-NO                PIC X(16).
           05  CO-RESERVE-AMT           PIC S9(11)V99 COMP-3.
           05  CO-SURR-CHARGE-AMT       PIC S9(11)V99 COMP-3.
           05  CO-CV-AMT                PIC S9(11)V99 COMP-3.
           05  CO-CALC-STATUS-KBN       PIC X(02).
