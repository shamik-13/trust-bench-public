      *================================================================
      * CDPYMFC -- CDPYMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDPYMF-REC.
           05  PY-PAYMENT-ID            PIC X(10).
           05  PY-CARD-NO               PIC X(16).
           05  PY-PAY-AMT               PIC S9(11)V99 COMP-3.
           05  PY-PAY-DT                PIC 9(08).
           05  PY-ALLOC-KBN             PIC X(02).
           05  PY-UNAPPLIED-AMT         PIC S9(11)V99 COMP-3.
