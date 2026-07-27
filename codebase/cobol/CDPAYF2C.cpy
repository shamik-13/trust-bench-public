      *================================================================
      * CDPAYF2C -- CDPAYF2 レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDPAYF2-REC.
           05  PY-PAYMENT-ID            PIC X(10).
           05  PY-CARD-NO               PIC X(16).
           05  PY-PAYMENT-DT            PIC 9(08).
           05  PY-PAYMENT-AMT           PIC S9(11)V99 COMP-3.
           05  PY-PAYMENT-CHANNEL       PIC X(10).
           05  PY-APPLIED-AMT           PIC S9(11)V99 COMP-3.
           05  PY-UNAPPLIED-AMT         PIC S9(11)V99 COMP-3.
