      *================================================================
      * CDPAYFC -- CDPAYF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDPAYF-REC.
           05  PY-PAY-ID                PIC X(10).
           05  PY-CARD-NO               PIC X(16).
           05  PY-PAY-AMT               PIC S9(11)V99 COMP-3.
           05  PY-PAY-DT                PIC 9(08).
           05  PY-PAY-METHOD            PIC X(10).
