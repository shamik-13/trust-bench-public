      *================================================================
      * KZFEEHC -- KZFEEHF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZFEEHF-REC.
           05  FE-ACCT-ID               PIC X(10).
           05  FE-FEE-AMT               PIC S9(11)V99 COMP-3.
           05  FE-FEE-YTD               PIC S9(11)V99 COMP-3.
           05  FE-CAP-FLAG              PIC X(01).
           05  FE-EXEMPT-FLAG           PIC X(01).
           05  FE-CYCLE-DT              PIC 9(08).
