      *================================================================
      * KZFEEHFC -- KZFEEHF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZFEEHF-REC.
           05  FH-ACCT-NO               PIC X(16).
           05  FH-CYCLE-DT              PIC 9(08).
           05  FH-FEE-AMT               PIC S9(11)V99 COMP-3.
           05  FH-FEE-YTD               PIC S9(11)V99 COMP-3.
           05  FH-CONFIRM-FLAG          PIC X(01).
