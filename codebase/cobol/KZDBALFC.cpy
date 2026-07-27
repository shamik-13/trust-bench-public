      *================================================================
      * KZDBALFC -- KZDBALF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZDBALF-REC.
           05  DB-ACCT-NO               PIC X(16).
           05  DB-CYCLE-ID              PIC X(10).
           05  DB-AVG-DAILY-BAL         PIC S9(11)V99 COMP-3.
           05  DB-INT-RATE              PIC S9(01)V9(04) COMP-3.
           05  DB-PERIOD-START-DT       PIC 9(08).
