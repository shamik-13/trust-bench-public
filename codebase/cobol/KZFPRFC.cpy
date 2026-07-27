      *================================================================
      * KZFPRFC -- KZFPRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZFPRF-REC.
           05  FP-FISCAL-PERIOD-ID      PIC X(10).
           05  FP-PERIOD-START-DT       PIC 9(08).
           05  FP-PERIOD-END-DT         PIC 9(08).
           05  FP-CLOSE-STATUS          PIC X(02).
           05  FP-BASE-CYCLE-ID         PIC X(10).
