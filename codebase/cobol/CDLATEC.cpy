      *================================================================
      * CDLATEC -- CDLATEF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDLATEF-REC.
           05  LAT-CARD-NO              PIC X(16).
           05  LAT-CYCLE-DT             PIC 9(08).
           05  LAT-DELINQ-DAYS          PIC X(10).
           05  LAT-LATE-INTEREST-AMT    PIC S9(11)V99 COMP-3.
           05  LAT-CALC-BASE-AMT        PIC S9(11)V99 COMP-3.
           05  LAT-CALC-DT              PIC 9(08).
