      *================================================================
      * CDSTMTF2C -- CDSTMTF2 レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDSTMTF2-REC.
           05  ST-CARD-NO               PIC X(16).
           05  ST-CYCLE-DT              PIC 9(08).
           05  ST-BILL-AMT              PIC S9(11)V99 COMP-3.
           05  ST-MIN-PAY-AMT           PIC S9(11)V99 COMP-3.
           05  ST-DUE-DT                PIC 9(08).
           05  ST-STMT-STATUS           PIC X(02).
           05  ST-DELINQ-DAYS           PIC X(10).
