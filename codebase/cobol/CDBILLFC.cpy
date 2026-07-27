      *================================================================
      * CDBILLFC -- CDBILLF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDBILLF-REC.
           05  BI-CARD-NO               PIC X(16).
           05  BI-CYCLE-DT              PIC 9(08).
           05  BI-BILL-AMT              PIC S9(11)V99 COMP-3.
           05  BI-MIN-PAY-AMT           PIC S9(11)V99 COMP-3.
           05  BI-DUE-DT                PIC 9(08).
           05  BI-BILL-STATUS           PIC X(02).
           05  BI-PROGRAM-ID            PIC X(10).
