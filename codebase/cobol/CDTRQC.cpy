      *================================================================
      * CDTRQC -- CDTRQF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDTRQF-REC.
           05  TRQ-REQUEST-ID           PIC X(10).
           05  TRQ-CARD-NO              PIC X(16).
           05  TRQ-BILLING-CYCLE-DT     PIC 9(08).
           05  TRQ-REQUEST-AMT          PIC S9(11)V99 COMP-3.
           05  TRQ-DUE-DT               PIC 9(08).
           05  TRQ-BANK-CD              PIC X(10).
           05  TRQ-ACCOUNT-NO           PIC X(16).
           05  TRQ-REQUEST-STATUS       PIC X(02).
