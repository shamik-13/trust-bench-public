      *================================================================
      * CDCAPTC -- CDCAPTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDCAPTF-REC.
           05  CP-CAPTURE-ID            PIC X(10).
           05  CP-AUTH-NO               PIC X(16).
           05  CP-CARD-NO               PIC X(16).
           05  CP-SALES-DT              PIC 9(08).
           05  CP-CAPTURE-AMT           PIC S9(11)V99 COMP-3.
           05  CP-MERCHANT-ID           PIC X(10).
           05  CP-CAPTURE-STATUS        PIC X(02).
