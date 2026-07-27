      *================================================================
      * CDLIMC -- CDLIMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDLIMF-REC.
           05  LM-CARD-NO               PIC X(16).
           05  LM-TEMP-LIMIT-AMT        PIC S9(11)V99 COMP-3.
           05  LM-START-DT              PIC 9(08).
           05  LM-END-DT                PIC 9(08).
           05  LM-APPROVAL-ID           PIC X(10).
           05  LM-STATUS                PIC X(10).
