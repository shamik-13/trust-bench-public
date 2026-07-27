      *================================================================
      * CDSALC -- CDSALF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDSALF-REC.
           05  SL-SALES-ID              PIC X(10).
           05  SL-AUTH-ID               PIC X(10).
           05  SL-CARD-NO               PIC X(16).
           05  SL-SALES-AMT             PIC S9(11)V99 COMP-3.
           05  SL-SALES-TS              PIC X(14).
           05  SL-MERCHANT-CODE         PIC X(04).
