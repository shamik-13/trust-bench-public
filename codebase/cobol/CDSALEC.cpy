      *================================================================
      * CDSALEC -- CDSALESF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDSALESF-REC.
           05  SL-SALES-ID              PIC X(10).
           05  SL-AUTH-ID               PIC X(10).
           05  SL-CARD-NO               PIC X(16).
           05  SL-MERCHANT-ID           PIC X(10).
           05  SL-SALES-DT              PIC 9(08).
           05  SL-POSTING-DT            PIC 9(08).
           05  SL-SALES-AMT             PIC S9(11)V99 COMP-3.
           05  SL-TAX-AMT               PIC S9(11)V99 COMP-3.
           05  SL-CAPTURE-STATUS        PIC X(02).
