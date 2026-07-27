      *================================================================
      * CDSALEFC -- CDSALEF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDSALEF-REC.
           05  SL-SALE-ID               PIC X(10).
           05  SL-CARD-NO               PIC X(16).
           05  SL-SALE-AMT              PIC S9(11)V99 COMP-3.
           05  SL-CURRENCY-CD           PIC X(10).
           05  SL-MERCHANT-CODE         PIC X(04).
           05  SL-SALE-DT               PIC 9(08).
           05  SL-AUTH-ID               PIC X(10).
