      *================================================================
      * CDSUMC -- CDSUMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDSUMF-REC.
           05  SM-SUMMARY-KEY           PIC X(10).
           05  SM-SUMMARY-DT            PIC 9(08).
           05  SM-MERCHANT-CODE         PIC X(04).
           05  SM-CURRENCY-CD           PIC X(10).
           05  SM-SALE-COUNT            PIC 9(08).
           05  SM-SALE-AMT              PIC S9(11)V99 COMP-3.
           05  SM-RETURN-AMT            PIC S9(11)V99 COMP-3.
