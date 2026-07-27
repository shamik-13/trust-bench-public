      *================================================================
      * KZYAGFC -- KZYAGF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZYAGF-REC.
           05  YA-ACCT-NO               PIC X(16).
           05  YA-YEAR                  PIC X(10).
           05  YA-TOTAL-GROSS-INT-AMT   PIC S9(11)V99 COMP-3.
           05  YA-TOTAL-NATIONAL-TAX-AMT PIC S9(11)V99 COMP-3.
           05  YA-TOTAL-LOCAL-TAX-AMT   PIC S9(11)V99 COMP-3.
           05  YA-TOTAL-NET-AMT         PIC S9(11)V99 COMP-3.
           05  YA-PAYMENT-COUNT         PIC 9(08).
