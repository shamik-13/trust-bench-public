      *================================================================
      * LFRSVC -- LFRSVF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFRSVF-REC.
           05  RS-POL-NO                PIC X(16).
           05  RS-VALUATION-DATE        PIC X(10).
           05  RS-RESERVE-AMT           PIC S9(11)V99 COMP-3.
           05  RS-NET-PREMIUM-AMT       PIC S9(11)V99 COMP-3.
           05  RS-INTEREST-RATE-CD      PIC S9(01)V9(04) COMP-3.
           05  RS-CALC-STATUS-KBN       PIC X(02).
