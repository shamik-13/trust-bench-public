      *================================================================
      * KZLLACF -- KZLLAF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZLLAF-REC.
           05  LA-ACCT-NO               PIC X(16).
           05  LA-ALLOWANCE-AMT         PIC S9(11)V99 COMP-3.
           05  LA-ALLOWANCE-RATE        PIC S9(01)V9(04) COMP-3.
           05  LA-CALC-DT               PIC 9(08).
           05  LA-ALLOWANCE-TIER        PIC X(10).
           05  LA-PRIOR-ALLOWANCE-AMT   PIC S9(11)V99 COMP-3.
