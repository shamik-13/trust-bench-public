      *================================================================
      * KZINTAFC -- KZINTAF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZINTAF-REC.
           05  IA-ACCT-ID               PIC X(10).
           05  IA-ACCRUAL-DT            PIC 9(08).
           05  IA-INT-AMT               PIC S9(11)V99 COMP-3.
           05  IA-CYCLE-BAL             PIC S9(11)V99 COMP-3.
           05  IA-RATE-CD               PIC S9(01)V9(04) COMP-3.
