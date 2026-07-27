      *================================================================
      * KZSTMTFC -- KZSTMTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZSTMTF-REC.
           05  ST-ACCT-ID               PIC X(10).
           05  ST-STMT-DT               PIC 9(08).
           05  ST-OPEN-BAL              PIC S9(11)V99 COMP-3.
           05  ST-CLOSE-BAL             PIC S9(11)V99 COMP-3.
           05  ST-FEE-AMT               PIC S9(11)V99 COMP-3.
           05  ST-INT-AMT               PIC S9(11)V99 COMP-3.
           05  ST-MIN-PAY-AMT           PIC S9(11)V99 COMP-3.
