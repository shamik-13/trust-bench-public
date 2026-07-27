      *================================================================
      * KZTAXFC -- KZTAXF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZTAXF-REC.
           05  TX-ACCT-NO               PIC X(16).
           05  TX-CYCLE-ID              PIC X(10).
           05  TX-TAX-DT                PIC 9(08).
           05  TX-GROSS-INT-AMT         PIC S9(11)V99 COMP-3.
           05  TX-NATIONAL-TAX-AMT      PIC S9(11)V99 COMP-3.
           05  TX-LOCAL-TAX-AMT         PIC S9(11)V99 COMP-3.
           05  TX-NET-INT-AMT           PIC S9(11)V99 COMP-3.
