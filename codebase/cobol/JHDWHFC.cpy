      *================================================================
      * JHDWHFC -- JHDWHF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHDWHF-REC.
           05  DW-ACCT-NO               PIC X(16).
           05  DW-CYCLE-DT              PIC 9(08).
           05  DW-FEE-AMT               PIC S9(11)V99 COMP-3.
           05  DW-FEE-YTD               PIC S9(11)V99 COMP-3.
           05  DW-EXTRACT-DT            PIC 9(08).
