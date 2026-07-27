      *================================================================
      * KZUNIFC -- KZUNIF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZUNIF-REC.
           05  UI-ACCT-NO               PIC X(16).
           05  UI-CYCLE-ID              PIC X(10).
           05  UI-UNPAID-INT-AMT        PIC S9(11)V99 COMP-3.
           05  UI-CARRY-FWD-DT          PIC 9(08).
           05  UI-CAUSE-CD              PIC X(10).
           05  UI-LAST-UPDATE-PGM       PIC X(10).
