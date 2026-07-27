      *================================================================
      * KPPENHFC -- KPPENHF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KPPENHF-REC.
           05  PH-ACCT-ID               PIC X(10).
           05  PH-PENALTY-DT            PIC 9(08).
           05  PH-PENALTY-AMT           PIC S9(11)V99 COMP-3.
           05  PH-DAYS-PAST-DUE         PIC X(10).
           05  PH-RATE-CD               PIC S9(01)V9(04) COMP-3.
