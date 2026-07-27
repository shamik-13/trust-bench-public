      *================================================================
      * KPDELINFC -- KPDELINF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KPDELINF-REC.
           05  DL-ACCT-ID               PIC X(10).
           05  DL-DAYS-PAST-DUE         PIC X(10).
           05  DL-DUE-AMT               PIC S9(11)V99 COMP-3.
           05  DL-LAST-DUE-DT           PIC 9(08).
           05  DL-PENALTY-RATE-CD       PIC S9(01)V9(04) COMP-3.
