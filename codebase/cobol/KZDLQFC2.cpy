      *================================================================
      * KZDLQFC -- KZDLQF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZDLQF-REC.
           05  DL-ACCT-NO               PIC X(16).
           05  DL-AS-OF-DATE            PIC X(10).
           05  DL-PAST-DUE-DAYS         PIC X(10).
           05  DL-DUE-AMT               PIC S9(11)V99 COMP-3.
           05  DL-WATCH-RANK            PIC X(10).
           05  DL-LAST-NOTICE-DATE      PIC X(10).
