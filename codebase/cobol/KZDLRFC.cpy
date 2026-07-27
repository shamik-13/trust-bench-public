      *================================================================
      * KZDLRFC -- KZDLRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZDLRF-REC.
           05  DR-ACCT-NO               PIC X(16).
           05  DR-DAYS-OVERDUE          PIC 9(05).
           05  DR-AGING-BUCKET          PIC X(02).
           05  DR-LATE-CHARGE-AMT       PIC S9(13).
           05  DR-NEW-STATUS            PIC X(02).
