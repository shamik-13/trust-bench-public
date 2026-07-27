      *================================================================
      * JHSTGC -- JHSTGF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHSTGF-REC.
           05  STG-ACCT-NO              PIC X(16).
           05  STG-CYCLE-DT             PIC 9(08).
           05  STG-FEE-AMT              PIC S9(11)V99 COMP-3.
           05  STG-FEE-YTD              PIC S9(11)V99 COMP-3.
           05  STG-SOURCE-CD            PIC X(10).
           05  STG-LOAD-TS              PIC X(14).
           05  STG-EDIT-STATUS          PIC X(02).
