      *================================================================
      * TGCLJNC -- TGCLJNF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGCLJNF-REC.
           05  CJ-CLEARING-DT           PIC 9(08).
           05  CJ-CENTER-SEQ            PIC X(10).
           05  CJ-DIRECTION             PIC X(10).
           05  CJ-BANK-CD               PIC X(10).
           05  CJ-BRANCH-CD             PIC X(10).
           05  CJ-ITEM-COUNT            PIC X(10).
           05  CJ-TOTAL-AMT             PIC S9(11)V99 COMP-3.
           05  CJ-MATCH-STATUS          PIC X(02).
           05  CJ-JOURNAL-TS            PIC X(14).
