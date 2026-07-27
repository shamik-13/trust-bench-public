      *================================================================
      * TGACPTC -- TGACPTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGACPTF-REC.
           05  AP-ACCEPT-DT             PIC 9(08).
           05  AP-CENTER-SEQ            PIC X(10).
           05  AP-FILE-ID               PIC X(10).
           05  AP-RECORD-TYPE           PIC X(02).
           05  AP-BANK-CD               PIC X(10).
           05  AP-BRANCH-CD             PIC X(10).
           05  AP-FORMAT-STATUS         PIC X(02).
           05  AP-ITEM-AMT              PIC S9(11)V99 COMP-3.
           05  AP-ACCEPT-TS             PIC X(14).
