      *================================================================
      * TGACKNC -- TGACKNF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGACKNF-REC.
           05  AK-ACK-DT                PIC 9(08).
           05  AK-CENTER-SEQ            PIC X(10).
           05  AK-NOTICE-TYPE           PIC X(02).
           05  AK-SOURCE-PROGRAM        PIC X(10).
           05  AK-TARGET-BANK           PIC X(04).
           05  AK-TARGET-BRANCH         PIC X(04).
           05  AK-ITEM-AMT              PIC S9(11)V99 COMP-3.
           05  AK-RESULT-CD             PIC X(10).
           05  AK-NOTICE-TEXT           PIC X(10).
