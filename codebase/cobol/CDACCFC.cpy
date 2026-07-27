      *================================================================
      * CDACCFC -- CDACCF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDACCF-REC.
           05  AC-CARD-NO               PIC X(16).
           05  AC-MEMBER-ID             PIC X(10).
           05  AC-STATUS-KBN            PIC X(02).
           05  AC-CREDIT-LIMIT          PIC S9(11)V99 COMP-3.
           05  AC-CASH-LIMIT            PIC S9(11)V99 COMP-3.
           05  AC-USED-AMT              PIC S9(11)V99 COMP-3.
           05  AC-DELAY-KBN             PIC X(02).
           05  AC-LAST-UPD-DT           PIC 9(08).
