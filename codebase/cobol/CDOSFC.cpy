      *================================================================
      * CDOSFC -- CDOSF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDOSF-REC.
           05  OS-CARD-NO               PIC X(16).
           05  OS-FEE-BAL-AMT           PIC S9(11)V99 COMP-3.
           05  OS-INTEREST-BAL-AMT      PIC S9(11)V99 COMP-3.
           05  OS-PRINCIPAL-BAL-AMT     PIC S9(11)V99 COMP-3.
           05  OS-CYCLE-DT              PIC 9(08).
