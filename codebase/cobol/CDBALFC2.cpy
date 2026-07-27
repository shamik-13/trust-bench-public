      *================================================================
      * CDBALFC -- CDBALF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDBALF-REC.
           05  BL-CARD-NO               PIC X(16).
           05  BL-CURRENT-BAL-AMT       PIC S9(11)V99 COMP-3.
           05  BL-LAST-STMT-AMT         PIC S9(11)V99 COMP-3.
           05  BL-CYCLE-DT              PIC 9(08).
