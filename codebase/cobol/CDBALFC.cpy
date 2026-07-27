      *================================================================
      * CDBALFC -- CDBALF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDBALF-REC.
           05  BL-CARD-NO               PIC X(16).
           05  BL-CYCLE-DT              PIC 9(08).
           05  BL-CLOSING-BAL-AMT       PIC S9(11)V99 COMP-3.
           05  BL-REVOLVING-BAL-AMT     PIC S9(11)V99 COMP-3.
           05  BL-NEW-CHARGE-AMT        PIC S9(11)V99 COMP-3.
           05  BL-CASH-ADV-AMT          PIC S9(11)V99 COMP-3.
