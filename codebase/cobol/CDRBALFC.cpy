      *================================================================
      * CDRBALFC -- CDRBALF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDRBALF-REC.
           05  RB-CARD-NO               PIC X(16).
           05  RB-CYCLE-DT              PIC 9(08).
           05  RB-REV-BAL-AMT           PIC S9(11)V99 COMP-3.
           05  RB-CARRIED-FEE-AMT       PIC S9(11)V99 COMP-3.
           05  RB-NEW-REV-AMT           PIC S9(11)V99 COMP-3.
