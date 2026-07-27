      *================================================================
      * CDPNTFC -- CDPNTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDPNTF-REC.
           05  PN-CARD-NO               PIC X(16).
           05  PN-POINT-BAL             PIC S9(11)V99 COMP-3.
           05  PN-POINT-EARNED          PIC X(10).
           05  PN-POINT-ADJ             PIC X(10).
           05  PN-LAST-EARN-DT          PIC 9(08).
           05  PN-PROGRAM-ID            PIC X(10).
