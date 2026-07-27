      *================================================================
      * CDPNTC -- CDPOINTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDPOINTF-REC.
           05  PT-MEMBER-ID             PIC X(10).
           05  PT-POINT-BAL             PIC S9(11)V99 COMP-3.
           05  PT-LAST-EARN-DT          PIC 9(08).
           05  PT-LAST-REDEEM-DT        PIC 9(08).
           05  PT-POINT-STATUS          PIC X(02).
