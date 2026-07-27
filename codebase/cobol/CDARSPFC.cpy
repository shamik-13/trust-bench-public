      *================================================================
      * CDARSPFC -- CDARSPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDARSPF-REC.
           05  AR-AUTH-ID               PIC X(10).
           05  AR-CARD-NO               PIC X(16).
           05  AR-DECISION-KBN          PIC X(02).
           05  AR-AVAIL-AMT             PIC S9(11)V99 COMP-3.
           05  AR-AUTH-AMT              PIC S9(11)V99 COMP-3.
           05  AR-DECLINE-REASON        PIC X(04).
