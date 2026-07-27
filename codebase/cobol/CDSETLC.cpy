      *================================================================
      * CDSETLC -- CDSETLF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDSETLF-REC.
           05  ST-SETTLEMENT-ID         PIC X(10).
           05  ST-MERCHANT-CODE         PIC X(04).
           05  ST-SETTLE-DT             PIC 9(08).
           05  ST-GROSS-AMT             PIC S9(11)V99 COMP-3.
           05  ST-NET-AMT               PIC S9(11)V99 COMP-3.
           05  ST-ADJ-AMT               PIC S9(11)V99 COMP-3.
           05  ST-SETTLE-STATUS         PIC X(02).
