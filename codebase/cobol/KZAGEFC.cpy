      *================================================================
      * KZAGEFC -- KZAGEF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZAGEF-REC.
           05  AG-AGING-KEY             PIC X(10).
           05  AG-BASE-DT               PIC 9(08).
           05  AG-TARGET-DT             PIC 9(08).
           05  AG-ELAPSED-DAYS          PIC X(10).
           05  AG-BUSINESS-DAYS         PIC X(10).
           05  AG-AGING-BUCKET          PIC X(10).
