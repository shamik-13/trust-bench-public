      *================================================================
      * CDFEEC -- CDFEEF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDFEEF-REC.
           05  FE-FEE-ID                PIC X(10).
           05  FE-CARD-NO               PIC X(16).
           05  FE-FEE-DT                PIC 9(08).
           05  FE-FEE-AMT               PIC S9(11)V99 COMP-3.
           05  FE-FEE-TYPE              PIC X(02).
           05  FE-BILL-CYCLE-CD         PIC X(10).
           05  FE-POST-STATUS           PIC X(02).
