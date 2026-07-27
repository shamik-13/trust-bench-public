      *================================================================
      * CDSTMTC -- CDSTMTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDSTMTF-REC.
           05  MT-STATEMENT-ID          PIC X(10).
           05  MT-MEMBER-ID             PIC X(10).
           05  MT-CARD-NO               PIC X(16).
           05  MT-SALE-ID               PIC X(10).
           05  MT-BILLED-AMT            PIC S9(11)V99 COMP-3.
           05  MT-FEE-AMT               PIC S9(11)V99 COMP-3.
           05  MT-POSTING-DT            PIC 9(08).
