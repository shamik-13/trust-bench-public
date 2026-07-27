      *================================================================
      * CDSIDXC -- CDSTMTIDXF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDSTMTIDXF-REC.
           05  SI-CARD-NO               PIC X(16).
           05  SI-CYCLE-DT              PIC 9(08).
           05  SI-STATEMENT-ID          PIC X(10).
           05  SI-BILL-AMT              PIC S9(11)V99 COMP-3.
           05  SI-DUE-DT                PIC 9(08).
           05  SI-PUBLISH-STATUS        PIC X(02).
