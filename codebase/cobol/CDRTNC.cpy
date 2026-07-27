      *================================================================
      * CDRTNC -- CDRTNF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDRTNF-REC.
           05  RT-RETURN-ID             PIC X(10).
           05  RT-SALE-ID               PIC X(10).
           05  RT-CARD-NO               PIC X(16).
           05  RT-RETURN-AMT            PIC S9(11)V99 COMP-3.
           05  RT-RETURN-DT             PIC 9(08).
           05  RT-RETURN-REASON         PIC X(04).
           05  RT-APPROVAL-STATUS       PIC X(02).
