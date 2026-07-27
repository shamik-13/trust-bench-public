      *================================================================
      * CDCAPF2C -- CDCAPF2 レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDCAPF2-REC.
           05  CP-CAPTURE-ID            PIC X(10).
           05  CP-TXN-ID                PIC X(10).
           05  CP-CARD-NO               PIC X(16).
           05  CP-CAPTURE-AMT           PIC S9(11)V99 COMP-3.
           05  CP-BRAND-KBN             PIC X(02).
           05  CP-CAPTURE-DT            PIC 9(08).
           05  CP-MATCH-KBN             PIC X(02).
