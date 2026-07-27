      *================================================================
      * CDCAPFC -- CDCAPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDCAPF-REC.
           05  BC-SALE-ID               PIC X(10).
           05  BC-CARD-NO               PIC X(16).
           05  BC-BILLED-AMT            PIC S9(11)V99 COMP-3.
           05  BC-FEE-AMT               PIC S9(11)V99 COMP-3.
           05  BC-CURRENCY-CD           PIC X(10).
           05  BC-CAP-STATUS            PIC X(02).
           05  BC-PROGRAM-ID            PIC X(10).
