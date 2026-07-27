      *================================================================
      * CDCBKPC -- CDCBKPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDCBKPF-REC.
           05  CBK-CHARGEBACK-ID        PIC X(10).
           05  CBK-SALE-ID              PIC X(10).
           05  CBK-CARD-NO              PIC X(16).
           05  CBK-MERCHANT-CODE        PIC X(04).
           05  CBK-CLAIM-AMT            PIC S9(11)V99 COMP-3.
           05  CBK-CLAIM-REASON         PIC X(04).
           05  CBK-CASE-STATUS          PIC X(02).
