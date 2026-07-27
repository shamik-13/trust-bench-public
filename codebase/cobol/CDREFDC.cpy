      *================================================================
      * CDREFDC -- CDREFDF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDREFDF-REC.
           05  REF-REFUND-ID            PIC X(10).
           05  REF-CARD-NO              PIC X(16).
           05  REF-PAY-ID               PIC X(10).
           05  REF-REFUND-AMT           PIC S9(11)V99 COMP-3.
           05  REF-BANK-CD              PIC X(10).
           05  REF-ACCOUNT-NO           PIC X(16).
           05  REF-REFUND-STATUS        PIC X(02).
           05  REF-APPROVAL-ID          PIC X(10).
