      *================================================================
      * CMMOVC -- CMMOVF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CMMOVF-REC.
           05  MV-RECEIPT-NO            PIC X(16).
           05  MV-CIF-NO                PIC X(16).
           05  MV-MOVE-KBN              PIC X(02).
           05  MV-REQUEST-DT            PIC 9(08).
           05  MV-OPERATOR-ID           PIC X(10).
           05  MV-MOVE-STATUS-KBN       PIC X(02).
