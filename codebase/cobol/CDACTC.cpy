      *================================================================
      * CDACTC -- CDACTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDACTF-REC.
           05  AC-ACCOUNT-ID            PIC X(10).
           05  AC-CARD-NO               PIC X(16).
           05  AC-BANK-CD               PIC X(10).
           05  AC-BRANCH-CD             PIC X(10).
           05  AC-DEPOSIT-TYPE          PIC X(02).
           05  AC-ACCOUNT-NO            PIC X(16).
           05  AC-HOLDER-KANA           PIC X(40).
           05  AC-TRANSFER-STATUS       PIC X(02).
