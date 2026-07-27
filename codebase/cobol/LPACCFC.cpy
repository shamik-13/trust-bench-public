      *================================================================
      * LPACCFC -- LPACCF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LPACCF-REC.
           05  AC-POL-NO                PIC X(16).
           05  AC-BANK-CD               PIC X(10).
           05  AC-BRANCH-CD             PIC X(10).
           05  AC-ACCOUNT-NO            PIC X(16).
           05  AC-ACCOUNT-HOLDER-KANA   PIC X(40).
           05  AC-TRANSFER-DAY          PIC X(10).
           05  AC-ACCOUNT-STATUS-KBN    PIC X(02).
