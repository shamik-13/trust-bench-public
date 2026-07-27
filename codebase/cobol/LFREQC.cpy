      *================================================================
      * LFREQC -- LFREQF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFREQF-REC.
           05  RQ-REQ-ID                PIC X(10).
           05  RQ-POL-NO                PIC X(16).
           05  RQ-REQ-DATE              PIC X(10).
           05  RQ-REQ-TYPE-KBN          PIC X(02).
           05  RQ-REQ-STATUS-KBN        PIC X(02).
           05  RQ-OPERATOR-ID           PIC X(10).
           05  RQ-RECEIPT-BRANCH-CD     PIC X(10).
