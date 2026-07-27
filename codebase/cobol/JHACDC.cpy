      *================================================================
      * JHACDC -- JHACDMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHACDMF-REC.
           05  ACD-ACCT-NO              PIC X(16).
           05  ACD-CUSTOMER-ID          PIC X(10).
           05  ACD-TRUST-PRODUCT-CD     PIC X(10).
           05  ACD-BRANCH-CD            PIC X(10).
           05  ACD-OPEN-DT              PIC 9(08).
           05  ACD-CLOSE-DT             PIC 9(08).
           05  ACD-STATUS-CD            PIC X(10).
           05  ACD-LAST-CHG-TS          PIC X(14).
