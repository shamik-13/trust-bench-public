      *================================================================
      * CDMERCC -- CDMERCF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDMERCF-REC.
           05  MC-MERCHANT-CODE         PIC X(04).
           05  MC-MERCHANT-NAME-KANA    PIC X(40).
           05  MC-SETTLE-BANK-CD        PIC X(10).
           05  MC-SETTLE-ACCOUNT-NO     PIC X(16).
           05  MC-MERCHANT-STATUS       PIC X(02).
           05  MC-FEE-PLAN-CD           PIC X(10).
