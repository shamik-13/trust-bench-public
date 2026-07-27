      *================================================================
      * KZRSCCF -- KZRSCF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZRSCF-REC.
           05  RSC-ACCT-NO              PIC X(16).
           05  RSC-REQUEST-DT           PIC 9(08).
           05  RSC-NEW-DUE-DT           PIC 9(08).
           05  RSC-NEW-PAYMENT-AMT      PIC S9(11)V99 COMP-3.
           05  RSC-APPROVAL-STATUS      PIC X(02).
