      *================================================================
      * TGOUTCFC -- TGOUTCF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGOUTCF-REC.
           05  OC-CORRECTION-DT         PIC 9(08).
           05  OC-ORIG-CENTER-SEQ       PIC X(10).
           05  OC-CORRECTION-TYPE       PIC X(02).
           05  OC-SENDER-BANK           PIC X(04).
           05  OC-SENDER-BRANCH         PIC X(04).
           05  OC-PAYEE-BANK            PIC X(04).
           05  OC-PAYEE-BRANCH          PIC X(04).
           05  OC-PAYEE-ACCT-TYPE       PIC X(02).
           05  OC-PAYEE-ACCT-NO         PIC X(16).
           05  OC-PAYEE-NAME-KANA       PIC X(40).
           05  OC-OUT-AMT               PIC S9(11)V99 COMP-3.
           05  OC-OC-SIGNATURE          PIC X(20).
