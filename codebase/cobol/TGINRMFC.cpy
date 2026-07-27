      *================================================================
      * TGINRMFC -- TGINRMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGINRMF-REC.
           05  IR-REMIT-DT              PIC 9(08).
           05  IR-CENTER-SEQ            PIC X(10).
           05  IR-REMIT-TYPE            PIC X(02).
           05  IR-SENDER-BANK           PIC X(04).
           05  IR-SENDER-BRANCH         PIC X(04).
           05  IR-SENDER-NAME-KANA      PIC X(40).
           05  IR-PAYEE-BANK            PIC X(04).
           05  IR-PAYEE-BRANCH          PIC X(04).
           05  IR-PAYEE-ACCT-TYPE       PIC X(02).
           05  IR-PAYEE-ACCT-NO         PIC X(16).
           05  IR-PAYEE-NAME-KANA       PIC X(40).
           05  IR-REMIT-AMT             PIC S9(11)V99 COMP-3.
           05  IR-REMIT-MSG             PIC X(20).
