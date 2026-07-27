      *================================================================
      * TGRTNQC -- TGRTNQF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGRTNQF-REC.
           05  RT-RETURN-KEY            PIC X(10).
           05  RT-ORIG-REMIT-DT         PIC 9(08).
           05  RT-ORIG-CENTER-SEQ       PIC X(10).
           05  RT-RETURN-TYPE           PIC X(02).
           05  RT-PAYEE-BANK            PIC X(04).
           05  RT-PAYEE-BRANCH          PIC X(04).
           05  RT-PAYEE-ACCT-TYPE       PIC X(02).
           05  RT-PAYEE-ACCT-NO         PIC X(16).
           05  RT-PAYEE-NAME-KANA       PIC X(40).
           05  RT-RETURN-AMT            PIC S9(11)V99 COMP-3.
           05  RT-QUEUE-STATUS          PIC X(02).
