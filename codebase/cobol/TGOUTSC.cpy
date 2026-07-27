      *================================================================
      * TGOUTSC -- TGOUTSF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGOUTSF-REC.
           05  OS-SEND-DT               PIC 9(08).
           05  OS-SEND-SEQ              PIC X(10).
           05  OS-REMIT-TYPE            PIC X(02).
           05  OS-SENDER-BANK           PIC X(04).
           05  OS-SENDER-BRANCH         PIC X(04).
           05  OS-PAYEE-BANK            PIC X(04).
           05  OS-PAYEE-BRANCH          PIC X(04).
           05  OS-PAYEE-ACCT-TYPE       PIC X(02).
           05  OS-PAYEE-ACCT-NO         PIC X(16).
           05  OS-PAYEE-NAME-KANA       PIC X(40).
           05  OS-SEND-AMT              PIC S9(11)V99 COMP-3.
           05  OS-SIGNATURE             PIC X(10).
           05  OS-BUILD-PROGRAM         PIC X(10).
