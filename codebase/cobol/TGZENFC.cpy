      *================================================================
      * TGZENFC -- TGZENF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGZENF-REC.
           05  ZE-VALUE-DT              PIC 9(08).
           05  ZE-CENTER-SEQ            PIC X(10).
           05  ZE-ZEN-TYPE              PIC X(02).
           05  ZE-COUNTER-BANK          PIC X(04).
           05  ZE-COUNTER-BRANCH        PIC X(04).
           05  ZE-RECV-ACCT-NO          PIC X(16).
           05  ZE-REMIT-AMT             PIC S9(11)V99 COMP-3.
           05  ZE-REMIT-NAME-KANA       PIC X(40).
