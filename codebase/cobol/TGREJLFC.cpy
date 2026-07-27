      *================================================================
      * TGREJLFC -- TGREJLF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGREJLF-REC.
           05  RJ-REMIT-DT              PIC 9(08).
           05  RJ-CENTER-SEQ            PIC X(10).
           05  RJ-REJ-REASON            PIC X(04).
           05  RJ-PROGRAM-ID            PIC X(10).
           05  RJ-PAYEE-ACCT-NO         PIC X(16).
           05  RJ-PAYEE-NAME-KANA       PIC X(40).
           05  RJ-MASTER-NAME-KANA      PIC X(40).
           05  RJ-REJ-AMT               PIC S9(11)V99 COMP-3.
           05  RJ-LOG-TS                PIC X(14).
