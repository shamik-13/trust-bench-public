      *================================================================
      * TGRTRFC -- TGRTRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGRTRF-REC.
           05  RT-RETURN-KEY            PIC X(10).
           05  RT-VALUE-DT              PIC 9(08).
           05  RT-CENTER-SEQ            PIC X(10).
           05  RT-COUNTER-BANK          PIC X(04).
           05  RT-RECV-ACCT-NO          PIC X(16).
           05  RT-RETURN-REASON         PIC X(04).
           05  RT-REMIT-AMT             PIC S9(11)V99 COMP-3.
           05  RT-REENTRY-FLAG          PIC X(01).
