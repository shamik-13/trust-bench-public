      *================================================================
      * KZODBFC -- KZODBF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZODBF-REC.
           05  OD-ACCT-NO               PIC X(16).
           05  OD-CYCLE-ID              PIC X(10).
           05  OD-OD-START-DT           PIC 9(08).
           05  OD-OD-END-DT             PIC 9(08).
           05  OD-NEG-AVG-BAL           PIC S9(11)V99 COMP-3.
           05  OD-OD-RATE               PIC S9(01)V9(04) COMP-3.
           05  OD-OD-INT-AMT            PIC S9(11)V99 COMP-3.
