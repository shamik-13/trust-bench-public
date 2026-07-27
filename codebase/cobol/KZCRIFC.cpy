      *================================================================
      * KZCRIFC -- KZCRIF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZCRIF-REC.
           05  CR-CORRECTION-NO         PIC X(16).
           05  CR-ORIG-ACCT-NO          PIC X(16).
           05  CR-ORIG-PAYMENT-DATE     PIC X(10).
           05  CR-CORRECTION-REASON-CD  PIC X(10).
           05  CR-REV-GROSS-AMT         PIC S9(11)V99 COMP-3.
           05  CR-REV-NATIONAL-TAX-AMT  PIC S9(11)V99 COMP-3.
           05  CR-REV-LOCAL-TAX-AMT     PIC S9(11)V99 COMP-3.
