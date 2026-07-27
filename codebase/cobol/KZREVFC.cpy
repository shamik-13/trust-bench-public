      *================================================================
      * KZREVFC -- KZREVF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZREVF-REC.
           05  RV-REVERSAL-ID           PIC X(10).
           05  RV-ACCT-NO               PIC X(16).
           05  RV-CYCLE-ID              PIC X(10).
           05  RV-ORIG-INT-AMT          PIC S9(11)V99 COMP-3.
           05  RV-REV-INT-AMT           PIC S9(11)V99 COMP-3.
           05  RV-REASON-CD             PIC X(10).
           05  RV-APPROVED-ID           PIC X(10).
