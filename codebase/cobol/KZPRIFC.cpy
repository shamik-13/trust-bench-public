      *================================================================
      * KZPRIFC -- KZPRIF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZPRIF-REC.
           05  PR-ACCT-NO               PIC X(16).
           05  PR-PAYEE-NAME-KANA       PIC X(40).
           05  PR-PAYEE-ADDR-CD         PIC X(10).
           05  PR-PAYMENT-DATE          PIC X(10).
           05  PR-GROSS-INT-AMT         PIC S9(11)V99 COMP-3.
           05  PR-NATIONAL-TAX-AMT      PIC S9(11)V99 COMP-3.
           05  PR-LOCAL-TAX-AMT         PIC S9(11)V99 COMP-3.
           05  PR-STATEMENT-CLASS       PIC X(10).
