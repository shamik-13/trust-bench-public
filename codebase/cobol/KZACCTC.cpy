      *================================================================
      * KZACCTC -- KZACCTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZACCTF-REC.
           05  AC-ACCT-ID               PIC X(10).
           05  AC-CARD-NO               PIC X(16).
           05  AC-GROUP-CODE            PIC X(04).
           05  AC-CYCLE-BAL             PIC S9(11)V99 COMP-3.
           05  AC-CREDIT-LIMIT          PIC S9(11)V99 COMP-3.
           05  AC-OVER-AMT              PIC S9(11)V99 COMP-3.
           05  AC-KYC-STATUS            PIC X(02).
