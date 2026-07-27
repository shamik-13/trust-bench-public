      *================================================================
      * KZACCTC4 -- KZACCTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZACCTF-REC.
           05  AC-ACCT-NO               PIC X(16).
           05  AC-CUST-ID               PIC X(10).
           05  AC-ACCT-TYPE             PIC X(02).
           05  AC-GROUP-CODE            PIC X(04).
           05  AC-CUR-BAL               PIC S9(11)V99 COMP-3.
           05  AC-AVG-BAL               PIC S9(11)V99 COMP-3.
           05  AC-CREDIT-LIMIT          PIC S9(09)V99 COMP-3.
