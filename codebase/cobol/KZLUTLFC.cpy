      *================================================================
      * KZLUTLFC -- KZLUTLF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZLUTLF-REC.
           05  LU-ACCT-NO               PIC X(16).
           05  LU-CUST-ID               PIC X(10).
           05  LU-AS-OF-DATE            PIC X(10).
           05  LU-CUR-BAL               PIC S9(11)V99 COMP-3.
           05  LU-CREDIT-LIMIT          PIC S9(11)V99 COMP-3.
           05  LU-UTIL-RATE             PIC S9(01)V9(04) COMP-3.
           05  LU-WARN-FLAG             PIC X(01).
