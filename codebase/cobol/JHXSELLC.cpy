      *================================================================
      * JHXSELLC -- JHXSELLF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHXSELLF-REC.
           05  XS-CUST-ID               PIC X(10).
           05  XS-SEG-CD                PIC X(10).
           05  XS-CANDIDATE-PRODUCT-CD  PIC X(10).
           05  XS-SCORE-RANK            PIC X(10).
           05  XS-AVG-BAL               PIC S9(11)V99 COMP-3.
           05  XS-REASON-CD             PIC X(10).
