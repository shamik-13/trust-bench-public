      *================================================================
      * JHRECC -- JHRECMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHRECMF-REC.
           05  REC-RECON-KEY            PIC X(10).
           05  REC-BUSINESS-DT          PIC 9(08).
           05  REC-SOURCE-CNT           PIC 9(08).
           05  REC-TARGET-CNT           PIC 9(08).
           05  REC-SOURCE-AMT           PIC S9(11)V99 COMP-3.
           05  REC-TARGET-AMT           PIC S9(11)V99 COMP-3.
           05  REC-DIFF-CNT             PIC 9(08).
           05  REC-DIFF-AMT             PIC S9(11)V99 COMP-3.
           05  REC-JUDGE-CD             PIC X(10).
