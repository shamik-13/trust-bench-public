      *================================================================
      * KZADLFC -- KZADLF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZADLF-REC.
           05  AL-AUDIT-ID              PIC X(10).
           05  AL-RUN-DATE              PIC X(10).
           05  AL-SOURCE-DSID           PIC X(10).
           05  AL-DEBIT-AMT             PIC S9(11)V99 COMP-3.
           05  AL-CREDIT-AMT            PIC S9(11)V99 COMP-3.
           05  AL-DIFF-AMT              PIC S9(11)V99 COMP-3.
           05  AL-STATUS-CD             PIC X(10).
