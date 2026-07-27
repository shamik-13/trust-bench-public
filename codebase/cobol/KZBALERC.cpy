      *================================================================
      * KZBALERC -- KZBALER レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZBALER-REC.
           05  BE-ACCT-ID               PIC X(10).
           05  BE-EDIT-DT               PIC 9(08).
           05  BE-ERROR-CD              PIC X(10).
           05  BE-SOURCE-PGM            PIC X(10).
           05  BE-DIFF-AMT              PIC S9(11)V99 COMP-3.
