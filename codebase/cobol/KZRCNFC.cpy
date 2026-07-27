      *================================================================
      * KZRCNFC -- KZRCNF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZRCNF-REC.
           05  RC-RECON-KEY             PIC X(10).
           05  RC-CYCLE-ID              PIC X(10).
           05  RC-GL-TOTAL-AMT          PIC S9(11)V99 COMP-3.
           05  RC-ACCRUAL-TOTAL-AMT     PIC S9(11)V99 COMP-3.
           05  RC-TAX-TOTAL-AMT         PIC S9(11)V99 COMP-3.
           05  RC-DIFF-AMT              PIC S9(11)V99 COMP-3.
           05  RC-DISPOSITION-CD        PIC X(10).
