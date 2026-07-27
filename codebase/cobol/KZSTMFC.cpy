      *================================================================
      * KZSTMFC -- KZSTMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZSTMF-REC.
           05  ST-ACCT-NO               PIC X(16).
           05  ST-CYCLE-ID              PIC X(10).
           05  ST-LINE-NO               PIC X(16).
           05  ST-STATEMENT-DT          PIC 9(08).
           05  ST-INT-AMT               PIC S9(11)V99 COMP-3.
           05  ST-TAX-AMT               PIC S9(11)V99 COMP-3.
           05  ST-NET-INT-AMT           PIC S9(11)V99 COMP-3.
           05  ST-PRINT-KBN             PIC X(02).
