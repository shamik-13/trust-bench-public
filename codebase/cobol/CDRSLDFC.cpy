      *================================================================
      * CDRSLDFC -- CDRSLDF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDRSLDF-REC.
           05  RS-CARD-NO               PIC X(16).
           05  RS-CYCLE-DT              PIC 9(08).
           05  RS-PRIN-AMT              PIC S9(11)V99 COMP-3.
           05  RS-FEE-AMT               PIC S9(11)V99 COMP-3.
           05  RS-PAY-AMT               PIC S9(11)V99 COMP-3.
           05  RS-SLIDE-TIER            PIC X(10).
           05  RS-RSLD-STATUS           PIC X(02).
           05  RS-PROGRAM-ID            PIC X(10).
