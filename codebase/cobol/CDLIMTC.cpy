      *================================================================
      * CDLIMTC -- CDLIMTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDLIMTF-REC.
           05  LM-CARD-NO               PIC X(16).
           05  LM-TOTAL-LIMIT-AMT       PIC S9(11)V99 COMP-3.
           05  LM-REV-LIMIT-AMT         PIC S9(11)V99 COMP-3.
           05  LM-USED-AMT              PIC S9(11)V99 COMP-3.
           05  LM-TEMP-LIMIT-AMT        PIC S9(11)V99 COMP-3.
           05  LM-LIMIT-STATUS          PIC X(02).
