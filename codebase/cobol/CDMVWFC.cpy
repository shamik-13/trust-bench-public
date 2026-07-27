      *================================================================
      * CDMVWFC -- CDMVWF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDMVWF-REC.
           05  MV-CARD-NO               PIC X(16).
           05  MV-TXN-ID                PIC X(10).
           05  MV-DISP-KBN              PIC X(02).
           05  MV-DISP-AMT              PIC S9(11)V99 COMP-3.
           05  MV-DISP-LABEL            PIC X(10).
