      *================================================================
      * CDFXRFC -- CDFXRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDFXRF-REC.
           05  FX-RATE-DT               PIC S9(01)V9(04) COMP-3.
           05  FX-BRAND-KBN             PIC X(02).
           05  FX-CCY-CD                PIC X(10).
           05  FX-FX-RATE               PIC S9(01)V9(04) COMP-3.
           05  FX-MARKUP-RATE           PIC S9(01)V9(04) COMP-3.
           05  FX-SOURCE-KBN            PIC X(02).
