      *================================================================
      * LFDIVC -- LFDIVF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFDIVF-REC.
           05  DV-POL-NO                PIC X(16).
           05  DV-DIV-YEAR              PIC X(10).
           05  DV-DIV-AMT               PIC S9(11)V99 COMP-3.
           05  DV-DIV-ALLOC-KBN         PIC X(02).
           05  DV-DIV-STATUS-KBN        PIC X(02).
