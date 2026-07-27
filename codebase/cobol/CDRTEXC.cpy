      *================================================================
      * CDRTEXC -- CDRTEXF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDRTEXF-REC.
           05  FX-CURRENCY-CD           PIC X(10).
           05  FX-RATE-DT               PIC S9(01)V9(04) COMP-3.
           05  FX-TTM-RATE              PIC S9(01)V9(04) COMP-3.
           05  FX-RATE-SOURCE           PIC S9(01)V9(04) COMP-3.
           05  FX-APPLY-STATUS          PIC X(02).
