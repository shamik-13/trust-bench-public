      *================================================================
      * KZINTRFC -- KZINTRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZINTRF-REC.
           05  IR-RATE-CD               PIC S9(01)V9(04) COMP-3.
           05  IR-APR                   PIC X(10).
           05  IR-EFFECTIVE-DT          PIC 9(08).
           05  IR-ROUND-MODE            PIC X(10).
