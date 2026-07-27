      *================================================================
      * KPPENRFC -- KPPENRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KPPENRF-REC.
           05  PR-PENALTY-RATE-CD       PIC S9(01)V9(04) COMP-3.
           05  PR-APR                   PIC X(10).
           05  PR-EFFECTIVE-DT          PIC 9(08).
           05  PR-ROUND-MODE            PIC X(10).
