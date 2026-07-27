      *================================================================
      * KZCTLFC -- KZCTLF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZCTLF-REC.
           05  CL-CONTROL-KEY           PIC X(10).
           05  CL-RUN-DT                PIC 9(08).
           05  CL-CYCLE-ID              PIC X(10).
           05  CL-RESTART-POINT         PIC X(10).
           05  CL-RUN-STATUS            PIC X(02).
           05  CL-LAST-PGM-ID           PIC X(10).
