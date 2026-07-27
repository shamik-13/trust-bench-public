      *================================================================
      * CDSTSC -- CDSTSF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDSTSF-REC.
           05  ST-CARD-NO               PIC X(16).
           05  ST-NEW-STATUS            PIC X(02).
           05  ST-SOURCE-SYS            PIC X(10).
           05  ST-STATUS-TS             PIC X(14).
           05  ST-REASON-CD             PIC X(10).
