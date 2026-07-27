      *================================================================
      * CDMERC -- CDMERF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDMERF-REC.
           05  MR-MERCHANT-CODE         PIC X(04).
           05  MR-MERCHANT-NAME-KANA    PIC X(40).
           05  MR-MCC                   PIC X(10).
           05  MR-RISK-RANK             PIC X(10).
           05  MR-STATUS                PIC X(10).
           05  MR-COUNTRY-CD            PIC X(10).
