      *================================================================
      * CDCOURC -- CDCOURF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDCOURF-REC.
           05  CRS-COURSE-CD            PIC X(10).
           05  CRS-COURSE-NAME          PIC X(40).
           05  CRS-FEE-RATE             PIC S9(01)V9(04) COMP-3.
           05  CRS-MIN-PAY-AMT          PIC S9(11)V99 COMP-3.
           05  CRS-COURSE-STATUS        PIC X(02).
