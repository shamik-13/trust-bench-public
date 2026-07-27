      *================================================================
      * CDNOTIC -- CDNOTIF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDNOTIF-REC.
           05  NT-NOTICE-ID             PIC X(10).
           05  NT-CARD-NO               PIC X(16).
           05  NT-NOTICE-DT             PIC 9(08).
           05  NT-NOTICE-TYPE           PIC X(02).
           05  NT-NOTICE-AMT            PIC S9(11)V99 COMP-3.
           05  NT-NOTICE-STATUS         PIC X(02).
