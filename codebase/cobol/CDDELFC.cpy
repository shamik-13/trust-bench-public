      *================================================================
      * CDDELFC -- CDDELF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDDELF-REC.
           05  DL-CARD-NO               PIC X(16).
           05  DL-BILLING-DT            PIC 9(08).
           05  DL-DELAY-DAYS            PIC X(10).
           05  DL-DUE-AMT               PIC S9(11)V99 COMP-3.
           05  DL-NOTICE-KBN            PIC X(02).
           05  DL-EXTRACT-DT            PIC 9(08).
