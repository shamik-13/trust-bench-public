      *================================================================
      * LFREPC -- LFREPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFREPF-REC.
           05  RP-REPORT-ID             PIC X(10).
           05  RP-LINE-NO               PIC X(16).
           05  RP-POL-NO                PIC X(16).
           05  RP-PRINT-KBN             PIC X(02).
           05  RP-PRINT-AMT             PIC S9(11)V99 COMP-3.
           05  RP-ERROR-KBN             PIC X(02).
