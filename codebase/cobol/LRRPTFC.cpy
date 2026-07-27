      *================================================================
      * LRRPTFC -- LRRPTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LRRPTF-REC.
           05  RP-REPORT-ID             PIC X(10).
           05  RP-REPORT-YM             PIC X(10).
           05  RP-REPORT-TYPE-KBN       PIC X(02).
           05  RP-POL-NO                PIC X(16).
           05  RP-LINE-NO               PIC X(16).
           05  RP-PRINT-AMT             PIC S9(11)V99 COMP-3.
           05  RP-OUTPUT-STATUS-KBN     PIC X(02).
