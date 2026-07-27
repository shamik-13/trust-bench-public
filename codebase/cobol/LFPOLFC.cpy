      *================================================================
      * LFPOLFC -- LFPOLF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFPOLF-REC.
           05  PO-POL-NO                PIC X(16).
           05  PO-ENTRY-AGE-CNT         PIC 9(08).
           05  PO-SEX-KBN               PIC X(02).
           05  PO-SUM-ASSURED-AMT       PIC S9(11)V99 COMP-3.
           05  PO-POL-STATUS-KBN        PIC X(02).
