      *================================================================
      * JHBIOC -- JHBIHOF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHBIHOF-REC.
           05  BIO-HANDOFF-ID           PIC X(10).
           05  BIO-FILE-DT              PIC 9(08).
           05  BIO-RECORD-TYPE          PIC X(02).
           05  BIO-PAYLOAD-LEN          PIC X(10).
           05  BIO-PAYLOAD-TEXT         PIC X(10).
           05  BIO-TRAILER-CNT          PIC 9(08).
           05  BIO-TRAILER-AMT          PIC S9(11)V99 COMP-3.
