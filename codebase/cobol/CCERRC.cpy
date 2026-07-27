      *================================================================
      * CCERRC -- CCERRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CCERRF-REC.
           05  ER-ERROR-ID              PIC X(10).
           05  ER-PGM-ID                PIC X(10).
           05  ER-BASE-DT               PIC 9(08).
           05  ER-RECORD-KEY            PIC X(10).
           05  ER-ERROR-KBN             PIC X(02).
           05  ER-ERROR-TEXT            PIC X(10).
