      *================================================================
      * TGERRFC -- TGERRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGERRF-REC.
           05  ER-VALUE-DT              PIC 9(08).
           05  ER-CENTER-SEQ            PIC X(10).
           05  ER-ERROR-CD              PIC X(10).
           05  ER-ERROR-FIELD           PIC X(10).
           05  ER-ERROR-TEXT            PIC X(10).
           05  ER-RAW-RECORD            PIC X(10).
