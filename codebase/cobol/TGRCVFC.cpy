      *================================================================
      * TGRCVFC -- TGRCVF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGRCVF-REC.
           05  RV-CENTER-SEQ            PIC X(10).
           05  RV-VALUE-DT              PIC 9(08).
           05  RV-RAW-RECORD            PIC X(10).
           05  RV-RECV-CENTER           PIC X(10).
           05  RV-FILE-TYPE             PIC X(02).
           05  RV-ACCEPT-TS             PIC X(14).
           05  RV-ACCEPT-STATUS         PIC X(02).
