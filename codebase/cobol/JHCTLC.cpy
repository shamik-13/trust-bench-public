      *================================================================
      * JHCTLC -- JHCTLKF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHCTLKF-REC.
           05  CT-JOB-ID                PIC X(10).
           05  CT-BUSINESS-DT           PIC 9(08).
           05  CT-RUN-SEQ               PIC X(10).
           05  CT-STATUS-CD             PIC X(10).
           05  CT-START-TS              PIC X(14).
           05  CT-END-TS                PIC X(14).
           05  CT-RESTART-POS           PIC X(10).
           05  CT-INPUT-CNT             PIC 9(08).
           05  CT-OUTPUT-CNT            PIC 9(08).
