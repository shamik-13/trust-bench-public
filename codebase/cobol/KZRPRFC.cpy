      *================================================================
      * KZRPRFC -- KZRPRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZRPRF-REC.
           05  RP-REPROCESS-ID          PIC X(10).
           05  RP-CYCLE-ID              PIC X(10).
           05  RP-ORIGINAL-DT           PIC 9(08).
           05  RP-REDATE-DT             PIC 9(08).
           05  RP-REQUEST-STATUS        PIC X(02).
           05  RP-APPROVER-ID           PIC X(10).
