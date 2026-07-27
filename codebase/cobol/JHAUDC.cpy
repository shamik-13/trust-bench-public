      *================================================================
      * JHAUDC -- JHAUDTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHAUDTF-REC.
           05  AUD-AUDIT-SEQ            PIC X(10).
           05  AUD-JOB-ID               PIC X(10).
           05  AUD-BUSINESS-DT          PIC 9(08).
           05  AUD-EVENT-CD             PIC X(10).
           05  AUD-DATASET-ID           PIC X(10).
           05  AUD-REC-CNT              PIC 9(08).
           05  AUD-AMT-TOTAL            PIC S9(13) COMP-3.
           05  AUD-EVENT-TS             PIC X(14).
