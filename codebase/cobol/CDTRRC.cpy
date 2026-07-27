      *================================================================
      * CDTRRC -- CDTRRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDTRRF-REC.
           05  TRR-RESULT-ID            PIC X(10).
           05  TRR-REQUEST-ID           PIC X(10).
           05  TRR-CARD-NO              PIC X(16).
           05  TRR-RESULT-CD            PIC X(10).
           05  TRR-SETTLED-AMT          PIC S9(11)V99 COMP-3.
           05  TRR-RETURN-REASON        PIC X(04).
           05  TRR-RESULT-DT            PIC 9(08).
