      *================================================================
      * JHMKEXPC -- JHMKEXPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHMKEXPF-REC.
           05  MX-EXPORT-DATE           PIC X(10).
           05  MX-CUST-ID               PIC X(10).
           05  MX-HOUSEHOLD-ID          PIC X(10).
           05  MX-SEG-CD                PIC X(10).
           05  MX-SEG-NAME              PIC X(40).
           05  MX-CAMPAIGN-ID           PIC X(10).
           05  MX-DM-PERMIT-FLAG        PIC X(01).
           05  MX-AVG-BAL               PIC S9(11)V99 COMP-3.
