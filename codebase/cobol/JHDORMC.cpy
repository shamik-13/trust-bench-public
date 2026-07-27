      *================================================================
      * JHDORMC -- JHDORMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHDORMF-REC.
           05  DM-CUST-ID               PIC X(10).
           05  DM-DORMANT-FLAG          PIC X(01).
           05  DM-LAST-ACTIVITY-DATE    PIC X(10).
           05  DM-AVG-BAL               PIC S9(11)V99 COMP-3.
           05  DM-REVIEW-REASON-CD      PIC X(10).
           05  DM-UPDATE-DATE           PIC X(10).
