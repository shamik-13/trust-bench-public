      *================================================================
      * JHCMPRC -- JHCMPRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHCMPRF-REC.
           05  CP-CAMPAIGN-ID           PIC X(10).
           05  CP-CUST-ID               PIC X(10).
           05  CP-SEG-CD                PIC X(10).
           05  CP-AGE-BAND-CD           PIC X(10).
           05  CP-AVG-BAL               PIC S9(11)V99 COMP-3.
           05  CP-TARGET-REASON-CD      PIC X(10).
           05  CP-EXTRACT-DATE          PIC X(10).
