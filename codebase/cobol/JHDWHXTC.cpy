      *================================================================
      * JHDWHXTC -- JHDWHXTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHDWHXTF-REC.
           05  DX-SNAPSHOT-DATE         PIC X(10).
           05  DX-CUST-ID               PIC X(10).
           05  DX-SEG-CD                PIC X(10).
           05  DX-AGE-BAND-CD           PIC X(10).
           05  DX-PREF-CD               PIC X(10).
           05  DX-HOUSEHOLD-ID          PIC X(10).
           05  DX-AVG-BAL               PIC S9(11)V99 COMP-3.
           05  DX-DORMANT-FLAG          PIC X(01).
