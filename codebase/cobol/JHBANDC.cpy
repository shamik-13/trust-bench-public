      *================================================================
      * JHBANDC -- JHBANDRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHBANDRF-REC.
           05  BD-REPORT-DATE           PIC X(10).
           05  BD-BAND-CD               PIC X(10).
           05  BD-SEG-CD                PIC X(10).
           05  BD-CUSTOMER-COUNT        PIC 9(08).
           05  BD-BALANCE-SUM           PIC S9(13) COMP-3.
