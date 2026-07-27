      *================================================================
      * JHSEGDC -- JHSEGDST レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHSEGDST-REC.
           05  SD-REPORT-DATE           PIC X(10).
           05  SD-SEG-CD                PIC X(10).
           05  SD-AGE-BAND-CD           PIC X(10).
           05  SD-PREF-CD               PIC X(10).
           05  SD-CUSTOMER-COUNT        PIC 9(08).
           05  SD-AVG-BAL-SUM           PIC S9(13) COMP-3.
           05  SD-AVG-BAL-MEAN          PIC S9(11)V99 COMP-3.
