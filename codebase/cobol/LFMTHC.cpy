      *================================================================
      * LFMTHC -- LFMTHF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFMTHF-REC.
           05  MT-SUMMARY-YM            PIC X(10).
           05  MT-PRODUCT-CD            PIC X(10).
           05  MT-CONTRACT-STATUS-KBN   PIC X(02).
           05  MT-POL-CNT               PIC 9(08).
           05  MT-RESERVE-TOTAL-AMT     PIC S9(11)V99 COMP-3.
           05  MT-CV-TOTAL-AMT          PIC S9(11)V99 COMP-3.
