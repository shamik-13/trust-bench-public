      *================================================================
      * KZEXPRC -- KZEXPRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZEXPRF-REC.
           05  XR-CUST-ID               PIC X(10).
           05  XR-PRODUCT-TYPE          PIC X(02).
           05  XR-EXPOSURE-AMT          PIC S9(11)V99 COMP-3.
           05  XR-CAPPED-AMT            PIC S9(11)V99 COMP-3.
           05  XR-OVER-FLAG             PIC X(01).
