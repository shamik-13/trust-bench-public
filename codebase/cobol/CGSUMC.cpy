      *================================================================
      * CGSUMC -- CGSUMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CGSUMF-REC.
           05  GS-SUMMARY-YYYYMM        PIC X(10).
           05  GS-SEGMENT-KBN           PIC X(02).
           05  GS-CUSTOMER-CNT          PIC 9(08).
           05  GS-ACTIVE-CNT            PIC 9(08).
           05  GS-STOP-CNT              PIC 9(08).
           05  GS-NEW-CNT               PIC 9(08).
