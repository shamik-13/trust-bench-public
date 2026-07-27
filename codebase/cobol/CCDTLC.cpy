      *================================================================
      * CCDTLC -- CCDTLF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CCDTLF-REC.
           05  DL-VAL-ID                PIC X(10).
           05  DL-FCT-ID                PIC X(10).
           05  DL-ORG-CD                PIC X(10).
           05  DL-VALUE-DT              PIC 9(08).
           05  DL-DETAIL-AMT            PIC S9(11)V99 COMP-3.
           05  DL-DETAIL-STATUS-KBN     PIC X(02).
