      *================================================================
      * CDAPPFC -- CDAPPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDAPPF-REC.
           05  AP-PAY-ID                PIC X(10).
           05  AP-CARD-NO               PIC X(16).
           05  AP-APPLIED-FEE-AMT       PIC S9(11)V99 COMP-3.
           05  AP-APPLIED-INT-AMT       PIC S9(11)V99 COMP-3.
           05  AP-APPLIED-PRIN-AMT      PIC S9(11)V99 COMP-3.
           05  AP-REMAIN-AMT            PIC S9(11)V99 COMP-3.
           05  AP-APP-STATUS            PIC X(02).
           05  AP-PROGRAM-ID            PIC X(10).
