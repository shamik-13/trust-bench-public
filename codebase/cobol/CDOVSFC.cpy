      *================================================================
      * CDOVSFC -- CDOVSF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDOVSF-REC.
           05  OV-TXN-ID                PIC X(10).
           05  OV-CARD-NO               PIC X(16).
           05  OV-TXN-KBN               PIC X(02).
           05  OV-FEE-KBN               PIC X(02).
           05  OV-FEE-AMT               PIC S9(11)V99 COMP-3.
           05  OV-INT-START-DT          PIC 9(08).
           05  OV-SETL-AMT              PIC S9(11)V99 COMP-3.
           05  OV-SETL-KBN              PIC X(02).
           05  OV-PROGRAM-ID            PIC X(10).
