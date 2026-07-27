      *================================================================
      * LPCLMFC -- LPCLMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LPCLMF-REC.
           05  CL-CLAIM-ID              PIC X(10).
           05  CL-POL-NO                PIC X(16).
           05  CL-DUE-YM                PIC X(10).
           05  CL-BILL-AMT              PIC S9(11)V99 COMP-3.
           05  CL-RECEIPT-AMT           PIC S9(11)V99 COMP-3.
           05  CL-CLAIM-STATUS-KBN      PIC X(02).
           05  CL-TRANSFER-RESULT-KBN   PIC X(02).
