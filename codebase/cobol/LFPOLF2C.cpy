      *================================================================
      * LFPOLF2C -- LFPOLF2 レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFPOLF2-REC.
           05  PO-POL-NO                PIC X(16).
           05  PO-PRODUCT-CD            PIC X(10).
           05  PO-INSURED-ID            PIC X(10).
           05  PO-CONTRACTOR-ID         PIC X(10).
           05  PO-CONTRACT-STATUS-KBN   PIC X(02).
           05  PO-ISSUE-DATE            PIC X(10).
           05  PO-PAID-TO-DATE          PIC X(10).
           05  PO-SUM-INSURED-AMT       PIC S9(11)V99 COMP-3.
