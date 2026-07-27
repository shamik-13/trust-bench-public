      *================================================================
      * KZIRMFC -- KZIRMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZIRMF-REC.
           05  RM-RATE-KEY              PIC S9(01)V9(04) COMP-3.
           05  RM-PRODUCT-CD            PIC X(10).
           05  RM-EFFECTIVE-DT          PIC 9(08).
           05  RM-INT-RATE              PIC S9(01)V9(04) COMP-3.
           05  RM-WITHHOLD-TAX-KBN      PIC X(02).
           05  RM-OD-RATE               PIC S9(01)V9(04) COMP-3.
           05  RM-APPROVAL-STAT         PIC X(10).
