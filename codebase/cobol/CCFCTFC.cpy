      *================================================================
      * CCFCTFC -- CCFCTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CCFCTF-REC.
           05  FC-FCT-ID                PIC X(10).
           05  FC-TRIGGER-DT            PIC 9(08).
           05  FC-CONC-AMT              PIC S9(11)V99 COMP-3.
           05  FC-FCT-STATUS-KBN        PIC X(02).
