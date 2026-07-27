      *================================================================
      * KZGLDTFC -- KZGLDTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZGLDTF-REC.
           05  GL-GL-BATCH-ID           PIC X(10).
           05  GL-ACCT-ID               PIC X(10).
           05  GL-ENTRY-CD              PIC X(10).
           05  GL-DR-CR-KBN             PIC X(02).
           05  GL-ENTRY-AMT             PIC S9(11)V99 COMP-3.
           05  GL-POST-DT               PIC 9(08).
