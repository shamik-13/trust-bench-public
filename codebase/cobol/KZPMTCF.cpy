      *================================================================
      * KZPMTCF -- KZPMTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZPMTF-REC.
           05  PM-ACCT-NO               PIC X(16).
           05  PM-PMT-DATE              PIC X(10).
           05  PM-PMT-AMT               PIC S9(11)V99 COMP-3.
           05  PM-PMT-TYPE              PIC X(02).
           05  PM-APPLY-SEQ-NO          PIC X(16).
