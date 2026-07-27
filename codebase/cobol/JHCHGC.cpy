      *================================================================
      * JHCHGC -- JHCHGEF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHCHGEF-REC.
           05  CHG-CHANGE-SEQ           PIC X(10).
           05  CHG-ACCT-NO              PIC X(16).
           05  CHG-CHANGE-TS            PIC X(14).
           05  CHG-CHANGE-TYPE          PIC X(02).
           05  CHG-BEFORE-HASH          PIC X(10).
           05  CHG-AFTER-HASH           PIC X(10).
           05  CHG-APPLY-STATUS         PIC X(02).
