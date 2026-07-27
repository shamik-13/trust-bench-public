      *================================================================
      * KZAUDCF -- KZAUDF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZAUDF-REC.
           05  AUD-ACCT-NO              PIC X(16).
           05  AUD-EVENT-DT             PIC 9(08).
           05  AUD-EVENT-TYPE           PIC X(02).
           05  AUD-OLD-STATUS           PIC X(02).
           05  AUD-NEW-STATUS           PIC X(02).
           05  AUD-CHANGE-AMT           PIC S9(11)V99 COMP-3.
