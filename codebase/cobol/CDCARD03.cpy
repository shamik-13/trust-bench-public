      *================================================================
      * CDCARDFC -- CDCARDF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDCARDF-REC.
           05  CF-CARD-NO               PIC X(16).
           05  CF-MEMBER-ID             PIC X(10).
           05  CF-CARD-STATUS           PIC X(02).
           05  CF-CREDIT-LIMIT          PIC S9(11)V99 COMP-3.
           05  CF-MEMBER-NAME-KANA      PIC X(40).
