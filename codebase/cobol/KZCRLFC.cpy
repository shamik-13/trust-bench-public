      *================================================================
      * KZCRLFC -- KZCRLF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZCRLF-REC.
           05  CR-ACCT-NO               PIC X(16).
           05  CR-OLD-LIMIT             PIC S9(09)V99 COMP-3.
           05  CR-NEW-LIMIT             PIC S9(09)V99 COMP-3.
           05  CR-RAISE-FLAG            PIC X(01).
           05  CR-KYC-STATUS            PIC X(01).
           05  CR-REASON                PIC X(04).
