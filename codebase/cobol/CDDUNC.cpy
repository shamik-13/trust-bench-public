      *================================================================
      * CDDUNC -- CDDUNF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDDUNF-REC.
           05  DUN-NOTICE-ID            PIC X(10).
           05  DUN-CARD-NO              PIC X(16).
           05  DUN-CYCLE-DT             PIC 9(08).
           05  DUN-DELINQ-AMT           PIC S9(11)V99 COMP-3.
           05  DUN-NOTICE-RANK          PIC X(10).
           05  DUN-CHANNEL-CD           PIC X(10).
           05  DUN-CREATE-DT            PIC 9(08).
