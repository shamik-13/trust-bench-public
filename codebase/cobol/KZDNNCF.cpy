      *================================================================
      * KZDNNCF -- KZDNNF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZDNNF-REC.
           05  DNN-ACCT-NO              PIC X(16).
           05  DNN-NOTICE-DATE          PIC X(10).
           05  DNN-NOTICE-TYPE          PIC X(02).
           05  DNN-NOTICE-COUNT         PIC 9(08).
           05  DNN-STOP-FLAG            PIC X(01).
           05  DNN-LEGAL-FLAG           PIC X(01).
           05  DNN-TEMPLATE-CODE        PIC X(04).
