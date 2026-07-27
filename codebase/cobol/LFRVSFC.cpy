      *================================================================
      * LFRVSFC -- LFRVSF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFRVSF-REC.
           05  RV-NOTICE-ID             PIC X(10).
           05  RV-POL-NO                PIC X(16).
           05  RV-NOTICE-YM             PIC X(10).
           05  RV-NOTICE-TYPE-KBN       PIC X(02).
           05  RV-OLD-PRM-AMT           PIC S9(11)V99 COMP-3.
           05  RV-NEW-PRM-AMT           PIC S9(11)V99 COMP-3.
           05  RV-NOTICE-STATUS-KBN     PIC X(02).
