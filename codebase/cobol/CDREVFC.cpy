      *================================================================
      * CDREVFC -- CDREVF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDREVF-REC.
           05  RV-CARD-NO               PIC X(16).
           05  RV-MEMBER-ID             PIC X(10).
           05  RV-REV-STATUS            PIC X(02).
           05  RV-REV-COURSE-CD         PIC X(10).
           05  RV-MEMBER-NAME-KANA      PIC X(40).
           05  RV-REV-START-DT          PIC 9(08).
