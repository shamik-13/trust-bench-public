      *================================================================
      * KZJNCFC -- KZJNCF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZJNCF-REC.
           05  JN-JOBNET-ID             PIC X(10).
           05  JN-CALENDAR-CD           PIC X(10).
           05  JN-SUPPRESS-HOLIDAY-FLAG PIC X(01).
           05  JN-RETRY-LIMIT           PIC S9(11)V99 COMP-3.
           05  JN-ACTIVE-FLAG           PIC X(01).
