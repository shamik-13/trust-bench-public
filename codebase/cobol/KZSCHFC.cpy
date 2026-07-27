      *================================================================
      * KZSCHFC -- KZSCHF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZSCHF-REC.
           05  SC-CYCLE-ID              PIC X(10).
           05  SC-SCHEDULED-DT          PIC 9(08).
           05  SC-JOBNET-ID             PIC X(10).
           05  SC-PERIOD-ID             PIC X(10).
           05  SC-CALENDAR-CD           PIC X(10).
           05  SC-GENERATE-STATUS       PIC S9(01)V9(04) COMP-3.
