      *================================================================
      * CDDLNQC -- CDDELINQF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDDELINQF-REC.
           05  DL-CARD-NO               PIC X(16).
           05  DL-CYCLE-DT              PIC 9(08).
           05  DL-DAYS-PAST-DUE         PIC X(10).
           05  DL-PAST-DUE-AMT          PIC S9(11)V99 COMP-3.
           05  DL-DUNNING-STAGE         PIC X(10).
           05  DL-EXTRACT-DT            PIC 9(08).
