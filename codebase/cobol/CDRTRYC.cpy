      *================================================================
      * CDRTRYC -- CDRTRYF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDRTRYF-REC.
           05  RTY-RETRY-ID             PIC X(10).
           05  RTY-CARD-NO              PIC X(16).
           05  RTY-ORIGINAL-REQUEST-ID  PIC X(10).
           05  RTY-RETRY-COUNT          PIC 9(08).
           05  RTY-NEXT-REQUEST-DT      PIC 9(08).
           05  RTY-RETRY-AMT            PIC S9(11)V99 COMP-3.
           05  RTY-RETRY-STATUS         PIC X(02).
