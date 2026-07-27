      *================================================================
      * CDLOGFC -- CDLOGF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDLOGF-REC.
           05  LG-LOG-ID                PIC X(10).
           05  LG-PROGRAM-ID            PIC X(10).
           05  LG-CARD-NO               PIC X(16).
           05  LG-EVENT-KBN             PIC X(02).
           05  LG-EVENT-DT              PIC 9(08).
           05  LG-DETAIL-CD             PIC X(10).
