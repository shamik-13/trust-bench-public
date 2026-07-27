      *================================================================
      * CDHISTC -- CDHISTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDHISTF-REC.
           05  HIS-CARD-NO              PIC X(16).
           05  HIS-PAY-ID               PIC X(10).
           05  HIS-EVENT-SEQ            PIC X(10).
           05  HIS-EVENT-TYPE           PIC X(02).
           05  HIS-EVENT-AMT            PIC S9(11)V99 COMP-3.
           05  HIS-EVENT-DT             PIC 9(08).
           05  HIS-SOURCE-PROGRAM       PIC X(10).
