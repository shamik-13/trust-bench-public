      *================================================================
      * CDEXCPC -- CDEXCPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDEXCPF-REC.
           05  EX-EXCEPTION-ID          PIC X(10).
           05  EX-SALE-ID               PIC X(10).
           05  EX-CARD-NO               PIC X(16).
           05  EX-REASON-CD             PIC X(10).
           05  EX-DETECTED-PGM          PIC X(10).
           05  EX-EXCEPTION-DT          PIC 9(08).
           05  EX-ACTION-STATUS         PIC X(02).
