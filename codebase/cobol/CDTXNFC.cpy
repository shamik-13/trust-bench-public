      *================================================================
      * CDTXNFC -- CDTXNF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDTXNF-REC.
           05  TX-TXN-ID                PIC X(10).
           05  TX-CARD-NO               PIC X(16).
           05  TX-TXN-KBN               PIC X(02).
           05  TX-CHANNEL-KBN           PIC X(02).
           05  TX-MERCHANT-KBN          PIC X(02).
           05  TX-TXN-AMT               PIC S9(11)V99 COMP-3.
           05  TX-TXN-DT                PIC 9(08).
           05  TX-AUTH-CD               PIC X(10).
