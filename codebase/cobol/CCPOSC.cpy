      *================================================================
      * CCPOSC -- CCPOSF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CCPOSF-REC.
           05  PS-ORG-CD                PIC X(10).
           05  PS-BASE-DT               PIC 9(08).
           05  PS-AVAILABLE-AMT         PIC S9(11)V99 COMP-3.
           05  PS-RESERVED-AMT          PIC S9(11)V99 COMP-3.
           05  PS-POSITION-STATUS-KBN   PIC X(02).
