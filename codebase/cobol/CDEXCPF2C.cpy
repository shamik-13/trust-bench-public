      *================================================================
      * CDEXCPF2C -- CDEXCPF2 レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDEXCPF2-REC.
           05  EXP-EXCEPTION-ID         PIC X(10).
           05  EXP-PAY-ID               PIC X(10).
           05  EXP-CARD-NO              PIC X(16).
           05  EXP-EXCEPTION-CD         PIC X(10).
           05  EXP-EXCEPTION-AMT        PIC S9(11)V99 COMP-3.
           05  EXP-DETECTED-PROGRAM     PIC X(10).
           05  EXP-DETECTED-DT          PIC 9(08).
