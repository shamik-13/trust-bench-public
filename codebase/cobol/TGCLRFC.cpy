      *================================================================
      * TGCLRFC -- TGCLRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGCLRF-REC.
           05  CL-SETTLE-DT             PIC 9(08).
           05  CL-COUNTER-BANK          PIC X(04).
           05  CL-NET-AMT               PIC S9(13) COMP-3.
           05  CL-ITEM-COUNT            PIC 9(08).
           05  CL-SETTLE-STATUS         PIC X(01).
