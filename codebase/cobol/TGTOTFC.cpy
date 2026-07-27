      *================================================================
      * TGTOTFC -- TGTOTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGTOTF-REC.
           05  TT-VALUE-DT              PIC 9(08).
           05  TT-COUNTER-BANK          PIC X(04).
           05  TT-TOTAL-TYPE            PIC X(02).
           05  TT-ITEM-COUNT            PIC X(10).
           05  TT-TOTAL-AMT             PIC S9(11)V99 COMP-3.
           05  TT-MATCH-STATUS          PIC X(02).
