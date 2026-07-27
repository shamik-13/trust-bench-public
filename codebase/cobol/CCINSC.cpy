      *================================================================
      * CCINSC -- CCINSF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CCINSF-REC.
           05  IN-INS-ID                PIC X(10).
           05  IN-RECV-DT               PIC 9(08).
           05  IN-ORG-CD                PIC X(10).
           05  IN-FCT-ID                PIC X(10).
           05  IN-INSTR-AMT             PIC S9(11)V99 COMP-3.
           05  IN-INSTR-KBN             PIC X(02).
           05  IN-INSTR-STATUS-KBN      PIC X(02).
