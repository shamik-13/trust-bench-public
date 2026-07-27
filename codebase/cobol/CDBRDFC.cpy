      *================================================================
      * CDBRDFC -- CDBRDF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDBRDF-REC.
           05  BR-BRAND-SETL-ID         PIC X(10).
           05  BR-TXN-ID                PIC X(10).
           05  BR-CARD-NO               PIC X(16).
           05  BR-BRAND-KBN             PIC X(02).
           05  BR-CCY-CD                PIC X(10).
           05  BR-BRAND-AMT             PIC S9(11)V99 COMP-3.
           05  BR-JPY-AMT               PIC S9(11)V99 COMP-3.
           05  BR-SETL-DT               PIC 9(08).
