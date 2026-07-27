      *================================================================
      * CDSTMFC -- CDSTMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDSTMF-REC.
           05  ST-CARD-NO               PIC X(16).
           05  ST-STATEMENT-ID          PIC X(10).
           05  ST-TXN-ID                PIC X(10).
           05  ST-LINE-KBN              PIC X(02).
           05  ST-LINE-AMT              PIC S9(11)V99 COMP-3.
           05  ST-LINE-LABEL            PIC X(10).
