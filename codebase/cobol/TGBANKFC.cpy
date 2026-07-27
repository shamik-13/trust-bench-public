      *================================================================
      * TGBANKFC -- TGBANKF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  TGBANKF-REC.
           05  BK-COUNTER-BANK          PIC X(04).
           05  BK-BANK-NAME-KANA        PIC X(40).
           05  BK-CLEARING-GROUP        PIC X(10).
           05  BK-VALID-FROM            PIC X(10).
           05  BK-VALID-TO              PIC X(10).
           05  BK-STATUS                PIC X(10).
