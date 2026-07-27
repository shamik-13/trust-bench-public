      *================================================================
      * CMATTC -- CMATTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CMATTF-REC.
           05  CA-CIF-NO                PIC X(16).
           05  CA-KANJI-NAME            PIC X(40).
           05  CA-KANA-NAME             PIC X(40).
           05  CA-ADDR-CD               PIC X(10).
           05  CA-PHONE-NO              PIC X(16).
           05  CA-UPDATE-DT             PIC 9(08).
           05  CA-ATTR-STATUS-KBN       PIC X(02).
