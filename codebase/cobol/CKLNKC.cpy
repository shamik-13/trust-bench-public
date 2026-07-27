      *================================================================
      * CKLNKC -- CKLNKF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CKLNKF-REC.
           05  LK-LINK-ID               PIC X(10).
           05  LK-KEY-ID                PIC X(10).
           05  LK-CIF-NO                PIC X(16).
           05  LK-TARGET-SYS-ID         PIC X(10).
           05  LK-SEND-STATUS-KBN       PIC X(02).
           05  LK-SEND-DT               PIC 9(08).
