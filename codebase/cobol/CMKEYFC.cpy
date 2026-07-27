      *================================================================
      * CMKEYFC -- CMKEYF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CMKEYF-REC.
           05  CK-KEY-ID                PIC X(10).
           05  CK-CIF-NO                PIC X(16).
           05  CK-CHECK-DIGIT-CNT       PIC 9(08).
           05  CK-KEY-STATUS-KBN        PIC X(02).
