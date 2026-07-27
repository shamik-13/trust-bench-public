      *================================================================
      * CDVELC -- CDVELF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDVELF-REC.
           05  VL-CARD-NO               PIC X(16).
           05  VL-WINDOW-START-TS       PIC X(14).
           05  VL-AUTH-COUNT-10M        PIC X(10).
           05  VL-AUTH-AMT-1H           PIC X(10).
           05  VL-LAST-AUTH-TS          PIC X(14).
           05  VL-VELOCITY-FLAG         PIC X(01).
