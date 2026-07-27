      *================================================================
      * LPPAYFC -- LPPAYF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LPPAYF-REC.
           05  PY-PAY-ID                PIC X(10).
           05  PY-POL-NO                PIC X(16).
           05  PY-DUE-YM                PIC X(10).
           05  PY-PAY-AMT               PIC S9(11)V99 COMP-3.
           05  PY-PAY-DATE              PIC X(10).
           05  PY-PAY-CHANNEL-KBN       PIC X(02).
           05  PY-MATCH-STATUS-KBN      PIC X(02).
