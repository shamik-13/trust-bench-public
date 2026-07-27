      *================================================================
      * LEJRNC -- LEJRNF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LEJRNF-REC.
           05  JR-JOURNAL-ID            PIC X(10).
           05  JR-POST-DATE             PIC X(10).
           05  JR-POL-NO                PIC X(16).
           05  JR-DR-ACCT-CD            PIC X(10).
           05  JR-CR-ACCT-CD            PIC X(10).
           05  JR-AMT                   PIC X(10).
           05  JR-JOURNAL-STATUS-KBN    PIC X(02).
