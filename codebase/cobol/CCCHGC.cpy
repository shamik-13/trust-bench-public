      *================================================================
      * CCCHGC -- CCCHGF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CCCHGF-REC.
           05  CH-CHANGE-ID             PIC X(10).
           05  CH-FCT-ID                PIC X(10).
           05  CH-CHANGE-DT             PIC 9(08).
           05  CH-CHANGE-KBN            PIC X(02).
           05  CH-BEFORE-STATUS-KBN     PIC X(02).
           05  CH-AFTER-STATUS-KBN      PIC X(02).
