      *================================================================
      * LVCHGC -- LVCHGF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LVCHGF-REC.
           05  CH-CHANGE-ID             PIC X(10).
           05  CH-POL-NO                PIC X(16).
           05  CH-CHANGE-DATE           PIC X(10).
           05  CH-CHANGE-TYPE-KBN       PIC X(02).
           05  CH-BEFORE-STATUS-KBN     PIC X(02).
           05  CH-AFTER-STATUS-KBN      PIC X(02).
           05  CH-CHANGE-STATUS-KBN     PIC X(02).
