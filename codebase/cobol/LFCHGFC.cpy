      *================================================================
      * LFCHGFC -- LFCHGF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFCHGF-REC.
           05  CG-CHANGE-ID             PIC X(10).
           05  CG-POL-NO                PIC X(16).
           05  CG-CHANGE-TYPE-KBN       PIC X(02).
           05  CG-APPLY-DATE            PIC X(10).
           05  CG-OLD-VALUE             PIC X(10).
           05  CG-NEW-VALUE             PIC X(10).
           05  CG-APPROVAL-STATUS-KBN   PIC X(02).
