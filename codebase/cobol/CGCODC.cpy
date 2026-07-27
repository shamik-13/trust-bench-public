      *================================================================
      * CGCODC -- CGCODF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CGCODF-REC.
           05  GC-CODE-ID               PIC X(10).
           05  GC-CODE-KBN              PIC X(02).
           05  GC-CODE-VALUE            PIC X(10).
           05  GC-CODE-NAME             PIC X(40).
           05  GC-VALID-FROM-DT         PIC 9(08).
           05  GC-VALID-TO-DT           PIC 9(08).
