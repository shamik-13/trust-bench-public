      *================================================================
      * JHREFMSTC -- JHREFMST レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHREFMST-REC.
           05  RF-REF-TYPE-CD           PIC X(10).
           05  RF-REF-CD                PIC X(10).
           05  RF-REF-NAME              PIC X(40).
           05  RF-VALID-FROM-DATE       PIC X(10).
           05  RF-VALID-TO-DATE         PIC X(10).
           05  RF-ACTIVE-FLAG           PIC X(01).
