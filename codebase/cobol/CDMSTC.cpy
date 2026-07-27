      *================================================================
      * CDMSTC -- CDMEMSTATF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDMEMSTATF-REC.
           05  MS-MEMBER-ID             PIC X(10).
           05  MS-STATUS-CD             PIC X(10).
           05  MS-STATUS-REASON         PIC X(04).
           05  MS-EFFECTIVE-DT          PIC 9(08).
           05  MS-LAST-UPDATED-TS       PIC X(14).
