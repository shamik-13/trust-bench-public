      *================================================================
      * CDAUTHF4C -- CDAUTHF4 レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDAUTHF4-REC.
           05  AU-AUTH-ID               PIC X(10).
           05  AU-CARD-NO               PIC X(16).
           05  AU-TXN-ID                PIC X(10).
           05  AU-AUTH-KBN              PIC X(02).
           05  AU-REQ-AMT               PIC S9(11)V99 COMP-3.
           05  AU-APPROVED-AMT          PIC S9(11)V99 COMP-3.
           05  AU-REASON-CD             PIC X(10).
           05  AU-AUTH-DT               PIC 9(08).
