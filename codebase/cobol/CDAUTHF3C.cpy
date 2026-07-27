      *================================================================
      * CDAUTHF3C -- CDAUTHF3 レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDAUTHF3-REC.
           05  AU-AUTH-NO               PIC X(16).
           05  AU-CARD-NO               PIC X(16).
           05  AU-AUTH-DT               PIC 9(08).
           05  AU-MERCHANT-ID           PIC X(10).
           05  AU-AUTH-AMT              PIC S9(11)V99 COMP-3.
           05  AU-AUTH-STATUS           PIC X(02).
           05  AU-REV-USE-FLG           PIC X(10).
           05  AU-APPROVAL-CD           PIC X(10).
