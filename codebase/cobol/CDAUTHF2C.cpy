      *================================================================
      * CDAUTHF2C -- CDAUTHF2 レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDAUTHF2-REC.
           05  AU-AUTH-ID               PIC X(10).
           05  AU-CARD-NO               PIC X(16).
           05  AU-AUTH-AMT              PIC S9(11)V99 COMP-3.
           05  AU-CURRENCY-CD           PIC X(10).
           05  AU-AUTH-DT               PIC 9(08).
           05  AU-AUTH-STATUS           PIC X(02).
           05  AU-MERCHANT-CODE         PIC X(04).
