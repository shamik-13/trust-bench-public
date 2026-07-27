      *================================================================
      * CDAUTHFC -- CDAUTHF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDAUTHF-REC.
           05  AU-AUTH-ID               PIC X(10).
           05  AU-CARD-NO               PIC X(16).
           05  AU-AUTH-AMT              PIC S9(11)V99 COMP-3.
           05  AU-AUTH-RESULT           PIC X(10).
           05  AU-MERCHANT-CODE         PIC X(04).
           05  AU-CURRENCY-CD           PIC X(10).
           05  AU-AUTH-TS               PIC X(14).
           05  AU-HOLD-EXP-DT           PIC 9(08).
