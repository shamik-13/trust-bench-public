      *================================================================
      * CDFRDC -- CDFRDF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDFRDF-REC.
           05  FR-AUTH-ID               PIC X(10).
           05  FR-CARD-NO               PIC X(16).
           05  FR-FRAUD-SCORE           PIC X(10).
           05  FR-RULE-HIT-CD           PIC X(10).
           05  FR-MODEL-VERSION         PIC X(10).
           05  FR-SCORE-TS              PIC X(14).
