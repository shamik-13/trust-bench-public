      *================================================================
      * CDFRDF2C -- CDFRDF2 レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDFRDF2-REC.
           05  FR-FRAUD-CASE-ID         PIC X(10).
           05  FR-SALE-ID               PIC X(10).
           05  FR-CARD-NO               PIC X(16).
           05  FR-MERCHANT-CODE         PIC X(04).
           05  FR-RISK-SCORE            PIC X(10).
           05  FR-RULE-HIT-CD           PIC X(10).
           05  FR-CASE-STATUS           PIC X(02).
