      *================================================================
      * JHMONC -- JHMONRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHMONRF-REC.
           05  MON-YYYYMM               PIC X(10).
           05  MON-TRUST-PRODUCT-CD     PIC X(10).
           05  MON-BRANCH-CD            PIC X(10).
           05  MON-FEE-AMT-SUM          PIC S9(13) COMP-3.
           05  MON-FEE-YTD-SUM          PIC S9(13) COMP-3.
           05  MON-ACCT-CNT             PIC 9(08).
           05  MON-ADJUST-CNT           PIC 9(08).
