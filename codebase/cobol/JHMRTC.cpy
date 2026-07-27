      *================================================================
      * JHMRTC -- JHMARTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  JHMARTF-REC.
           05  MRT-MART-KEY             PIC X(10).
           05  MRT-YYYYMM               PIC X(10).
           05  MRT-ACCT-NO              PIC X(16).
           05  MRT-CUSTOMER-ID          PIC X(10).
           05  MRT-TRUST-PRODUCT-CD     PIC X(10).
           05  MRT-BRANCH-CD            PIC X(10).
           05  MRT-FEE-AMT              PIC S9(11)V99 COMP-3.
           05  MRT-FEE-YTD              PIC S9(11)V99 COMP-3.
           05  MRT-LOAD-DT              PIC 9(08).
