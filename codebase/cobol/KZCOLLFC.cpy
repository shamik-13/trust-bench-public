      *================================================================
      * KZCOLLFC -- KZCOLLF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZCOLLF-REC.
           05  CL-COLLATERAL-ID         PIC X(10).
           05  CL-CUST-ID               PIC X(10).
           05  CL-ACCT-NO               PIC X(16).
           05  CL-COLLATERAL-TYPE       PIC X(02).
           05  CL-APPRAISAL-AMT         PIC S9(11)V99 COMP-3.
           05  CL-HAIRCUT-RATE          PIC S9(01)V9(04) COMP-3.
           05  CL-VALUATION-DATE        PIC X(10).
