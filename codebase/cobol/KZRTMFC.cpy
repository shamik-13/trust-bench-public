      *================================================================
      * KZRTMFC -- KZRTMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZRTMF-REC.
           05  RT-RATE-KEY              PIC S9(01)V9(04) COMP-3.
           05  RT-EFFECTIVE-DATE        PIC X(10).
           05  RT-TAX-KIND              PIC X(10).
           05  RT-RATE-NUMERATOR        PIC S9(01)V9(04) COMP-3.
           05  RT-RATE-DENOMINATOR      PIC S9(01)V9(04) COMP-3.
           05  RT-AUTH-REF-NO           PIC X(16).
