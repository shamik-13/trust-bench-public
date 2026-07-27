      *================================================================
      * LFPRMFC -- LFPRMF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  LFPRMF-REC.
           05  PR-PRM-ID                PIC X(10).
           05  PR-POL-NO                PIC X(16).
           05  PR-SUM-ASSURED-AMT       PIC S9(11)V99 COMP-3.
           05  PR-PRM-AMT               PIC S9(11)V99 COMP-3.
           05  PR-BAND-KBN              PIC X(02).
           05  PR-CALC-STATUS-KBN       PIC X(02).
