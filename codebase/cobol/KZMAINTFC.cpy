      *================================================================
      * KZMAINTFC -- KZMAINTF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZMAINTF-REC.
           05  MT-REQUEST-ID            PIC X(10).
           05  MT-REQUEST-TYPE          PIC X(02).
           05  MT-CUST-ID               PIC X(10).
           05  MT-ACCT-NO               PIC X(16).
           05  MT-EFFECTIVE-DATE        PIC X(10).
           05  MT-FIELD-CODE            PIC X(04).
           05  MT-NEW-VALUE             PIC X(10).
           05  MT-APPROVAL-ID           PIC X(10).
