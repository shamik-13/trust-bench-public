      *================================================================
      * KZPMRFC -- KZPMRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZPMRF-REC.
           05  PM-TAX-OFFICE-CD         PIC X(10).
           05  PM-PAYMENT-DATE          PIC X(10).
           05  PM-ACCT-NO               PIC X(16).
           05  PM-ACCT-TYPE             PIC X(02).
           05  PM-NATIONAL-TAX-AMT      PIC S9(11)V99 COMP-3.
           05  PM-SPECIAL-RECON-AMT     PIC S9(11)V99 COMP-3.
           05  PM-PAYMENT-REF-NO        PIC X(16).
