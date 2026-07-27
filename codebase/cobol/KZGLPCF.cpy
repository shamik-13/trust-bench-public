      *================================================================
      * KZGLPCF -- KZGLPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZGLPF-REC.
           05  GP-ACCT-NO               PIC X(16).
           05  GP-GL-DATE               PIC X(10).
           05  GP-DEBIT-ACCT-CD         PIC X(10).
           05  GP-CREDIT-ACCT-CD        PIC X(10).
           05  GP-JRNL-AMT              PIC S9(11)V99 COMP-3.
           05  GP-JRNL-TYPE             PIC X(02).
           05  GP-COST-CENTER-CD        PIC X(10).
