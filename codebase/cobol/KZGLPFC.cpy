      *================================================================
      * KZGLPFC -- KZGLPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZGLPF-REC.
           05  GL-JOURNAL-NO            PIC X(16).
           05  GL-POST-DT               PIC 9(08).
           05  GL-ACCT-NO               PIC X(16).
           05  GL-CYCLE-ID              PIC X(10).
           05  GL-DR-CR-KBN             PIC X(02).
           05  GL-GL-ACCT-CD            PIC X(10).
           05  GL-POST-AMT              PIC S9(11)V99 COMP-3.
           05  GL-TAX-KBN               PIC X(02).
           05  GL-SRC-PGM-ID            PIC X(10).
