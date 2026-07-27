      *================================================================
      * KZTXRFC -- KZTXRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZTXRF-REC.
           05  TR-ACCT-NO               PIC X(16).
           05  TR-ACCT-TYPE             PIC X(02).
           05  TR-GROSS-INT-AMT         PIC S9(11)V99 COMP-3.
           05  TR-NATIONAL-TAX-AMT      PIC S9(13).
           05  TR-LOCAL-TAX-AMT         PIC S9(13).
           05  TR-TOTAL-TAX-AMT         PIC S9(13).
           05  TR-NET-AMT               PIC S9(13).
