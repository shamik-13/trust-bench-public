      *================================================================
      * KZLCRFC -- KZLCRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZLCRF-REC.
           05  LC-PREFECTURE-CD         PIC X(10).
           05  LC-REMITTANCE-DATE       PIC X(10).
           05  LC-ACCT-TYPE             PIC X(02).
           05  LC-LOCAL-TAX-AMT         PIC S9(11)V99 COMP-3.
           05  LC-RECORD-COUNT          PIC 9(08).
           05  LC-BANK-REF-NO           PIC X(16).
