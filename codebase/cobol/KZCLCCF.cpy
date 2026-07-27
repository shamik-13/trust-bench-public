      *================================================================
      * KZCLCCF -- KZCLCF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZCLCF-REC.
           05  CLC-ACCT-NO              PIC X(16).
           05  CLC-CASE-OPEN-DT         PIC 9(08).
           05  CLC-CASE-STATUS          PIC X(02).
           05  CLC-COLLECTOR-ID         PIC X(10).
           05  CLC-PRIORITY-CODE        PIC X(04).
           05  CLC-LAST-ACTION-DT       PIC 9(08).
