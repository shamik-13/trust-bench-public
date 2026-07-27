      *================================================================
      * CDCANC -- CDCANF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CDCANF-REC.
           05  CAN-CANCEL-ID            PIC X(10).
           05  CAN-PAY-ID               PIC X(10).
           05  CAN-CARD-NO              PIC X(16).
           05  CAN-CANCEL-AMT           PIC S9(11)V99 COMP-3.
           05  CAN-CANCEL-REASON        PIC X(04).
           05  CAN-REQUEST-USER         PIC X(10).
           05  CAN-CANCEL-DT            PIC 9(08).
