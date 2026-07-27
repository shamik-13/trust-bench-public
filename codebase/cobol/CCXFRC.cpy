      *================================================================
      * CCXFRC -- CCXFRF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  CCXFRF-REC.
           05  XF-XFER-ID               PIC X(10).
           05  XF-FCT-ID                PIC X(10).
           05  XF-FROM-ORG-CD           PIC X(10).
           05  XF-TO-ORG-CD             PIC X(10).
           05  XF-VALUE-DT              PIC 9(08).
           05  XF-XFER-AMT              PIC S9(11)V99 COMP-3.
           05  XF-XFER-STATUS-KBN       PIC X(02).
