      *================================================================
      * KZDGRPC -- KZDGRPF レコードレイアウト (record layout). Shared/pinned.
      *================================================================
       01  KZDGRPF-REC.
           05  DG-GROUP-CODE            PIC X(04).
           05  DG-OL-FEE-RATE           PIC S9(01)V9(04) COMP-3.
           05  DG-OL-FEE-RATE-OLD       PIC S9(01)V9(04) COMP-3.
           05  DG-EXEMPT-FLAG           PIC X(01).
