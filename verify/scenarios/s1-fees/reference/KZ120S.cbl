       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ120S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF
               ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS WS-ACCT-STAT.
           SELECT KZDGRPF
               ASSIGN TO "KZDGRPF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DG-GROUP-CODE
               FILE STATUS IS WS-DGRP-STAT.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
       COPY KZACCTC.
       FD  KZDGRPF.
       COPY KZDGRPC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-ACCT-STAT        PIC XX.
           05  WS-DGRP-STAT        PIC XX.
       01  WS-WORK.
           05  WS-EFFECTIVE-GROUP  PIC X(04).
           05  WS-RATE             PIC S9(01)V9(04) COMP-3.
           05  WS-ACCT-FOUND       PIC X VALUE "N".
               88  ACCT-FOUND      VALUE "Y".
           05  WS-EOF              PIC X VALUE "N".
               88  END-OF-ACCT     VALUE "Y".
           05  WS-FILES-OPEN       PIC X VALUE "N".
               88  FILES-OPEN      VALUE "Y".
       01  WS-CONSTANTS.
           05  CN-STD0             PIC X(04) VALUE "STD0".
           05  CN-GLD1             PIC X(04) VALUE "GLD1".
           05  CN-PLT2             PIC X(04) VALUE "PLT2".
           05  CN-EXMP             PIC X(04) VALUE "EXMP".
           05  CN-PREM             PIC X(04) VALUE "PREM".
           05  CN-RATE-STD         PIC S9(01)V9(04) COMP-3
               VALUE 0.0150.
           05  CN-RATE-GLD         PIC S9(01)V9(04) COMP-3
               VALUE 0.0100.
           05  CN-RATE-PLT         PIC S9(01)V9(04) COMP-3
               VALUE 0.0080.
           05  CN-RATE-ZERO        PIC S9(01)V9(04) COMP-3
               VALUE 0.0000.
       01  WS-RETURN-CODES.
           05  CN-RET-OK           PIC S9(04) COMP VALUE 0.
           05  CN-RET-WARN         PIC S9(04) COMP VALUE 4.
           05  CN-RET-INPUT        PIC S9(04) COMP VALUE 12.
           05  CN-RET-FILE         PIC S9(04) COMP VALUE 16.
           05  CN-RET-CALL         PIC S9(04) COMP VALUE 20.

       COPY LK-RATE-PARM.

       LINKAGE SECTION.
       COPY LK-GRP-PARM.

       PROCEDURE DIVISION USING LK-GRP-PARM.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE-INPUT
           IF LK-RET-CD OF LK-GRP-PARM = CN-RET-OK
               PERFORM 3000-OPEN-FILES
           END-IF
           IF LK-RET-CD OF LK-GRP-PARM = CN-RET-OK
               PERFORM 4000-MAP-GROUP
               PERFORM 5000-READ-GROUP
           END-IF
           IF LK-RET-CD OF LK-GRP-PARM = CN-RET-OK
               PERFORM 6000-CHECK-ACCOUNT-GROUP
           END-IF
           IF LK-RET-CD OF LK-GRP-PARM = CN-RET-OK
               OR LK-RET-CD OF LK-GRP-PARM = CN-RET-WARN
               PERFORM 7000-DETERMINE-RATE
               PERFORM 8000-CALCULATE-FEE
           END-IF
           PERFORM 9000-CLOSE-FILES
           GOBACK.

       1000-INITIALIZE.
           MOVE ZEROES TO LK-FEE-AMT OF LK-GRP-PARM
           MOVE "N" TO LK-EXEMPT OF LK-GRP-PARM
           MOVE CN-RET-OK TO LK-RET-CD OF LK-GRP-PARM
           MOVE SPACES TO WS-EFFECTIVE-GROUP
           MOVE CN-RATE-ZERO TO WS-RATE
           MOVE "N" TO WS-ACCT-FOUND
           MOVE "N" TO WS-EOF
           MOVE "N" TO WS-FILES-OPEN.

       2000-VALIDATE-INPUT.
           IF LK-GRP-CODE OF LK-GRP-PARM = SPACES
               MOVE CN-RET-INPUT TO LK-RET-CD OF LK-GRP-PARM
           END-IF
           IF LK-RET-CD OF LK-GRP-PARM = CN-RET-OK
               IF LK-OVER-AMT OF LK-GRP-PARM < ZERO
                   MOVE CN-RET-INPUT TO LK-RET-CD OF LK-GRP-PARM
               END-IF
           END-IF.

       3000-OPEN-FILES.
           OPEN INPUT KZACCTF KZDGRPF
           IF WS-ACCT-STAT = "00" AND WS-DGRP-STAT = "00"
               MOVE "Y" TO WS-FILES-OPEN
           ELSE
               MOVE CN-RET-FILE TO LK-RET-CD OF LK-GRP-PARM
           END-IF.

       4000-MAP-GROUP.
           IF LK-GRP-CODE OF LK-GRP-PARM = CN-PREM
               MOVE CN-STD0 TO WS-EFFECTIVE-GROUP
           ELSE
               MOVE LK-GRP-CODE OF LK-GRP-PARM TO WS-EFFECTIVE-GROUP
           END-IF.

       5000-READ-GROUP.
           MOVE WS-EFFECTIVE-GROUP TO DG-GROUP-CODE
           READ KZDGRPF
               KEY IS DG-GROUP-CODE
               INVALID KEY
                   CONTINUE
           END-READ
           IF WS-DGRP-STAT NOT = "00" AND WS-DGRP-STAT NOT = "23"
               MOVE CN-RET-FILE TO LK-RET-CD OF LK-GRP-PARM
           END-IF.

       6000-CHECK-ACCOUNT-GROUP.
           MOVE LOW-VALUES TO AC-ACCT-ID
           START KZACCTF
               KEY IS GREATER THAN OR EQUAL TO AC-ACCT-ID
               INVALID KEY
                   MOVE "Y" TO WS-EOF
           END-START
           IF WS-ACCT-STAT NOT = "00" AND WS-ACCT-STAT NOT = "23"
               MOVE CN-RET-FILE TO LK-RET-CD OF LK-GRP-PARM
               MOVE "Y" TO WS-EOF
           END-IF
           PERFORM UNTIL END-OF-ACCT OR ACCT-FOUND
               READ KZACCTF NEXT RECORD
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF AC-GROUP-CODE = WS-EFFECTIVE-GROUP
                           MOVE "Y" TO WS-ACCT-FOUND
                       END-IF
               END-READ
               IF WS-ACCT-STAT NOT = "00" AND WS-ACCT-STAT NOT = "10"
                   MOVE CN-RET-FILE TO LK-RET-CD OF LK-GRP-PARM
                   MOVE "Y" TO WS-EOF
               END-IF
           END-PERFORM
           IF LK-RET-CD OF LK-GRP-PARM = CN-RET-OK
               IF NOT ACCT-FOUND
                   MOVE CN-RET-WARN TO LK-RET-CD OF LK-GRP-PARM
               END-IF
           END-IF.

       7000-DETERMINE-RATE.
           IF WS-DGRP-STAT = "00"
               MOVE DG-OL-FEE-RATE TO WS-RATE
               IF DG-EXEMPT-FLAG = "Y" OR WS-RATE = CN-RATE-ZERO
                   MOVE CN-RATE-ZERO TO WS-RATE
                   MOVE "Y" TO LK-EXEMPT OF LK-GRP-PARM
               END-IF
           ELSE
               EVALUATE WS-EFFECTIVE-GROUP
                   WHEN CN-GLD1
                       MOVE CN-RATE-GLD TO WS-RATE
                   WHEN CN-PLT2
                       MOVE CN-RATE-PLT TO WS-RATE
                   WHEN CN-EXMP
                       MOVE CN-RATE-ZERO TO WS-RATE
                       MOVE "Y" TO LK-EXEMPT OF LK-GRP-PARM
                   WHEN CN-STD0
                       MOVE CN-RATE-STD TO WS-RATE
                   WHEN OTHER
                       MOVE CN-RATE-STD TO WS-RATE
               END-EVALUATE
           END-IF
           IF WS-RATE = CN-RATE-ZERO
               MOVE "Y" TO LK-EXEMPT OF LK-GRP-PARM
           END-IF.

       8000-CALCULATE-FEE.
           IF LK-EXEMPT OF LK-GRP-PARM = "Y"
               MOVE ZEROES TO LK-FEE-AMT OF LK-GRP-PARM
           ELSE
               MOVE WS-EFFECTIVE-GROUP TO LK-GRP-CODE OF LK-RATE-PARM
               MOVE LK-OVER-AMT OF LK-GRP-PARM
                   TO LK-OVER-AMT OF LK-RATE-PARM
               MOVE ZEROES TO LK-FEE-AMT OF LK-RATE-PARM
               MOVE CN-RET-OK TO LK-RET-CD OF LK-RATE-PARM
               CALL "KZ125S" USING LK-RATE-PARM
               IF LK-RET-CD OF LK-RATE-PARM = CN-RET-OK
                   MOVE LK-FEE-AMT OF LK-RATE-PARM
                       TO LK-FEE-AMT OF LK-GRP-PARM
               ELSE
                   COMPUTE LK-FEE-AMT OF LK-GRP-PARM ROUNDED =
                       LK-OVER-AMT OF LK-GRP-PARM * WS-RATE
                   MOVE CN-RET-CALL TO LK-RET-CD OF LK-GRP-PARM
               END-IF
           END-IF.

       9000-CLOSE-FILES.
           IF FILES-OPEN
               CLOSE KZACCTF KZDGRPF
           END-IF.
