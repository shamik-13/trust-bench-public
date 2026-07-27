       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ120S.
      *
      * 変更履歴
      * 版数  年月日        担当                         概要
      * 1.00  平成30年04月  システム部 勘定系チーム      新規作成
      * 1.01  令和02年10月  システム部 勘定系チーム      料率判定見直し
      * 1.02  令和05年06月  システム部 勘定系チーム      例外処理整備
      *

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT KZDGRPF ASSIGN TO "KZDGRPF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DG-GROUP-CODE
               FILE STATUS IS WS-DGRP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
       COPY KZACCTC.
       FD  KZDGRPF.
       COPY KZDGRPC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-ACCT-STATUS        PIC XX VALUE SPACES.
           05  WS-DGRP-STATUS        PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05  WS-ACCT-EOF           PIC X VALUE "N".
               88  ACCT-EOF                VALUE "Y".
               88  ACCT-NOT-EOF            VALUE "N".
           05  WS-ACCT-GRP-FOUND     PIC X VALUE "N".
               88  ACCT-GRP-FOUND          VALUE "Y".
               88  ACCT-GRP-NOT-FOUND      VALUE "N".
           05  WS-DGRP-FOUND         PIC X VALUE "N".
               88  DGRP-FOUND              VALUE "Y".
               88  DGRP-NOT-FOUND          VALUE "N".
           05  WS-ACCT-OPEN          PIC X VALUE "N".
               88  ACCT-OPEN               VALUE "Y".
           05  WS-DGRP-OPEN          PIC X VALUE "N".
               88  DGRP-OPEN               VALUE "Y".

       01  WS-RATE-WORK.
           05  WS-EFFECTIVE-GROUP    PIC X(04) VALUE SPACES.
           05  WS-WORK-RATE          PIC S9(01)V9(04) COMP-3
                                      VALUE 0.
           05  WS-STANDARD-RATE      PIC S9(01)V9(04) COMP-3
                                      VALUE 0.0150.
           05  WS-PREM-RATE          PIC S9(01)V9(04) COMP-3
                                      VALUE 0.0080.
           05  WS-ZERO-RATE          PIC S9(01)V9(04) COMP-3
                                      VALUE 0.

       01  WS-RETURN-CODES.
           05  WS-RET-OK             PIC S9(04) COMP VALUE 0.
           05  WS-RET-WARN           PIC S9(04) COMP VALUE 4.
           05  WS-RET-NOTFOUND       PIC S9(04) COMP VALUE 8.
           05  WS-RET-FILEERR        PIC S9(04) COMP VALUE 12.
           05  WS-RET-PARMERR        PIC S9(04) COMP VALUE 16.

       COPY LK-RATE-PARM.

       LINKAGE SECTION.
       COPY LK-GRP-PARM.

       PROCEDURE DIVISION USING LK-GRP-PARM.

       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE-PARAMETER
           IF LK-RET-CD OF LK-GRP-PARM = WS-RET-OK
               PERFORM 3000-OPEN-FILES
           END-IF
           IF LK-RET-CD OF LK-GRP-PARM = WS-RET-OK
               PERFORM 4000-READ-GROUP-MASTER
           END-IF
           IF LK-RET-CD OF LK-GRP-PARM = WS-RET-OK
               PERFORM 5000-CHECK-ACCOUNT-USAGE
           END-IF
           IF LK-RET-CD OF LK-GRP-PARM = WS-RET-OK
               PERFORM 6000-DETERMINE-RATE
           END-IF
           IF LK-RET-CD OF LK-GRP-PARM = WS-RET-OK
               PERFORM 7000-DELEGATE-FEE-VALUE
           END-IF
           PERFORM 9000-CLOSE-FILES
           GOBACK.

       1000-INITIALIZE.
           MOVE ZEROES TO LK-FEE-AMT OF LK-GRP-PARM
           MOVE "N"    TO LK-EXEMPT  OF LK-GRP-PARM
           MOVE WS-RET-OK TO LK-RET-CD OF LK-GRP-PARM
           MOVE "N" TO WS-ACCT-EOF
           MOVE "N" TO WS-ACCT-GRP-FOUND
           MOVE "N" TO WS-DGRP-FOUND
           MOVE "N" TO WS-ACCT-OPEN
           MOVE "N" TO WS-DGRP-OPEN
           MOVE SPACES TO WS-ACCT-STATUS
           MOVE SPACES TO WS-DGRP-STATUS
           MOVE SPACES TO WS-EFFECTIVE-GROUP
           MOVE ZEROES TO WS-WORK-RATE.

       2000-VALIDATE-PARAMETER.
           IF LK-GRP-CODE OF LK-GRP-PARM = SPACES
               MOVE WS-RET-PARMERR TO LK-RET-CD OF LK-GRP-PARM
           END-IF
           IF LK-RET-CD OF LK-GRP-PARM = WS-RET-OK
               IF LK-OVER-AMT OF LK-GRP-PARM < ZERO
                   MOVE WS-RET-PARMERR TO LK-RET-CD OF LK-GRP-PARM
               END-IF
           END-IF
           IF LK-RET-CD OF LK-GRP-PARM = WS-RET-OK
               EVALUATE LK-GRP-CODE OF LK-GRP-PARM
                   WHEN "STD0"
                   WHEN "GLD1"
                   WHEN "PLT2"
                   WHEN "EXMP"
                   WHEN "PREM"
                       CONTINUE
                   WHEN OTHER
                       MOVE WS-RET-NOTFOUND TO LK-RET-CD OF LK-GRP-PARM
               END-EVALUATE
           END-IF.

       3000-OPEN-FILES.
           OPEN INPUT KZACCTF
           IF WS-ACCT-STATUS = "00"
               MOVE "Y" TO WS-ACCT-OPEN
           ELSE
               MOVE WS-RET-FILEERR TO LK-RET-CD OF LK-GRP-PARM
           END-IF

           IF LK-RET-CD OF LK-GRP-PARM = WS-RET-OK
               OPEN INPUT KZDGRPF
               IF WS-DGRP-STATUS = "00"
                   MOVE "Y" TO WS-DGRP-OPEN
               ELSE
                   MOVE WS-RET-FILEERR TO LK-RET-CD OF LK-GRP-PARM
               END-IF
           END-IF.

       4000-READ-GROUP-MASTER.
           MOVE LK-GRP-CODE OF LK-GRP-PARM TO DG-GROUP-CODE
           READ KZDGRPF KEY IS DG-GROUP-CODE
               INVALID KEY
                   MOVE "N" TO WS-DGRP-FOUND
               NOT INVALID KEY
                   MOVE "Y" TO WS-DGRP-FOUND
           END-READ

           IF DGRP-NOT-FOUND
               MOVE WS-RET-NOTFOUND TO LK-RET-CD OF LK-GRP-PARM
           ELSE
               IF WS-DGRP-STATUS NOT = "00"
                   MOVE WS-RET-FILEERR TO LK-RET-CD OF LK-GRP-PARM
               END-IF
           END-IF.

       5000-CHECK-ACCOUNT-USAGE.
           PERFORM UNTIL ACCT-EOF OR ACCT-GRP-FOUND
               READ KZACCTF NEXT RECORD
                   AT END
                       MOVE "Y" TO WS-ACCT-EOF
                   NOT AT END
                       IF AC-GROUP-CODE =
                          LK-GRP-CODE OF LK-GRP-PARM
                           MOVE "Y" TO WS-ACCT-GRP-FOUND
                       END-IF
               END-READ
           END-PERFORM

           IF WS-ACCT-STATUS NOT = "00" AND
              WS-ACCT-STATUS NOT = "10"
               MOVE WS-RET-FILEERR TO LK-RET-CD OF LK-GRP-PARM
           END-IF.

       6000-DETERMINE-RATE.
           MOVE LK-GRP-CODE OF LK-GRP-PARM TO WS-EFFECTIVE-GROUP

           EVALUATE WS-EFFECTIVE-GROUP
               WHEN "STD0"
                   MOVE WS-STANDARD-RATE TO WS-WORK-RATE
               WHEN "GLD1"
                   MOVE 0.0100 TO WS-WORK-RATE
               WHEN "PLT2"
                   MOVE 0.0080 TO WS-WORK-RATE
               WHEN "EXMP"
                   MOVE WS-ZERO-RATE TO WS-WORK-RATE
               WHEN "PREM"
                   MOVE WS-PREM-RATE TO WS-WORK-RATE
               WHEN OTHER
                   MOVE WS-STANDARD-RATE TO WS-WORK-RATE
           END-EVALUATE

           IF DGRP-FOUND
               IF DG-EXEMPT-FLAG = "Y"
                   MOVE WS-ZERO-RATE TO WS-WORK-RATE
               ELSE
                   IF DG-OL-FEE-RATE > ZERO
                       MOVE DG-OL-FEE-RATE TO WS-WORK-RATE
                   END-IF
               END-IF
           END-IF

           IF WS-WORK-RATE = ZERO
               MOVE "Y" TO LK-EXEMPT OF LK-GRP-PARM
               MOVE ZEROES TO LK-FEE-AMT OF LK-GRP-PARM
           ELSE
               MOVE "N" TO LK-EXEMPT OF LK-GRP-PARM
           END-IF.

       7000-DELEGATE-FEE-VALUE.
           IF LK-EXEMPT OF LK-GRP-PARM = "Y"
               MOVE ZEROES TO LK-FEE-AMT OF LK-GRP-PARM
           ELSE
               MOVE WS-EFFECTIVE-GROUP TO LK-GRP-CODE OF LK-RATE-PARM
               MOVE LK-OVER-AMT OF LK-GRP-PARM
                   TO LK-OVER-AMT OF LK-RATE-PARM
               MOVE ZEROES TO LK-FEE-AMT OF LK-RATE-PARM
               MOVE WS-RET-OK TO LK-RET-CD OF LK-RATE-PARM

               CALL "KZ125S" USING LK-RATE-PARM

               IF LK-RET-CD OF LK-RATE-PARM = WS-RET-OK
                   MOVE LK-FEE-AMT OF LK-RATE-PARM
                       TO LK-FEE-AMT OF LK-GRP-PARM
               ELSE
                   COMPUTE LK-FEE-AMT OF LK-GRP-PARM ROUNDED =
                       LK-OVER-AMT OF LK-GRP-PARM * WS-WORK-RATE
                   MOVE LK-RET-CD OF LK-RATE-PARM
                       TO LK-RET-CD OF LK-GRP-PARM
               END-IF
           END-IF.

       9000-CLOSE-FILES.
           IF ACCT-OPEN
               CLOSE KZACCTF
           END-IF
           IF DGRP-OPEN
               CLOSE KZDGRPF
           END-IF.
