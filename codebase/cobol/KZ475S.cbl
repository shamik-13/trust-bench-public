       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ475S.
      *-------------------------------------------------------------
      * 変更履歴
      * 版数  年月日(和暦)  担当                    概要
      * 1.00  H18.04.01    システム部 勘定系チーム 利息監査合計初版作成
      * 1.01  H24.10.15    システム部 勘定系チーム 監査合計抽出条件見直し
      * 1.02  R03.06.30    システム部 勘定系チーム 利息集計端数処理修正
      *-------------------------------------------------------------
       AUTHOR. KZ-BATCH.
      ******************************************************************
      * INTEREST AUDIT TOTAL SUBPROGRAM.
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZRCNF
               ASSIGN TO "KZRCNF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZRCNF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZRCNF
           RECORD CONTAINS 120 CHARACTERS
           DATA RECORD IS KZRCNF-REC.
           COPY KZRCNFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-KZRCNF-ST          PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05  WS-EOF-SW             PIC X VALUE "N".
               88  WS-EOF                 VALUE "Y".
               88  WS-NOT-EOF             VALUE "N".
           05  WS-HARD-ERR-SW        PIC X VALUE "N".
               88  WS-HARD-ERR            VALUE "Y".
               88  WS-NO-HARD-ERR         VALUE "N".

       01  WS-WORK-AREA.
           05  WS-WORK-KEY           PIC X(40) VALUE SPACE.
           05  WS-DISP-CD            PIC X(02) VALUE SPACE.
           05  WS-ABS-DIFF-AMT       PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-CALC-DIFF-AMT      PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-OLD-GL-AMT         PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-OLD-ACCR-AMT       PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-OLD-TAX-AMT        PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-OLD-DIFF-AMT       PIC S9(13)V99 COMP-3 VALUE 0.

       01  WS-CONSTANTS.
           05  WS-CD-ADD             PIC X(02) VALUE "00".
           05  WS-CD-DRCR            PIC X(02) VALUE "11".
           05  WS-CD-TAX             PIC X(02) VALUE "21".
           05  WS-CD-CARRY           PIC X(02) VALUE "31".
           05  WS-CD-INPUT           PIC X(02) VALUE "90".

       LINKAGE SECTION.
       01  LK-KZ475S-PARM.
           05  LK-CALLER-PGM         PIC X(08).
           05  LK-CYCLE-ID           PIC X(08).
           05  LK-AMT-KIND           PIC X(01).
               88  LK-KIND-ADD            VALUE "A".
               88  LK-KIND-DRCR-DIFF      VALUE "B".
               88  LK-KIND-TAX-DIFF       VALUE "T".
               88  LK-KIND-CARRY-DIFF     VALUE "C".
           05  LK-AUDIT-KEY          PIC X(24).
           05  LK-GL-AMT             PIC S9(13)V99 COMP-3.
           05  LK-ACCRUAL-AMT        PIC S9(13)V99 COMP-3.
           05  LK-TAX-AMT            PIC S9(13)V99 COMP-3.
           05  LK-BASE-ACCRUAL-AMT   PIC S9(13)V99 COMP-3.
           05  LK-BASE-TAX-AMT       PIC S9(13)V99 COMP-3.
           05  LK-RETURN-STATUS      PIC X(02).

       PROCEDURE DIVISION USING LK-KZ475S-PARM.
       0000-MAIN SECTION.
       0000-START.
           MOVE 8 TO RETURN-CODE
           MOVE "00" TO LK-RETURN-STATUS
           SET WS-NO-HARD-ERR TO TRUE

           PERFORM 1000-VALIDATE
           IF WS-NO-HARD-ERR
               PERFORM 2000-BUILD-KEY
               PERFORM 3000-READ-CURRENT
           END-IF
           IF WS-NO-HARD-ERR
               PERFORM 4000-BUILD-OUTPUT
           END-IF
           IF WS-NO-HARD-ERR
               PERFORM 5000-WRITE-OUTPUT
           END-IF

           IF WS-NO-HARD-ERR
               MOVE 0 TO RETURN-CODE
               MOVE "00" TO LK-RETURN-STATUS
               DISPLAY "KZ475S OK KEY=" WS-WORK-KEY
           ELSE
               IF LK-RETURN-STATUS = "00"
                   MOVE "99" TO LK-RETURN-STATUS
               END-IF
           END-IF

           GOBACK
           .

       1000-VALIDATE SECTION.
       1000-START.
           IF LK-CALLER-PGM = SPACE
               DISPLAY "KZ475S CALLER REQUIRED"
               MOVE "91" TO LK-RETURN-STATUS
               SET WS-HARD-ERR TO TRUE
           END-IF

           IF LK-CYCLE-ID = SPACE
               DISPLAY "KZ475S CYCLE REQUIRED"
               MOVE "92" TO LK-RETURN-STATUS
               SET WS-HARD-ERR TO TRUE
           END-IF

           IF LK-AUDIT-KEY = SPACE
               DISPLAY "KZ475S AUDIT KEY REQUIRED"
               MOVE "93" TO LK-RETURN-STATUS
               SET WS-HARD-ERR TO TRUE
           END-IF

           IF NOT LK-KIND-ADD
              AND NOT LK-KIND-DRCR-DIFF
              AND NOT LK-KIND-TAX-DIFF
              AND NOT LK-KIND-CARRY-DIFF
               DISPLAY "KZ475S BAD KIND=" LK-AMT-KIND
               MOVE "94" TO LK-RETURN-STATUS
               SET WS-HARD-ERR TO TRUE
           END-IF

           IF LK-GL-AMT < 0
              OR LK-ACCRUAL-AMT < 0
              OR LK-TAX-AMT < 0
               DISPLAY "KZ475S BAD AMOUNT KEY=" LK-AUDIT-KEY
               MOVE "95" TO LK-RETURN-STATUS
               SET WS-HARD-ERR TO TRUE
           END-IF
           .

       2000-BUILD-KEY SECTION.
       2000-START.
           MOVE SPACE TO WS-WORK-KEY
           STRING LK-CALLER-PGM DELIMITED BY SIZE
                  LK-CYCLE-ID   DELIMITED BY SIZE
                  LK-AUDIT-KEY  DELIMITED BY SIZE
             INTO WS-WORK-KEY
           END-STRING
           .

       3000-READ-CURRENT SECTION.
       3000-START.
           SET WS-NOT-EOF TO TRUE
           MOVE 0 TO WS-OLD-GL-AMT
                     WS-OLD-ACCR-AMT
                     WS-OLD-TAX-AMT
                     WS-OLD-DIFF-AMT

           OPEN INPUT KZRCNF
           IF WS-KZRCNF-ST = "35"
               GO TO 3000-EXIT
           END-IF

           IF WS-KZRCNF-ST NOT = "00"
               DISPLAY "KZRCNF OPEN INPUT ST=" WS-KZRCNF-ST
               MOVE "81" TO LK-RETURN-STATUS
               SET WS-HARD-ERR TO TRUE
               GO TO 3000-CLOSE
           END-IF

           PERFORM UNTIL WS-EOF OR WS-HARD-ERR
               READ KZRCNF
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       IF WS-KZRCNF-ST NOT = "00"
                           DISPLAY "KZRCNF READ ST=" WS-KZRCNF-ST
                           MOVE "82" TO LK-RETURN-STATUS
                           SET WS-HARD-ERR TO TRUE
                       ELSE
                           PERFORM 3100-ACCUM-CURRENT
                       END-IF
               END-READ
           END-PERFORM

           .
       3000-CLOSE.
           CLOSE KZRCNF
           IF WS-KZRCNF-ST NOT = "00"
               DISPLAY "KZRCNF CLOSE INPUT ST=" WS-KZRCNF-ST
               MOVE "83" TO LK-RETURN-STATUS
               SET WS-HARD-ERR TO TRUE
           END-IF
           .
       3000-EXIT.
           EXIT.

       3100-ACCUM-CURRENT SECTION.
       3100-START.
           IF RC-RECON-KEY = WS-WORK-KEY
              AND RC-CYCLE-ID = LK-CYCLE-ID
               ADD RC-GL-TOTAL-AMT TO WS-OLD-GL-AMT
               ADD RC-ACCRUAL-TOTAL-AMT TO WS-OLD-ACCR-AMT
               ADD RC-TAX-TOTAL-AMT TO WS-OLD-TAX-AMT
               ADD RC-DIFF-AMT TO WS-OLD-DIFF-AMT
           END-IF
           .

       4000-BUILD-OUTPUT SECTION.
       4000-START.
           MOVE LOW-VALUE TO KZRCNF-REC
           MOVE WS-WORK-KEY TO RC-RECON-KEY
           MOVE LK-CYCLE-ID TO RC-CYCLE-ID
           MOVE 0 TO RC-GL-TOTAL-AMT
                     RC-ACCRUAL-TOTAL-AMT
                     RC-TAX-TOTAL-AMT
                     RC-DIFF-AMT

           EVALUATE TRUE
               WHEN LK-KIND-ADD
                   MOVE WS-CD-ADD TO WS-DISP-CD
                   MOVE LK-GL-AMT TO RC-GL-TOTAL-AMT
                   MOVE LK-ACCRUAL-AMT TO RC-ACCRUAL-TOTAL-AMT
                   MOVE LK-TAX-AMT TO RC-TAX-TOTAL-AMT
                   COMPUTE WS-CALC-DIFF-AMT =
                       LK-GL-AMT - LK-ACCRUAL-AMT - LK-TAX-AMT
               WHEN LK-KIND-DRCR-DIFF
                   MOVE WS-CD-DRCR TO WS-DISP-CD
                   COMPUTE WS-CALC-DIFF-AMT =
                       LK-GL-AMT - LK-ACCRUAL-AMT - LK-TAX-AMT
                   SUBTRACT WS-OLD-DIFF-AMT
                       FROM WS-CALC-DIFF-AMT
               WHEN LK-KIND-TAX-DIFF
                   MOVE WS-CD-TAX TO WS-DISP-CD
                   COMPUTE WS-CALC-DIFF-AMT =
                       LK-TAX-AMT - LK-BASE-TAX-AMT
               WHEN LK-KIND-CARRY-DIFF
                   MOVE WS-CD-CARRY TO WS-DISP-CD
                   COMPUTE WS-CALC-DIFF-AMT =
                       LK-ACCRUAL-AMT - LK-BASE-ACCRUAL-AMT
               WHEN OTHER
                   MOVE WS-CD-INPUT TO WS-DISP-CD
                   MOVE 0 TO WS-CALC-DIFF-AMT
           END-EVALUATE

           IF LK-KIND-ADD
               IF WS-CALC-DIFF-AMT NOT = 0
                   DISPLAY "KZ475S ADD DIFF KEY="
                       WS-WORK-KEY
               END-IF
           ELSE
               IF WS-CALC-DIFF-AMT = 0
                   DISPLAY "KZ475S ZERO DIFF KEY="
                       WS-WORK-KEY
                   MOVE "10" TO LK-RETURN-STATUS
                   SET WS-HARD-ERR TO TRUE
               END-IF
           END-IF

           IF WS-CALC-DIFF-AMT < 0
               COMPUTE WS-ABS-DIFF-AMT = WS-CALC-DIFF-AMT * -1
           ELSE
               MOVE WS-CALC-DIFF-AMT TO WS-ABS-DIFF-AMT
           END-IF

           IF WS-ABS-DIFF-AMT > 9999999999999.99
               DISPLAY "KZ475S DIFF OVERFLOW KEY=" WS-WORK-KEY
               MOVE "96" TO LK-RETURN-STATUS
               SET WS-HARD-ERR TO TRUE
           END-IF

           MOVE WS-CALC-DIFF-AMT TO RC-DIFF-AMT
           MOVE WS-DISP-CD TO RC-DISPOSITION-CD
           .

       5000-WRITE-OUTPUT SECTION.
       5000-START.
           IF WS-HARD-ERR
               GO TO 5000-EXIT
           END-IF

           OPEN EXTEND KZRCNF
           IF WS-KZRCNF-ST NOT = "00"
               DISPLAY "KZRCNF OPEN EXTEND ST=" WS-KZRCNF-ST
               MOVE "84" TO LK-RETURN-STATUS
               SET WS-HARD-ERR TO TRUE
               GO TO 5000-EXIT
           END-IF

           WRITE KZRCNF-REC
           IF WS-KZRCNF-ST NOT = "00"
               DISPLAY "KZRCNF WRITE ST=" WS-KZRCNF-ST
               MOVE "85" TO LK-RETURN-STATUS
               SET WS-HARD-ERR TO TRUE
           END-IF

           CLOSE KZRCNF
           IF WS-KZRCNF-ST NOT = "00"
               DISPLAY "KZRCNF CLOSE EXTEND ST=" WS-KZRCNF-ST
               MOVE "86" TO LK-RETURN-STATUS
               SET WS-HARD-ERR TO TRUE
           END-IF
           .
       5000-EXIT.
           EXIT.
