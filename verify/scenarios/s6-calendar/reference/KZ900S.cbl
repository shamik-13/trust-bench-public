       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ900S.
      ******************************************************************
      * 日付計算共通サブルーチン
      * KZCALF の休業日フラグを参照し、休業日は前営業日へ繰上げる。
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCALF
               ASSIGN       TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS RANDOM
               RECORD KEY   IS CA-CAL-DT
               FILE STATUS  IS WS-KZCALF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZCALF.
           COPY KZCALFC.

       WORKING-STORAGE SECTION.
       01  WS-KZCALF-ST             PIC X(02) VALUE SPACE.
       01  WS-CAL-OPEN-SW           PIC X(01) VALUE 'N'.
           88  WS-CAL-OPENED                  VALUE 'Y'.
           88  WS-CAL-CLOSED                  VALUE 'N'.

       01  WS-WORK.
           05  WS-NOMINAL-DT        PIC 9(08) VALUE ZERO.
           05  WS-WORK-DT           PIC 9(08) VALUE ZERO.
           05  WS-WORK-INT          PIC S9(09) COMP VALUE ZERO.
           05  WS-TRY-COUNT         PIC 9(04) VALUE ZERO.
           05  WS-MAX-TRY           PIC 9(04) VALUE 370.
           05  WS-DATE-OK-SW        PIC X(01) VALUE 'N'.
               88  WS-DATE-OK                 VALUE 'Y'.
               88  WS-DATE-NG                 VALUE 'N'.
           05  WS-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-FOUND                   VALUE 'Y'.
               88  WS-NOT-FOUND               VALUE 'N'.

       LINKAGE SECTION.
           COPY LK-CAL-PARM.

       PROCEDURE DIVISION USING LK-CAL-PARM.
       0000-MAIN.
           PERFORM 1000-INIT
           IF LK-CAL-RET = '00'
               PERFORM 2000-CALCULATE
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE '00' TO LK-CAL-RET
           MOVE ZERO TO RETURN-CODE
           MOVE LK-CAL-NOMINAL-DT TO WS-NOMINAL-DT
           MOVE ZERO TO LK-CAL-RESOLVED-DT
           MOVE 'N'  TO LK-CAL-ROLLED-FLAG
           SET WS-DATE-NG TO TRUE
           SET WS-NOT-FOUND TO TRUE

           PERFORM 1100-CHECK-DATE
           IF WS-DATE-NG
               MOVE '10' TO LK-CAL-RET
               DISPLAY 'INVALID DATE DT=' LK-CAL-NOMINAL-DT
           ELSE
               OPEN INPUT KZCALF
               IF WS-KZCALF-ST = '00'
                   SET WS-CAL-OPENED TO TRUE
               ELSE
                   MOVE '20' TO LK-CAL-RET
                   MOVE 8 TO RETURN-CODE
                   DISPLAY 'KZCALF OPEN ERROR ST=' WS-KZCALF-ST
               END-IF
           END-IF.

       1100-CHECK-DATE.
           IF WS-NOMINAL-DT < 16010101
              OR WS-NOMINAL-DT > 99991231
               SET WS-DATE-NG TO TRUE
           ELSE
               COMPUTE WS-WORK-INT =
                   FUNCTION INTEGER-OF-DATE(WS-NOMINAL-DT)
               COMPUTE WS-WORK-DT =
                   FUNCTION DATE-OF-INTEGER(WS-WORK-INT)
               IF WS-WORK-DT = WS-NOMINAL-DT
                   SET WS-DATE-OK TO TRUE
               ELSE
                   SET WS-DATE-NG TO TRUE
               END-IF
           END-IF.

       2000-CALCULATE.
           MOVE WS-NOMINAL-DT TO CA-CAL-DT
           READ KZCALF
               INVALID KEY
                   MOVE '31' TO LK-CAL-RET
                   DISPLAY 'KZCALF DATE NOT FOUND DT='
                           WS-NOMINAL-DT
               NOT INVALID KEY
                   EVALUATE CA-HOLIDAY-FLAG
                       WHEN 'N'
                           MOVE WS-NOMINAL-DT
                             TO LK-CAL-RESOLVED-DT
                           MOVE 'N' TO LK-CAL-ROLLED-FLAG
                           MOVE '00' TO LK-CAL-RET
                       WHEN 'Y'
                           PERFORM 2100-ROLL-BACK
                       WHEN OTHER
                           MOVE '32' TO LK-CAL-RET
                           DISPLAY 'KZCALF BAD HOLIDAY FLAG DT='
                                   CA-CAL-DT
                           DISPLAY 'KZCALF BAD HOLIDAY FLAG='
                                   CA-HOLIDAY-FLAG
                   END-EVALUATE
           END-READ.

       2100-ROLL-BACK.
           COMPUTE WS-WORK-INT =
               FUNCTION INTEGER-OF-DATE(WS-NOMINAL-DT)
           MOVE ZERO TO WS-TRY-COUNT
           SET WS-NOT-FOUND TO TRUE

           PERFORM UNTIL WS-FOUND
              OR WS-TRY-COUNT >= WS-MAX-TRY
               ADD 1 TO WS-TRY-COUNT
               SUBTRACT 1 FROM WS-WORK-INT
               COMPUTE WS-WORK-DT =
                   FUNCTION DATE-OF-INTEGER(WS-WORK-INT)
               MOVE WS-WORK-DT TO CA-CAL-DT
               READ KZCALF
                   INVALID KEY
                       CONTINUE
                   NOT INVALID KEY
                       EVALUATE CA-HOLIDAY-FLAG
                           WHEN 'N'
                               MOVE WS-WORK-DT
                                 TO LK-CAL-RESOLVED-DT
                               MOVE 'Y' TO LK-CAL-ROLLED-FLAG
                               MOVE '00' TO LK-CAL-RET
                               SET WS-FOUND TO TRUE
                           WHEN 'Y'
                               CONTINUE
                           WHEN OTHER
                               MOVE '32' TO LK-CAL-RET
                               DISPLAY 'KZCALF BAD HOLIDAY FLAG DT='
                                       CA-CAL-DT
                               DISPLAY 'KZCALF BAD HOLIDAY FLAG='
                                       CA-HOLIDAY-FLAG
                               SET WS-FOUND TO TRUE
                       END-EVALUATE
               END-READ
           END-PERFORM

           IF WS-NOT-FOUND
               MOVE '33' TO LK-CAL-RET
               DISPLAY 'PREV BUSINESS DATE NOT FOUND DT='
                       WS-NOMINAL-DT
               DISPLAY 'SEARCH DAYS=' WS-MAX-TRY
           END-IF.

       9000-FINAL.
           IF WS-CAL-OPENED
               CLOSE KZCALF
               SET WS-CAL-CLOSED TO TRUE
               IF WS-KZCALF-ST NOT = '00'
                   MOVE '90' TO LK-CAL-RET
                   MOVE 8 TO RETURN-CODE
                   DISPLAY 'KZCALF CLOSE ERROR ST=' WS-KZCALF-ST
               END-IF
           END-IF

           IF LK-CAL-RET = '00'
               MOVE 0 TO RETURN-CODE
           END-IF.
