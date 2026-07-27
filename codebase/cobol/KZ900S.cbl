       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ900S.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当     概要
      * 1.00  20260510  共通基盤  初版作成
      * 1.01  20260524  共通基盤  KZCALF参照による休業日判定を追加
      * 1.02  20260602  共通基盤  繰下げ時の範囲超過検知を追加
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCALF
               ASSIGN TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CAL-DT
               FILE STATUS IS WS-KZCALF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZCALF.
           COPY KZCALFC.

       WORKING-STORAGE SECTION.
       01  WS-KZCALF-ST              PIC X(02) VALUE SPACE.
       01  WS-OPEN-FLAG              PIC X(01) VALUE 'N'.
           88  WS-OPENED                       VALUE 'Y'.

       01  WS-WORK.
           05  WS-NOMINAL-X          PIC X(08) VALUE SPACE.
           05  WS-NOMINAL-N          PIC 9(08) VALUE ZERO.
           05  WS-WORK-DT-N          PIC 9(08) VALUE ZERO.
           05  WS-WORK-DT-X          PIC X(08) VALUE SPACE.
           05  WS-DATE-INT           PIC S9(09) COMP VALUE ZERO.
           05  WS-STEP-CNT           PIC 9(03) VALUE ZERO.
           05  WS-MAX-STEP           PIC 9(03) VALUE 120.
           05  WS-DATE-OK-FLAG       PIC X(01) VALUE 'N'.
           05  WS-FOUND-FLAG         PIC X(01) VALUE 'N'.
           05  WS-YYYY               PIC 9(04) VALUE ZERO.
           05  WS-MM                 PIC 9(02) VALUE ZERO.
           05  WS-DD                 PIC 9(02) VALUE ZERO.
           05  WS-LAST-DD            PIC 9(02) VALUE ZERO.
           05  WS-REM4               PIC 9(04) VALUE ZERO.
           05  WS-REM100             PIC 9(04) VALUE ZERO.
           05  WS-REM400             PIC 9(04) VALUE ZERO.
           05  WS-LEAP-FLAG          PIC X(01) VALUE 'N'.

       LINKAGE SECTION.
           COPY LK-CAL-PARM.

       PROCEDURE DIVISION USING LK-CAL-PARM.
       0000-MAIN SECTION.
           PERFORM 1000-INIT
           IF LK-CAL-RET = '00'
               PERFORM 2000-CHECK-NOMINAL
           END-IF

           IF WS-OPENED
               PERFORM 8000-CLOSE
           END-IF

           IF LK-CAL-RET = '00'
               MOVE 0 TO RETURN-CODE
           ELSE
               MOVE 8 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-INIT SECTION.
           MOVE '00' TO LK-CAL-RET
           MOVE ZERO TO LK-CAL-RESOLVED-DT
           MOVE 'N' TO LK-CAL-ROLLED-FLAG
           MOVE 'N' TO WS-OPEN-FLAG
           MOVE 'N' TO WS-FOUND-FLAG

           MOVE LK-CAL-NOMINAL-DT TO WS-NOMINAL-X
           IF WS-NOMINAL-X IS NOT NUMERIC
               DISPLAY 'KZ900S INVALID DATE FORMAT DT='
                   WS-NOMINAL-X
               MOVE '10' TO LK-CAL-RET
           ELSE
               MOVE WS-NOMINAL-X TO WS-NOMINAL-N
               PERFORM 1100-VALIDATE-DATE
               IF WS-DATE-OK-FLAG NOT = 'Y'
                   DISPLAY 'KZ900S INVALID DATE VALUE DT='
                       WS-NOMINAL-X
                   MOVE '10' TO LK-CAL-RET
               END-IF
           END-IF

           IF LK-CAL-RET = '00'
               OPEN INPUT KZCALF
               IF WS-KZCALF-ST = '00'
                   MOVE 'Y' TO WS-OPEN-FLAG
               ELSE
                   DISPLAY 'KZCALF OPEN FAILED ST=' WS-KZCALF-ST
                   MOVE '20' TO LK-CAL-RET
               END-IF
           END-IF.

       1100-VALIDATE-DATE SECTION.
           MOVE 'N' TO WS-DATE-OK-FLAG
           MOVE WS-NOMINAL-X(1:4) TO WS-YYYY
           MOVE WS-NOMINAL-X(5:2) TO WS-MM
           MOVE WS-NOMINAL-X(7:2) TO WS-DD

           IF WS-YYYY < 1900 OR WS-YYYY > 2099
               EXIT SECTION
           END-IF
           IF WS-MM < 1 OR WS-MM > 12
               EXIT SECTION
           END-IF

           EVALUATE WS-MM
               WHEN 1
               WHEN 3
               WHEN 5
               WHEN 7
               WHEN 8
               WHEN 10
               WHEN 12
                   MOVE 31 TO WS-LAST-DD
               WHEN 4
               WHEN 6
               WHEN 9
               WHEN 11
                   MOVE 30 TO WS-LAST-DD
               WHEN 2
                   PERFORM 1110-CHECK-LEAP
                   IF WS-LEAP-FLAG = 'Y'
                       MOVE 29 TO WS-LAST-DD
                   ELSE
                       MOVE 28 TO WS-LAST-DD
                   END-IF
           END-EVALUATE

           IF WS-DD >= 1 AND WS-DD <= WS-LAST-DD
               MOVE 'Y' TO WS-DATE-OK-FLAG
           END-IF.

       1110-CHECK-LEAP SECTION.
           MOVE 'N' TO WS-LEAP-FLAG
           COMPUTE WS-REM4 = FUNCTION MOD(WS-YYYY 4)
           COMPUTE WS-REM100 = FUNCTION MOD(WS-YYYY 100)
           COMPUTE WS-REM400 = FUNCTION MOD(WS-YYYY 400)
           IF WS-REM4 = 0
              AND (WS-REM100 NOT = 0 OR WS-REM400 = 0)
               MOVE 'Y' TO WS-LEAP-FLAG
           END-IF.

       2000-CHECK-NOMINAL SECTION.
           MOVE WS-NOMINAL-X TO CA-CAL-DT
           READ KZCALF KEY IS CA-CAL-DT
               INVALID KEY
                   DISPLAY 'KZCALF DATE NOT FOUND DT='
                       WS-NOMINAL-X
                       ' ST='
                       WS-KZCALF-ST
                   MOVE '30' TO LK-CAL-RET
               NOT INVALID KEY
                   EVALUATE CA-HOLIDAY-FLAG
                       WHEN 'N'
                           MOVE WS-NOMINAL-X TO LK-CAL-RESOLVED-DT
                           MOVE 'N' TO LK-CAL-ROLLED-FLAG
                       WHEN 'Y'
                           PERFORM 3000-ROLL-FORWARD
                       WHEN OTHER
                           DISPLAY 'KZCALF INVALID HOLIDAY FLAG DT='
                               CA-CAL-DT
                               ' FLG='
                               CA-HOLIDAY-FLAG
                           MOVE '40' TO LK-CAL-RET
                   END-EVALUATE
           END-READ.

       3000-ROLL-FORWARD SECTION.
           MOVE FUNCTION INTEGER-OF-DATE(WS-NOMINAL-N) TO WS-DATE-INT
           MOVE ZERO TO WS-STEP-CNT
           MOVE 'N' TO WS-FOUND-FLAG

           PERFORM UNTIL WS-FOUND-FLAG = 'Y'
              OR WS-STEP-CNT >= WS-MAX-STEP
               ADD 1 TO WS-STEP-CNT
               ADD 1 TO WS-DATE-INT
               MOVE FUNCTION DATE-OF-INTEGER(WS-DATE-INT)
                   TO WS-WORK-DT-N
               MOVE WS-WORK-DT-N TO WS-WORK-DT-X
               MOVE WS-WORK-DT-X TO CA-CAL-DT

               READ KZCALF KEY IS CA-CAL-DT
                   INVALID KEY
                       CONTINUE
                   NOT INVALID KEY
                       EVALUATE CA-HOLIDAY-FLAG
                           WHEN 'N'
                               MOVE 'Y' TO WS-FOUND-FLAG
                               MOVE CA-CAL-DT TO LK-CAL-RESOLVED-DT
                               MOVE 'Y' TO LK-CAL-ROLLED-FLAG
                           WHEN 'Y'
                               CONTINUE
                           WHEN OTHER
                               DISPLAY 'KZCALF INVALID HOLIDAY FLAG DT='
                                   CA-CAL-DT
                                   ' FLG='
                                   CA-HOLIDAY-FLAG
                               MOVE '40' TO LK-CAL-RET
                               MOVE 'Y' TO WS-FOUND-FLAG
                       END-EVALUATE
               END-READ
           END-PERFORM

           IF LK-CAL-RET = '00'
              AND WS-FOUND-FLAG NOT = 'Y'
               DISPLAY 'KZ900S BUSINESS DATE NOT FOUND DT='
                   WS-NOMINAL-X
                   ' RANGE='
                   WS-MAX-STEP
               MOVE '50' TO LK-CAL-RET
           END-IF.

       8000-CLOSE SECTION.
           CLOSE KZCALF
           IF WS-KZCALF-ST NOT = '00'
               DISPLAY 'KZCALF CLOSE FAILED ST=' WS-KZCALF-ST
               IF LK-CAL-RET = '00'
                   MOVE '60' TO LK-CAL-RET
               END-IF
           END-IF.
