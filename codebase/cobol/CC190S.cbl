       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC190S.
       AUTHOR. MFG-KYOTSU-CAL.
      ******************************************************************
      * 営業日判定サブルーチン（MFG共通基盤 カレンダーチーム）
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCCALF ASSIGN TO "CCCALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CCCALF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CCCALF.
           COPY CCCALFC.

       WORKING-STORAGE SECTION.
       01  WS-CCCALF-ST              PIC X(02) VALUE SPACE.
       01  WS-EOF-FLG                PIC X(01) VALUE 'N'.
           88  WS-EOF                          VALUE 'Y'.
       01  WS-FOUND-FLG              PIC X(01) VALUE 'N'.
           88  WS-FOUND                        VALUE 'Y'.
       01  WS-NEXT-FOUND-FLG         PIC X(01) VALUE 'N'.
           88  WS-NEXT-FOUND                   VALUE 'Y'.
       01  WS-HARD-ERR-FLG           PIC X(01) VALUE 'N'.
           88  WS-HARD-ERR                     VALUE 'Y'.

       01  WS-REQ-DATE               PIC 9(08) VALUE ZERO.
       01  WS-CALC-NEXT-DATE         PIC 9(08) VALUE ZERO.
       01  WS-CAL-DATE               PIC 9(08) VALUE ZERO.

       01  WS-DATE-WK.
           05  WS-DATE-YYYY          PIC 9(04).
           05  WS-DATE-MM            PIC 9(02).
           05  WS-DATE-DD            PIC 9(02).
       01  WS-DATE-NUM REDEFINES WS-DATE-WK
                                      PIC 9(08).

       01  WS-MAX-DD                 PIC 9(02) VALUE ZERO.
       01  WS-QUOT                   PIC 9(04) VALUE ZERO.
       01  WS-REM                    PIC 9(04) VALUE ZERO.
       01  WS-LEAP-FLG               PIC X(01) VALUE 'N'.
           88  WS-LEAP                         VALUE 'Y'.

       LINKAGE SECTION.
       01  CC190S-PARM.
           05  LK-REQ-DATE           PIC X(08).
           05  LK-HOLIDAY-FLAG       PIC X(01).
           05  LK-NEXT-CAL-DATE      PIC X(08).
           05  LK-STATUS-CD          PIC X(02).
           05  LK-REASON             PIC X(60).

       PROCEDURE DIVISION USING CC190S-PARM.
       0000-MAIN.
           MOVE 8                     TO RETURN-CODE
           MOVE SPACE                 TO LK-HOLIDAY-FLAG
           MOVE SPACE                 TO LK-NEXT-CAL-DATE
           MOVE '00'                  TO LK-STATUS-CD
           MOVE SPACE                 TO LK-REASON
           MOVE 'N'                   TO WS-HARD-ERR-FLG

           PERFORM 1000-VALIDATE-PARM

           IF LK-STATUS-CD = '00'
               PERFORM 2000-CALC-NEXT-DATE
           END-IF

           IF LK-STATUS-CD = '00'
               PERFORM 3000-SEARCH-CALENDAR
           END-IF

           IF LK-STATUS-CD = '00'
               MOVE 0                 TO RETURN-CODE
           ELSE
               IF WS-HARD-ERR
                   MOVE 12            TO RETURN-CODE
               ELSE
                   MOVE 8             TO RETURN-CODE
               END-IF
           END-IF

           GOBACK.

       1000-VALIDATE-PARM.
           IF LK-REQ-DATE NOT NUMERIC
               MOVE '10'              TO LK-STATUS-CD
               MOVE 'REQ DATE NOT NUMERIC'
                                      TO LK-REASON
               DISPLAY 'CC190S BAD DATE=' LK-REQ-DATE
               EXIT PARAGRAPH
           END-IF

           MOVE LK-REQ-DATE           TO WS-REQ-DATE
           MOVE WS-REQ-DATE           TO WS-DATE-NUM

           IF WS-DATE-YYYY < 1900 OR WS-DATE-YYYY > 2099
               MOVE '11'              TO LK-STATUS-CD
               MOVE 'YEAR OUT OF RANGE'
                                      TO LK-REASON
               DISPLAY 'CC190S BAD YEAR=' WS-DATE-YYYY
               EXIT PARAGRAPH
           END-IF

           IF WS-DATE-MM < 1 OR WS-DATE-MM > 12
               MOVE '12'              TO LK-STATUS-CD
               MOVE 'MONTH INVALID'   TO LK-REASON
               DISPLAY 'CC190S BAD MONTH=' WS-DATE-MM
               EXIT PARAGRAPH
           END-IF

           PERFORM 1100-SET-MAX-DD

           IF WS-DATE-DD < 1 OR WS-DATE-DD > WS-MAX-DD
               MOVE '13'              TO LK-STATUS-CD
               MOVE 'DAY INVALID'     TO LK-REASON
               DISPLAY 'CC190S BAD DAY=' WS-DATE-DD
               EXIT PARAGRAPH
           END-IF.

       1100-SET-MAX-DD.
           MOVE 'N'                   TO WS-LEAP-FLG

           DIVIDE WS-DATE-YYYY BY 400
               GIVING WS-QUOT
               REMAINDER WS-REM
           END-DIVIDE

           IF WS-REM = ZERO
               MOVE 'Y'               TO WS-LEAP-FLG
           ELSE
               DIVIDE WS-DATE-YYYY BY 100
                   GIVING WS-QUOT
                   REMAINDER WS-REM
               END-DIVIDE
               IF WS-REM NOT = ZERO
                   DIVIDE WS-DATE-YYYY BY 4
                       GIVING WS-QUOT
                       REMAINDER WS-REM
                   END-DIVIDE
                   IF WS-REM = ZERO
                       MOVE 'Y'       TO WS-LEAP-FLG
                   END-IF
               END-IF
           END-IF

           EVALUATE WS-DATE-MM
               WHEN 1
               WHEN 3
               WHEN 5
               WHEN 7
               WHEN 8
               WHEN 10
               WHEN 12
                   MOVE 31            TO WS-MAX-DD
               WHEN 4
               WHEN 6
               WHEN 9
               WHEN 11
                   MOVE 30            TO WS-MAX-DD
               WHEN 2
                   IF WS-LEAP
                       MOVE 29        TO WS-MAX-DD
                   ELSE
                       MOVE 28        TO WS-MAX-DD
                   END-IF
               WHEN OTHER
                   MOVE ZERO          TO WS-MAX-DD
           END-EVALUATE.

       2000-CALC-NEXT-DATE.
           MOVE WS-REQ-DATE           TO WS-DATE-NUM
           PERFORM 1100-SET-MAX-DD

           IF WS-DATE-DD < WS-MAX-DD
               ADD 1                  TO WS-DATE-DD
           ELSE
               MOVE 1                 TO WS-DATE-DD
               IF WS-DATE-MM < 12
                   ADD 1              TO WS-DATE-MM
               ELSE
                   MOVE 1             TO WS-DATE-MM
                   ADD 1              TO WS-DATE-YYYY
               END-IF
           END-IF

           MOVE WS-DATE-NUM           TO WS-CALC-NEXT-DATE
           MOVE WS-CALC-NEXT-DATE     TO LK-NEXT-CAL-DATE.

       3000-SEARCH-CALENDAR.
           MOVE 'N'                   TO WS-EOF-FLG
           MOVE 'N'                   TO WS-FOUND-FLG
           MOVE 'N'                   TO WS-NEXT-FOUND-FLG

           OPEN INPUT CCCALF

           IF WS-CCCALF-ST NOT = '00'
               MOVE '90'              TO LK-STATUS-CD
               MOVE 'CALENDAR OPEN FAILED'
                                      TO LK-REASON
               MOVE 'Y'               TO WS-HARD-ERR-FLG
               DISPLAY 'CC190S OPEN ST=' WS-CCCALF-ST
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-EOF
               READ CCCALF
                   AT END
                       MOVE 'Y'       TO WS-EOF-FLG
                   NOT AT END
                       PERFORM 3100-CHECK-CALENDAR
               END-READ
           END-PERFORM

           CLOSE CCCALF
           IF WS-CCCALF-ST NOT = '00'
               MOVE '91'              TO LK-STATUS-CD
               MOVE 'CALENDAR CLOSE FAILED'
                                      TO LK-REASON
               MOVE 'Y'               TO WS-HARD-ERR-FLG
               DISPLAY 'CC190S CLOSE ST=' WS-CCCALF-ST
               EXIT PARAGRAPH
           END-IF

           IF LK-STATUS-CD NOT = '00'
               EXIT PARAGRAPH
           END-IF

           IF NOT WS-FOUND
               MOVE '20'              TO LK-STATUS-CD
               MOVE 'REQ DATE NOT IN CALENDAR'
                                      TO LK-REASON
               DISPLAY 'CC190S DATE NOT FOUND=' LK-REQ-DATE
               EXIT PARAGRAPH
           END-IF

           IF NOT WS-NEXT-FOUND
               MOVE '21'              TO LK-STATUS-CD
               MOVE 'NEXT DATE NOT IN CALENDAR'
                                      TO LK-REASON
               DISPLAY 'CC190S NEXT NOT FOUND=' LK-NEXT-CAL-DATE
           END-IF.

       3100-CHECK-CALENDAR.
           MOVE CL-CAL-DT             TO WS-CAL-DATE

           IF WS-CAL-DATE = WS-REQ-DATE
               IF CL-HOLIDAY-FLAG = 'Y'
                   MOVE CL-HOLIDAY-FLAG
                                      TO LK-HOLIDAY-FLAG
                   MOVE 'Y'           TO WS-FOUND-FLG
               ELSE
                   IF CL-HOLIDAY-FLAG = 'N'
                       MOVE CL-HOLIDAY-FLAG
                                      TO LK-HOLIDAY-FLAG
                       MOVE 'Y'       TO WS-FOUND-FLG
                   ELSE
                       MOVE '22'      TO LK-STATUS-CD
                       MOVE 'HOLIDAY FLAG INVALID'
                                      TO LK-REASON
                       MOVE 'Y'       TO WS-HARD-ERR-FLG
                       MOVE 'Y'       TO WS-EOF-FLG
                       DISPLAY 'CC190S BAD FLAG DATE='
                               WS-CAL-DATE
                               ' FLAG=' CL-HOLIDAY-FLAG
                   END-IF
               END-IF
           END-IF

           IF WS-CAL-DATE = WS-CALC-NEXT-DATE
               MOVE 'Y'               TO WS-NEXT-FOUND-FLG
           END-IF

           IF WS-FOUND AND WS-NEXT-FOUND
               MOVE 'Y'               TO WS-EOF-FLG
           END-IF.
